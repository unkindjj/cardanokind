{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}


import           Prelude (read)

import           Data.Semigroup ((<>))
import qualified Data.IP as IP
import qualified Data.Set as Set
import           Network.Socket (PortNumber)
import           Options.Applicative

import           Cardano.BM.Data.Tracer (TracingVerbosity (..))
import           Cardano.Node.Configuration.Lib (finaliseCardanoConfiguration)
import           Cardano.Node.Configuration.PartialTypes (PartialCardanoConfiguration (..))
import           Cardano.Node.Configuration.Presets (mainnetConfiguration)
import           Cardano.Node.Configuration.Types (CardanoConfiguration (..),
                                                   CardanoEnvironment (..))
import           Cardano.Node.Features.Logging (LoggingCLIArguments (..),
                                                LoggingLayer (..),
                                                createLoggingFeature
                                                )
import           Cardano.Prelude hiding (option)
import           Cardano.Shell.Lib (GeneralException (..),
                                    runCardanoApplicationWithFeatures)
import           Cardano.Shell.Types (CardanoApplication (..),
                                      CardanoFeature (..),
                                      CardanoFeatureInit (..))
import           Ouroboros.Consensus.BlockchainTime (SlotLength(..), slotLengthFromMillisec)
import qualified Ouroboros.Consensus.Ledger.Mock as Mock
import           Ouroboros.Consensus.NodeNetwork (ProtocolTracers'(..))
import           Ouroboros.Consensus.NodeId (NodeId (..))
import qualified Ouroboros.Consensus.Node.Tracers as Consensus

import           Cardano.Node.CLI
import           Cardano.Node.ConfigCLI (ConsensusTraceOptions, NodeCLIArguments(..), NodeCommand(..), ProtocolTraceOptions, TraceOptions(..))
import           Cardano.Node.Parsers (loggingParser, parseProtocol, parseViewMode)
import           Cardano.Node.Run
import           Cardano.Node.Topology (NodeAddress (..), TopologyInfo (..))


main :: IO ()
main = do

    let cardanoConfiguration = mainnetConfiguration
    let cardanoEnvironment   = NoEnvironment

    logConfig           <- execParser opts

    (cardanoFeatures, nodeLayer) <- initializeAllFeatures logConfig cardanoConfiguration cardanoEnvironment

    let cardanoApplication :: NodeLayer -> CardanoApplication
        cardanoApplication = CardanoApplication . nlRunNode

    runCardanoApplicationWithFeatures cardanoFeatures (cardanoApplication nodeLayer)

initializeAllFeatures :: CLIArguments -> PartialCardanoConfiguration -> CardanoEnvironment -> IO ([CardanoFeature], NodeLayer)
initializeAllFeatures (CLIArguments logCli nodeCli) partialConfig cardanoEnvironment = do
    finalConfig <- case finaliseCardanoConfiguration $
                          mergeConfiguration partialConfig (commonCLI nodeCli)
                   of
      Left err -> throwIO $ ConfigurationError err
      Right x  -> pure x

    (loggingLayer, loggingFeature) <- createLoggingFeature cardanoEnvironment finalConfig logCli
    (nodeLayer   , nodeFeature)    <- createNodeFeature loggingLayer nodeCli cardanoEnvironment finalConfig

    -- Here we return all the features.
    let allCardanoFeatures :: [CardanoFeature]
        allCardanoFeatures =
            [ loggingFeature
            , nodeFeature
            ]

    pure (allCardanoFeatures, nodeLayer)

-------------------------------------------------------------------------------
-- Layer
-------------------------------------------------------------------------------

data NodeLayer = NodeLayer
    { nlRunNode   :: forall m. MonadIO m => m ()
    }

-------------------------------------------------------------------------------
-- Node Feature
-------------------------------------------------------------------------------

type NodeCardanoFeature = CardanoFeatureInit CardanoEnvironment LoggingLayer CardanoConfiguration NodeCLIArguments NodeLayer


createNodeFeature :: LoggingLayer -> NodeCLIArguments -> CardanoEnvironment -> CardanoConfiguration -> IO (NodeLayer, CardanoFeature)
createNodeFeature loggingLayer nodeCLI cardanoEnvironment cardanoConfiguration = do
    -- we parse any additional configuration if there is any
    -- We don't know where the user wants to fetch the additional configuration from, it could be from
    -- the filesystem, so we give him the most flexible/powerful context, @IO@.

    -- we construct the layer
    nodeLayer <- (featureInit nodeCardanoFeatureInit) cardanoEnvironment loggingLayer cardanoConfiguration nodeCLI

    -- we construct the cardano feature
    let cardanoFeature = nodeCardanoFeature nodeCardanoFeatureInit nodeLayer

    -- we return both
    pure (nodeLayer, cardanoFeature)

nodeCardanoFeatureInit :: NodeCardanoFeature
nodeCardanoFeatureInit = CardanoFeatureInit
    { featureType    = "NodeFeature"
    , featureInit    = featureStart'
    , featureCleanup = featureCleanup'
    }
  where
    featureStart' :: CardanoEnvironment -> LoggingLayer -> CardanoConfiguration -> NodeCLIArguments -> IO NodeLayer
    featureStart' _ loggingLayer cc nodeCli = do
        pure $ NodeLayer {nlRunNode = liftIO $ runNode nodeCli loggingLayer cc}

    featureCleanup' :: NodeLayer -> IO ()
    featureCleanup' _ = pure ()


nodeCardanoFeature :: NodeCardanoFeature -> NodeLayer -> CardanoFeature
nodeCardanoFeature nodeCardanoFeature' nodeLayer = CardanoFeature
    { featureName       = featureType nodeCardanoFeature'
    , featureStart      = pure ()
    , featureShutdown   = liftIO $ (featureCleanup nodeCardanoFeature') nodeLayer
    }


-------------------------------------------------------------------------------
-- Parsers & Types
-------------------------------------------------------------------------------

-- | The product type of all command line arguments.
-- All here being - from all the features.
data CLIArguments = CLIArguments !LoggingCLIArguments !NodeCLIArguments

-- | The product parser for all the CLI arguments.
commandLineParser :: Parser CLIArguments
commandLineParser = CLIArguments
    <$> loggingParser
    <*> nodeParser

-- | Top level parser with info.
opts :: ParserInfo CLIArguments
opts = info (commandLineParser <**> helper)
    (  fullDesc
    <> progDesc "Cardano demo node."
    <> header "Demo node to run."
    )

nodeParser :: Parser NodeCLIArguments
nodeParser = NodeCLIArguments
    <$> parseSlotDuration
    <*> parseCommonCLI
    <*> parseNodeCommand

parseSlotDuration :: Parser SlotLength
parseSlotDuration = option (mkSlotLength <$> auto) $ mconcat [
      long "slot-duration"
    , value (mkSlotLength 5)
    , help "The slot duration (seconds)"
    ]
  where
    mkSlotLength :: Integer -> SlotLength
    mkSlotLength = slotLengthFromMillisec . (* 1000)

parseTraceBlockFetchClient :: Parser Bool
parseTraceBlockFetchClient  =
    switch (
         long "trace-block-fetch-client"
      <> help "Trace BlockFetch client."
    )

parseTraceBlockFetchServer :: Parser Bool
parseTraceBlockFetchServer  =
    switch (
         long "trace-block-fetch-server"
      <> help "Trace BlockFetch server."
    )

parseNodeCommand :: Parser NodeCommand
parseNodeCommand = subparser $ mconcat [
    command' "node" "Run a node." $
      SimpleNode
        <$> parseTopologyInfo
        <*> parseNodeAddress
        <*> parseProtocol
        <*> parseViewMode
        <*> parseTraceOptions
  , command' "submit" "Submit a transaction." $
      TxSubmitter <$> parseTopologyInfo <*> parseMockTx <*> parseProtocol
  , command' "trace-acceptor" "Spawn an acceptor." $
      pure TraceAcceptor
  ]

parseTopologyInfo :: Parser TopologyInfo
parseTopologyInfo = TopologyInfo <$> parseNodeId <*> parseTopologyFile

parseNodeId :: Parser NodeId
parseNodeId =
    option (fmap CoreId auto) (
            long "node-id"
         <> short 'n'
         <> metavar "NODE-ID"
         <> help "The ID for this node"
    )

parseMockTx :: Parser Mock.Tx
parseMockTx = mkTx
    <$> many parseMockTxIn
    <*> many parseMockTxOut
  where
    mkTx :: [Mock.TxIn] -> [Mock.TxOut] -> Mock.Tx
    mkTx ins = Mock.Tx (Set.fromList ins)

parseMockTxIn :: Parser Mock.TxIn
parseMockTxIn = (,)
    <$> strOption (mconcat [
            long "txin"
          , help "Hash of the input transaction. Single hex char."
          ])
    <*> option auto (mconcat [
            long "txix"
          , help "Index of the output in the specified transaction"
          ])

parseMockTxOut :: Parser Mock.TxOut
parseMockTxOut = (,)
    <$> strOption (mconcat [
            long "address"
          , help "Address to transfer to"
          ])
    <*> option auto (mconcat [
            long "amount"
          , help "Amount to transfer"
          ])

parseTopologyFile :: Parser FilePath
parseTopologyFile =
    strOption (
            long "topology"
         <> short 't'
         <> metavar "FILEPATH"
         <> help "The path to a file describing the topology."
    )

parseNodeAddress :: Parser NodeAddress
parseNodeAddress = NodeAddress <$> parseHostAddr <*> parsePort

parseHostAddr :: Parser IP.IP
parseHostAddr =
    option (read <$> str) (
          long "host-addr"
       <> metavar "HOST-NAME"
       <> help "The ipv6 or ipv4 address"
    )

parsePort :: Parser PortNumber
parsePort =
    option ((fromIntegral :: Int -> PortNumber) <$> auto) (
          long "port"
       <> metavar "PORT"
       <> help "The port number"
    )

parseTraceOptions :: Parser TraceOptions
parseTraceOptions = TraceOptions
  <$> parseTracingVerbosity
  <*> parseTraceChainDB
  <*> parseConsensusTraceOptions
  <*> parseProtocolTraceOptions
  <*> parseTraceIpSubscription
  <*> parseTraceDnsSubscription
  <*> parseTraceDnsResolver

parseTracingVerbosity :: Parser TracingVerbosity
parseTracingVerbosity = asum [
    flag' MinimalVerbosity (long "tracing-verbosity-minimal"
            <> help "Minimal level of the rendering of captured items")
    <|>
    flag' MaximalVerbosity (long "tracing-verbosity-maximal"
            <> help "Maximal level of the rendering of captured items")
    <|>
    flag NormalVerbosity NormalVerbosity (long "tracing-verbosity-normal"
            <> help "the default level of the rendering of captured items")
    ]

parseTraceChainDB :: Parser Bool
parseTraceChainDB =
    switch (
         long "trace-chain-db"
      <> help "Verbose tracer of ChainDB."
    )

parseConsensusTraceOptions :: Parser ConsensusTraceOptions
parseConsensusTraceOptions = Consensus.Tracers
  <$> (Const <$> parseTraceChainSyncClient)
  <*> (Const <$> parseTraceChainSyncServer)
  <*> (Const <$> parseTraceBlockFetchDecisions)
  <*> (Const <$> parseTraceBlockFetchClient)
  <*> (Const <$> parseTraceBlockFetchServer)
  <*> (Const <$> parseTraceTxInbound)
  <*> (Const <$> parseTraceTxOutbound)
  <*> (Const <$> parseTraceLocalTxSubmissionServer)
  <*> (Const <$> parseTraceMempool)
  <*> (Const <$> parseTraceForge)

parseTraceBlockFetchDecisions :: Parser Bool
parseTraceBlockFetchDecisions =
    switch (
         long "trace-block-fetch-decisions"
      <> help "Trace BlockFetch decisions made by the BlockFetch client."
    )

parseTraceChainSyncClient :: Parser Bool
parseTraceChainSyncClient  =
    switch (
         long "trace-chain-sync-client"
      <> help "Trace ChainSync client."
    )

parseTraceChainSyncServer :: Parser Bool
parseTraceChainSyncServer  =
    switch (
         long "trace-chain-sync-server"
      <> help "Trace ChainSync server."
    )

parseTraceTxInbound :: Parser Bool
parseTraceTxInbound =
    switch (
         long "trace-tx-inbound"
      <> help "Trace TxSubmission server (inbound transactions)."
    )

parseTraceTxOutbound :: Parser Bool
parseTraceTxOutbound =
    switch (
         long "trace-tx-outbound"
      <> help "Trace TxSubmission client (outbound transactions)."
    )

parseTraceLocalTxSubmissionServer :: Parser Bool
parseTraceLocalTxSubmissionServer =
    switch (
         long "trace-local-tx-submission-server"
      <> help "Trace local TxSubmission server."
    )

parseTraceMempool :: Parser Bool
parseTraceMempool =
    switch (
         long "trace-mempool"
      <> help "Trace mempool."
    )

parseTraceForge :: Parser Bool
parseTraceForge =
    switch (
         long "trace-forge"
      <> help "Trace block forging."
    )

parseTraceChainSyncProtocol :: Parser Bool
parseTraceChainSyncProtocol =
    switch (
         long "trace-chain-sync-protocol"
      <> help "Trace ChainSync protocol messages."
    )

parseTraceBlockFetchProtocol :: Parser Bool
parseTraceBlockFetchProtocol =
    switch (
         long "trace-block-fetch-protocol"
      <> help "Trace BlockFetch protocol messages."
    )

parseTraceTxSubmissionProtocol :: Parser Bool
parseTraceTxSubmissionProtocol =
    switch (
         long "trace-tx-submission-protocol"
      <> help "Trace TxSubmission protocol messages."
    )

parseTraceLocalChainSyncProtocol :: Parser Bool
parseTraceLocalChainSyncProtocol =
    switch (
         long "trace-local-chain-sync-protocol"
      <> help "Trace local ChainSync protocol messages."
    )

parseTraceLocalTxSubmissionProtocol :: Parser Bool
parseTraceLocalTxSubmissionProtocol =
    switch (
         long "trace-local-tx-submission-protocol"
      <> help "Trace local TxSubmission protocol messages."
    )


parseProtocolTraceOptions :: Parser ProtocolTraceOptions
parseProtocolTraceOptions = ProtocolTracers
  <$> (Const <$> parseTraceChainSyncProtocol)
  <*> (Const <$> parseTraceBlockFetchProtocol)
  <*> (Const <$> parseTraceTxSubmissionProtocol)
  <*> (Const <$> parseTraceLocalChainSyncProtocol)
  <*> (Const <$> parseTraceLocalTxSubmissionProtocol)

parseTraceIpSubscription :: Parser Bool
parseTraceIpSubscription =
    switch (
         long "trace-ip-subscription"
      <> help "Trace IP Subscription messages."
    )

parseTraceDnsSubscription :: Parser Bool
parseTraceDnsSubscription =
    switch (
         long "trace-dns-subscription"
      <> help "Trace DNS Subscription messages."
    )

parseTraceDnsResolver :: Parser Bool
parseTraceDnsResolver =
    switch (
         long "trace-dns-resolver"
      <> help "Trace DNS Resolver messages."
    )