{-# LANGUAGE PatternSynonyms #-}
module Cardano.Api.Shelley
  ( module Cardano.Api.LocalChainSync
  , module Cardano.Api.MetaData
  , module Cardano.Api.Protocol
  , module Cardano.Api.Shelley.Genesis
  , module Cardano.Api.Shelley.ITN
  , module Cardano.Api.TextView
  , module Cardano.Api.TxSubmit
  , module Cardano.Api.Typed
  , module Cardano.Api.Protocol.Cardano
  , module Cardano.Api.Protocol.Shelley
  , module Cardano.Api.Protocol.Types
  , module Cardano.Slotting.Slot
  , module Network.Mux
  , module Ouroboros.Consensus.Block
  , module Ouroboros.Consensus.BlockchainTime
  , module Ouroboros.Consensus.Cardano
  , module Ouroboros.Consensus.Cardano.Block
  , module Ouroboros.Consensus.Cardano.CanHardFork
  , module Ouroboros.Consensus.Cardano.ShelleyHFC
  , module Ouroboros.Consensus.Config.SupportsNode
  , module Ouroboros.Consensus.Fragment.InFuture
  , module Ouroboros.Consensus.HardFork.Combinator
  , module Ouroboros.Consensus.HardFork.Combinator.AcrossEras
  , module Ouroboros.Consensus.HardFork.Combinator.Degenerate
  , module Ouroboros.Consensus.HardFork.History.EraParams
  , module Ouroboros.Consensus.HeaderValidation
  , module Ouroboros.Consensus.Ledger.Abstract
  , module Ouroboros.Consensus.Ledger.Basics
  , module Ouroboros.Consensus.Ledger.Inspect
  , module Ouroboros.Consensus.Ledger.Extended
  , module Ouroboros.Consensus.Ledger.SupportsMempool
  , module Ouroboros.Consensus.Ledger.SupportsProtocol
  , module Ouroboros.Consensus.Mempool.API
  , module Ouroboros.Consensus.Network.NodeToClient
  , module Ouroboros.Consensus.Network.NodeToNode
  , module Ouroboros.Consensus.Node
  , module Ouroboros.Consensus.Node.NetworkProtocolVersion
  , module Ouroboros.Consensus.Node.ProtocolInfo
  , module Ouroboros.Consensus.Node.Run
  , module Ouroboros.Consensus.Node.Tracers
  , module Ouroboros.Consensus.Protocol.BFT
  , module Ouroboros.Consensus.Shelley.Ledger
  , module Ouroboros.Consensus.Shelley.Ledger.Ledger
  , module Ouroboros.Consensus.Shelley.Ledger.Mempool
  , module Ouroboros.Consensus.Shelley.Ledger.Block
  , module Ouroboros.Consensus.Shelley.Node
  , module Ouroboros.Consensus.Shelley.Protocol
  , module Ouroboros.Consensus.Shelley.Protocol.Crypto
  , module Ouroboros.Consensus.Shelley.Protocol.HotKey
  , module Ouroboros.Consensus.Storage.ChainDB
  , module Ouroboros.Consensus.Storage.ImmutableDB
  , module Ouroboros.Consensus.Storage.ImmutableDB.API
  , module Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal
  , module Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types
  , module Ouroboros.Consensus.Storage.VolatileDB
  , module Ouroboros.Consensus.TypeFamilyWrappers
  , module Ouroboros.Consensus.Util.Condense
  , module Ouroboros.Consensus.Util.OptNP
  , module Ouroboros.Consensus.Util.ResourceRegistry
  , module Ouroboros.Consensus.Util.STM
  , module Ouroboros.Network.AnchoredFragment
  , module Ouroboros.Network.Block
  , module Ouroboros.Network.BlockFetch
  , module Ouroboros.Network.BlockFetch.ClientRegistry
  , module Ouroboros.Network.BlockFetch.ClientState
  , module Ouroboros.Network.BlockFetch.Decision
  , module Ouroboros.Network.Codec
  , module Ouroboros.Network.KeepAlive
  , module Ouroboros.Network.Magic
  , module Ouroboros.Network.Mux
  , module Ouroboros.Network.NodeToClient
  , module Ouroboros.Network.NodeToNode
  , module Ouroboros.Network.Point
  , module Ouroboros.Network.Protocol.ChainSync.Client
  , module Ouroboros.Network.Protocol.ChainSync.Type
  , module Ouroboros.Network.Protocol.LocalStateQuery.Type
  , module Ouroboros.Network.Protocol.LocalTxSubmission.Type
  , module Ouroboros.Network.Protocol.TxSubmission.Type
  , module Ouroboros.Network.Subscription
  , module Ouroboros.Network.TxSubmission.Inbound
  , module Ouroboros.Network.TxSubmission.Outbound
  , module Shelley.Spec.Ledger.API
  , module Shelley.Spec.Ledger.API.Validation
  , module Shelley.Spec.Ledger.BaseTypes
  , module Shelley.Spec.Ledger.BlockChain
  , module Shelley.Spec.Ledger.Credential
  , module Shelley.Spec.Ledger.Delegation.Certificates
  , module Shelley.Spec.Ledger.EpochBoundary
  , module Shelley.Spec.Ledger.Genesis
  , module Shelley.Spec.Ledger.Keys
  , module Shelley.Spec.Ledger.LedgerState
  , module Shelley.Spec.Ledger.MetaData
  , module Shelley.Spec.Ledger.OCert
  , module Shelley.Spec.Ledger.PParams
  , module Shelley.Spec.Ledger.Rewards
  , module Shelley.Spec.Ledger.Address
  , module Shelley.Spec.Ledger.TxBody
  , module Shelley.Spec.Ledger.UTxO

  , module Shelley.Spec.Ledger.STS.Bbody
  , module Shelley.Spec.Ledger.STS.Chain
  , module Shelley.Spec.Ledger.STS.Deleg
  , module Shelley.Spec.Ledger.STS.Delegs
  , module Shelley.Spec.Ledger.STS.Delpl
  , module Shelley.Spec.Ledger.STS.Epoch
  , module Shelley.Spec.Ledger.STS.Ledger
  , module Shelley.Spec.Ledger.STS.Ledgers
  , module Shelley.Spec.Ledger.STS.Mir
  , module Shelley.Spec.Ledger.STS.NewEpoch
  , module Shelley.Spec.Ledger.STS.Newpp
  , module Shelley.Spec.Ledger.STS.Ocert
  , module Shelley.Spec.Ledger.STS.Overlay
  , module Shelley.Spec.Ledger.STS.Pool
  , module Shelley.Spec.Ledger.STS.PoolReap
  , module Shelley.Spec.Ledger.STS.Ppup
  , module Shelley.Spec.Ledger.STS.Prtcl
  , module Shelley.Spec.Ledger.STS.Rupd
  , module Shelley.Spec.Ledger.STS.Snap
  , module Shelley.Spec.Ledger.STS.Tick
  , module Shelley.Spec.Ledger.STS.Tickn
  , module Shelley.Spec.Ledger.STS.Updn
  , module Shelley.Spec.Ledger.STS.Utxo
  , module Shelley.Spec.Ledger.STS.Utxow
  ) where

-- | This module provides a library interface that is intended to be the complete API
-- for Shelley covering everything, including exposing constructors for the lower level types.
--

import           Cardano.Api.LocalChainSync (getLocalTip)
import           Cardano.Api.MetaData (TxMetadata (..), TxMetadataJsonError (..),
                     TxMetadataJsonSchema (TxMetadataJsonDetailedSchema, TxMetadataJsonNoSchema),
                     TxMetadataRangeError (..), metadataFromJson, metadataToJson,
                     validateTxMetadata)
import           Cardano.Api.Protocol (Protocol (ByronProtocol, CardanoProtocol, ShelleyProtocol),
                     withlocalNodeConnectInfo)
import           Cardano.Api.Protocol.Cardano (mkSomeNodeClientProtocolCardano)
import           Cardano.Api.Protocol.Shelley (mkSomeNodeClientProtocolShelley)
import           Cardano.Api.Protocol.Types (SomeNodeClientProtocol (..))
import           Cardano.Api.Shelley.Genesis (shelleyGenesisDefaults)
import           Cardano.Api.Shelley.ITN (xprvFromBytes)
import           Cardano.Api.TextView (TextView (..), TextViewDescription (..), TextViewError (..),
                     TextViewType (..), textShow)
import           Cardano.Api.TxSubmit (TxForMode (..), TxSubmitResultForMode (..), submitTx)
import           Cardano.Api.Typed
import           Cardano.Slotting.Slot (EpochNo (..), SlotNo (SlotNo))
import           Network.Mux (MuxError, MuxMode (InitiatorMode))
import           Ouroboros.Consensus.Block (BlockProtocol, CannotForge, CodecConfig,
                     ConvertRawHash (fromShortRawHash, hashSize, toRawHash, toShortRawHash),
                     ForgeStateInfo, ForgeStateUpdateError, GetHeader (..), Header,
                     RealPoint (RealPoint), blockNo, headerPoint, realPointHash, realPointSlot)
import           Ouroboros.Consensus.BlockchainTime (SlotLength, SystemStart (SystemStart),
                     TraceBlockchainTimeEvent (TraceCurrentSlotUnknown, TraceStartTimeInTheFuture),
                     getSlotLength, slotLengthFromSec)
import           Ouroboros.Consensus.Cardano (ProtocolCardano, ProtocolClient (..), ProtocolShelley,
                     SecurityParam (..), protocolClientInfo, protocolInfo)
import           Ouroboros.Consensus.Cardano.Block (CardanoBlock,
                     Either (QueryResultEraMismatch, QueryResultSuccess),
                     EraMismatch (EraMismatch, ledgerEraName, otherEraName),
                     HardForkApplyTxErr (ApplyTxErrByron, ApplyTxErrShelley, ApplyTxErrWrongEra),
                     Query (QueryIfCurrentShelley))
                   --  LedgerState (LedgerStateByron, LedgerStateShelley),

import           Ouroboros.Consensus.Cardano.ShelleyHFC (ShelleyBlockHFC)
import           Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode (..))
import           Ouroboros.Consensus.Fragment.InFuture (defaultClockSkew)
import           Ouroboros.Consensus.HardFork.Combinator (HardForkApplyTxErr (..), HardForkBlock,
                     HardForkEnvelopeErr (HardForkEnvelopeErrFromEra, HardForkEnvelopeErrWrongEra),
                     HardForkLedgerError (HardForkLedgerErrorFromEra, HardForkLedgerErrorWrongEra),
                     HardForkLedgerUpdate (HardForkUpdateInEra, HardForkUpdateTransitionConfirmed, HardForkUpdateTransitionDone, HardForkUpdateTransitionRolledBack),
                     HardForkLedgerWarning (HardForkWarningInEra, HardForkWarningTransitionInFinalEra, HardForkWarningTransitionMismatch, HardForkWarningTransitionReconfirmed, HardForkWarningTransitionUnconfirmed),
                     HardForkValidationErr (..), OneEraHash, SingleEraBlock, getHardForkGenTx,
                     getHardForkGenTxId, getHardForkHeader, getOneEraApplyTxErr, getOneEraGenTx,
                     getOneEraHash, getOneEraHeader)
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch (..),
                     OneEraApplyTxErr, OneEraCannotForge (..), OneEraEnvelopeErr (..),
                     OneEraGenTxId (getOneEraGenTxId), OneEraLedgerError (..),
                     OneEraLedgerUpdate (..), OneEraLedgerWarning (..), OneEraValidationErr (..),
                     PerEraForgeStateInfo (..), PerEraForgeStateUpdateError (..), mkEraMismatch)
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate (Either (DegenQueryResult),
                     Query (DegenQuery))
import           Ouroboros.Consensus.HardFork.History.EraParams (EraParams (..), SafeBeforeEpoch,
                     SafeZone)
import           Ouroboros.Consensus.HeaderValidation (HeaderEnvelopeError (OtherHeaderEnvelopeError, UnexpectedBlockNo, UnexpectedPrevHash, UnexpectedSlotNo),
                     HeaderError (HeaderEnvelopeError, HeaderProtocolError),
                     OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerErr, LedgerError)
import           Ouroboros.Consensus.Ledger.Basics (LedgerState)
import           Ouroboros.Consensus.Ledger.Inspect (InspectLedger,
                     LedgerEvent (LedgerUpdate, LedgerWarning), LedgerUpdate (LedgerUpdate),
                     LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx, GenTxId,
                     HasTxId (txId), HasTxs (extractTxs))
import           Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Mempool.API (MempoolSize (MempoolSize, msNumBytes, msNumTxs), TraceEventMempool (TraceMempoolAddedTx, TraceMempoolManuallyRemovedTxs, TraceMempoolRejectedTx, TraceMempoolRemoveTxs))
import           Ouroboros.Consensus.Network.NodeToClient
                     (Codecs' (Codecs, cChainSyncCodec, cStateQueryCodec, cTxSubmissionCodec),
                     clientCodecs)
import           Ouroboros.Consensus.Node (DiffusionArguments (DiffusionArguments, daAcceptedConnectionsLimit, daAddresses, daDnsProducers, daIpProducers, daLocalAddress),
                     DiffusionTracers (DiffusionTracers, dtAcceptPolicyTracer, dtDnsResolverTracer, dtDnsSubscriptionTracer, dtErrorPolicyTracer, dtHandshakeLocalTracer, dtHandshakeTracer, dtIpSubscriptionTracer, dtLocalErrorPolicyTracer, dtMuxLocalTracer, dtMuxTracer),
                     DnsSubscriptionTarget (DnsSubscriptionTarget, dstDomain, dstPort, dstValency),
                     IPSubscriptionTarget (IPSubscriptionTarget, ispIps, ispValency),
                     NodeArgs (NodeArgs, blockFetchConfiguration),
                     NodeKernel (NodeKernel, getFetchClientRegistry, getNodeCandidates),
                     RunNode (..),
                     RunNodeArgs (RunNodeArgs, rnCustomiseChainDbArgs, rnCustomiseNodeArgs, rnDatabasePath, rnDiffusionArguments, rnMaxClockSkew, rnNetworkMagic, rnNodeKernelHook, rnNodeToClientVersions, rnNodeToNodeVersions, rnProtocolInfo, rnTraceConsensus, rnTraceDB, rnTraceDiffusion, rnTraceNTC, rnTraceNTN),
                     getChainDB, remoteAddress, run)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion (HasNetworkProtocolVersion (..),
                     supportedNodeToClientVersions, supportedNodeToNodeVersions)
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (ProtocolInfo, pInfoConfig),
                     pClientInfoCodecConfig)
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Node.Tracers (TraceForgeEvent (..))
import           Ouroboros.Consensus.Protocol.BFT (BftValidationErr (BftInvalidSignature))
import           Ouroboros.Consensus.Shelley.Ledger (Era, Query (GetCBOR, GetCurrentEpochState, GetCurrentPParams, GetFilteredDelegationsAndRewardAccounts, GetFilteredUTxO, GetStakeDistribution, GetUTxO),
                     ShelleyBlock, shelleyLedgerState)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock (ShelleyBlock),
                     ShelleyHash (..))
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (ApplyTxError (ApplyTxError))
import           Ouroboros.Consensus.Shelley.Node (MaxMajorProtVer (MaxMajorProtVer), Nonce (Nonce),
                     ShelleyGenesis,
                     TPraosLeaderCredentials (TPraosLeaderCredentials, tpraosLeaderCredentialsCanBeLeader, tpraosLeaderCredentialsInitSignKey))
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto, TPraosCanBeLeader (TPraosCanBeLeader, tpraosCanBeLeaderColdVerKey, tpraosCanBeLeaderOpCert, tpraosCanBeLeaderSignKeyVRF),
                     TPraosCannotForge (TPraosCannotForgeKeyNotUsableYet, TPraosCannotForgeWrongVRF))
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardShelley)
import           Ouroboros.Consensus.Shelley.Protocol.HotKey
                     (KESEvolutionError (KESCouldNotEvolve, KESKeyAlreadyPoisoned),
                     KESInfo (KESInfo, kesEndPeriod, kesEvolution, kesStartPeriod))
import           Ouroboros.Consensus.Storage.ChainDB (ChainDB,
                     ChainDbArgs (cdbImmutableDbValidation, cdbVolatileDbValidation))
import           Ouroboros.Consensus.Storage.ImmutableDB.API (tipBlockNo, tipHash)
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks.Internal (ChunkNo (unChunkNo))
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types (BlockOrEBB (Block, EBB))
import           Ouroboros.Consensus.TypeFamilyWrappers
                     (WrapApplyTxErr (WrapApplyTxErr, unwrapApplyTxErr),
                     WrapCannotForge (WrapCannotForge, unwrapCannotForge),
                     WrapEnvelopeErr (WrapEnvelopeErr, unwrapEnvelopeErr),
                     WrapForgeStateInfo (WrapForgeStateInfo, unwrapForgeStateInfo),
                     WrapForgeStateInfo (unwrapForgeStateInfo),
                     WrapForgeStateUpdateError (WrapForgeStateUpdateError, unwrapForgeStateUpdateError),
                     WrapForgeStateUpdateError (WrapForgeStateUpdateError, unwrapForgeStateUpdateError),
                     WrapGenTxId (WrapGenTxId, unwrapGenTxId),
                     WrapHeaderHash (WrapHeaderHash, unwrapHeaderHash),
                     WrapLedgerConfig (WrapLedgerConfig, unwrapLedgerConfig),
                     WrapLedgerErr (WrapLedgerErr, unwrapLedgerErr),
                     WrapLedgerUpdate (WrapLedgerUpdate, unwrapLedgerUpdate),
                     WrapLedgerWarning (WrapLedgerWarning, unwrapLedgerWarning),
                     WrapTipInfo (WrapTipInfo, unwrapTipInfo),
                     WrapValidationErr (WrapValidationErr, unwrapValidationErr))
import           Ouroboros.Consensus.Util.OptNP (toNP)
import           Ouroboros.Network.BlockFetch.ClientRegistry (readFetchClientsStateVars)
import           Ouroboros.Network.BlockFetch.ClientState (PeerFetchInFlight, PeerFetchStatus (PeerFetchStatusAberrant, PeerFetchStatusBusy, PeerFetchStatusReady, PeerFetchStatusShutdown),
                     TraceFetchClientState (AcknowledgedFetchRequest, AddedFetchRequest, CompletedBlockFetch, CompletedFetchBatch, RejectedFetchBatch, StartedFetchBatch),
                     TraceLabelPeer (..), peerFetchBlocksInFlight, peerFetchBytesInFlight,
                     peerFetchMaxSlotNo, peerFetchReqsInFlight, readFetchClientState)
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision, FetchDecline (..))
import           Ouroboros.Network.Codec (AnyMessage (AnyMessage))
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient)
import           Ouroboros.Network.Subscription (ConnectResult (..), DnsTrace (..),
                     SubscriberError (..), SubscriptionTrace (..), WithDomainName (..),
                     WithIPList (..))
import           Ouroboros.Network.TxSubmission.Inbound (TraceTxSubmissionInbound (..))
import           Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound (..))
-- import           Ouroboros.Consensus.Storage.ChainDB.API (getTipPoint) Conflict
import           Ouroboros.Consensus.Cardano.CanHardFork (TriggerHardFork (TriggerHardForkAtEpoch, TriggerHardForkAtVersion, TriggerHardForkNever))
import           Ouroboros.Consensus.Ledger.Extended (ledgerState)
import           Ouroboros.Consensus.Network.NodeToNode (Tracers, Tracers' (Tracers, tBlockFetchSerialisedTracer, tBlockFetchSerialisedTracer, tBlockFetchTracer, tChainSyncSerialisedTracer, tChainSyncTracer, tTxSubmissionTracer, tTxSubmissionTracer),
                     nullTracers)
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
                     (ShelleyLedgerError (BBodyError, TickError))
import           Ouroboros.Consensus.Storage.ImmutableDB (ValidationPolicy (ValidateAllChunks))
import           Ouroboros.Consensus.Storage.VolatileDB (BlockValidationPolicy (ValidateAll))
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry)
import           Ouroboros.Consensus.Util.STM (onEachChange)
import           Ouroboros.Network.AnchoredFragment (Anchor (AnchorGenesis),
                     AnchoredFragment (Empty), addBlock, anchor, anchorToBlockNo, anchorToTip,
                     headAnchor, headBlockNo, headPoint, headSlot, intersect, last, lastSlot,
                     rollback, toOldestFirst)
import           Ouroboros.Network.Block (BlockNo (..), pattern BlockPoint, ChainHash,
                     ChainUpdate (AddBlock, RollBack), pattern GenesisPoint, HasHeader, HeaderHash,
                     MaxSlotNo (MaxSlotNo, NoMaxSlotNo), Point, Serialised (..), StandardHash,
                     Tip (Tip, TipGenesis), blockHash, blockSlot, genesisPoint, getTipBlockNo,
                     getTipPoint, pointSlot)
import           Ouroboros.Network.BlockFetch (BlockFetchConfiguration (bfcMaxConcurrencyBulkSync, bfcMaxConcurrencyDeadline))
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.Mux (MuxPeer (..), OuroborosApplication (..),
                     RunMiniProtocol (InitiatorProtocolOnly))
import           Ouroboros.Network.NodeToClient (DictVersion, IOManager, LocalAddress,
                     LocalConnectionId, NetworkConnectTracers (nctHandshakeTracer, nctMuxTracer),
                     NetworkConnectTracers (NetworkConnectTracers),
                     NodeToClientProtocols (NodeToClientProtocols, localChainSyncProtocol, localStateQueryProtocol, localTxSubmissionProtocol),
                     NodeToClientVersion, NodeToClientVersionData (NodeToClientVersionData),
                     TraceSendRecv, Versions, connectTo, foldMapVersions, localSnocket,
                     localStateQueryPeerNull, localTxSubmissionPeerNull,
                     versionedNodeToClientProtocols, withIOManager)
import           Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit (..), ErrorPolicyTrace (..),
                     RemoteConnectionId, TraceSendRecv (..), WithAddr (..))
import           Ouroboros.Network.Point (WithOrigin (..), fromWithOrigin, withOrigin,
                     withOriginToMaybe)
import           Ouroboros.Network.Protocol.ChainSync.Client (ChainSyncClient (ChainSyncClient),
                     ClientStIdle (SendMsgFindIntersect, SendMsgRequestNext),
                     ClientStIntersect (ClientStIntersect, recvMsgIntersectFound, recvMsgIntersectNotFound),
                     ClientStNext (ClientStNext, recvMsgRollBackward, recvMsgRollForward),
                     chainSyncClientPeer)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync, Message (MsgAwaitReply, MsgFindIntersect, MsgIntersectFound, MsgIntersectNotFound, MsgRequestNext, MsgRollBackward, MsgRollForward))
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..),
                     LocalStateQuery)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (LocalTxSubmission)
import           Ouroboros.Network.Protocol.TxSubmission.Type
                     (Message (MsgDone, MsgReplyTxIds, MsgReplyTxs, MsgRequestTxIds, MsgRequestTxs),
                     TxSubmission)
import           Shelley.Spec.Ledger.Address (Addr (Addr, AddrBootstrap),
                     BootstrapAddress (BootstrapAddress))
import           Shelley.Spec.Ledger.API (ChainTransitionError (ChainTransitionError))
import           Shelley.Spec.Ledger.API.Validation (BlockTransitionError (BlockTransitionError),
                     TickTransitionError (TickTransitionError))
import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..), fromSMaybe)
import           Shelley.Spec.Ledger.BlockChain (HashHeader (..), LastAppliedBlock (labBlockNo))
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Credential (Credential, Ptr, StakeReference)
import           Shelley.Spec.Ledger.Delegation.Certificates
                     (IndividualPoolStake (IndividualPoolStake), PoolDistr (PoolDistr))
import           Shelley.Spec.Ledger.EpochBoundary (SnapShot, SnapShots, Stake (Stake))
import           Shelley.Spec.Ledger.Genesis (ShelleyGenesis (..), ValidationErr,
                     describeValidationErr, validateGenesis)
import           Shelley.Spec.Ledger.Keys (GenDelegs, KeyHash, KeyRole (StakePool, Staking),
                     coerceKeyRole)
import           Shelley.Spec.Ledger.LedgerState (AccountState, DPState, DState, EpochState,
                     FutureGenDeleg, InstantaneousRewards, PPUPState, PState, RewardAccounts,
                     UTxOState (_deposited, _fees, _ppups, _utxo), WitHashes (WitHashes), esLState,
                     nesEs, _utxoState)
import           Shelley.Spec.Ledger.MetaData (MetaDataHash (..))
import           Shelley.Spec.Ledger.OCert (KESPeriod (..))
import           Shelley.Spec.Ledger.PParams (PParams, PParams' (..),
                     ProposedPPUpdates (ProposedPPUpdates), ProtVer (ProtVer))
import           Shelley.Spec.Ledger.Rewards (Likelihood (Likelihood), LogWeight (LogWeight),
                     NonMyopic)
import           Shelley.Spec.Ledger.TxBody (MIRPot (ReservesMIR, TreasuryMIR))
import           Shelley.Spec.Ledger.UTxO (UTxO (UTxO))
--, TxId (TxId), TxIn (TxIn), TxOut (TxOut))

import           Shelley.Spec.Ledger.STS.Bbody (BbodyPredicateFailure (InvalidBodyHashBBODY, LedgersFailure, WrongBlockBodySizeBBODY))
import           Shelley.Spec.Ledger.STS.Chain (ChainPredicateFailure (BbodyFailure, BlockSizeTooLargeCHAIN, HeaderSizeTooLargeCHAIN, ObsoleteNodeCHAIN, PrtclFailure, PrtclSeqFailure, TickFailure, TicknFailure))
import           Shelley.Spec.Ledger.STS.Deleg (DelegPredicateFailure (DuplicateGenesisDelegateDELEG, DuplicateGenesisVRFDELEG, GenesisKeyNotInpMappingDELEG, InsufficientForInstantaneousRewardsDELEG, MIRCertificateTooLateinEpochDELEG, StakeDelegationImpossibleDELEG, StakeKeyAlreadyRegisteredDELEG, StakeKeyInRewardsDELEG, StakeKeyNonZeroAccountBalanceDELEG, StakeKeyNotRegisteredDELEG, WrongCertificateTypeDELEG))
import           Shelley.Spec.Ledger.STS.Delegs (DelegsPredicateFailure (DelegateeNotRegisteredDELEG, DelplFailure, WithdrawalsNotInRewardsDELEGS))
import           Shelley.Spec.Ledger.STS.Delpl (DelplPredicateFailure (DelegFailure, PoolFailure))
import           Shelley.Spec.Ledger.STS.Epoch
                     (EpochPredicateFailure (NewPpFailure, PoolReapFailure, SnapFailure))
import           Shelley.Spec.Ledger.STS.Ledger
                     (LedgerPredicateFailure (DelegsFailure, UtxowFailure))
import           Shelley.Spec.Ledger.STS.Ledgers (LedgersPredicateFailure (LedgerFailure))
import           Shelley.Spec.Ledger.STS.Mir (MirPredicateFailure)
import           Shelley.Spec.Ledger.STS.NewEpoch
                     (NewEpochPredicateFailure (CorruptRewardUpdate, EpochFailure, MirFailure))
import           Shelley.Spec.Ledger.STS.Newpp (NewppPredicateFailure (UnexpectedDepositPot))
import           Shelley.Spec.Ledger.STS.Ocert (OcertPredicateFailure (CounterTooSmallOCERT, InvalidKesSignatureOCERT, InvalidSignatureOCERT, KESAfterEndOCERT, KESBeforeStartOCERT, NoCounterForKeyHashOCERT))
import           Shelley.Spec.Ledger.STS.Overlay (OverlayPredicateFailure (NotActiveSlotOVERLAY, OcertFailure, UnknownGenesisKeyOVERLAY, VRFKeyBadLeaderValue, VRFKeyBadNonce, VRFKeyUnknown, VRFKeyWrongVRFKey, VRFLeaderValueTooBig, WrongGenesisColdKeyOVERLAY, WrongGenesisVRFKeyOVERLAY))
import           Shelley.Spec.Ledger.STS.Pool (PoolPredicateFailure (StakePoolCostTooLowPOOL, StakePoolNotRegisteredOnKeyPOOL, StakePoolRetirementWrongEpochPOOL, WrongCertificateTypePOOL))
import           Shelley.Spec.Ledger.STS.PoolReap (PoolreapPredicateFailure)
import           Shelley.Spec.Ledger.STS.Ppup (PpupPredicateFailure (NonGenesisUpdatePPUP, PPUpdateWrongEpoch, PVCannotFollowPPUP))

import           Shelley.Spec.Ledger.STS.Prtcl (PrtclPredicateFailure (OverlayFailure, UpdnFailure), PrtlSeqFailure (WrongBlockNoPrtclSeq, WrongBlockSequencePrtclSeq, WrongSlotIntervalPrtclSeq))
import           Shelley.Spec.Ledger.STS.Rupd (RupdPredicateFailure)
import           Shelley.Spec.Ledger.STS.Snap (SnapPredicateFailure)
import           Shelley.Spec.Ledger.STS.Tick (TickPredicateFailure (NewEpochFailure, RupdFailure))
import           Shelley.Spec.Ledger.STS.Tickn (TicknPredicateFailure)
import           Shelley.Spec.Ledger.STS.Updn (UpdnPredicateFailure)
import           Shelley.Spec.Ledger.STS.Utxo (UtxoPredicateFailure (BadInputsUTxO, ExpiredUTxO, FeeTooSmallUTxO, InputSetEmptyUTxO, MaxTxSizeUTxO, OutputBootAddrAttrsTooBig, OutputTooSmallUTxO, UpdateFailure, ValueNotConservedUTxO, WrongNetwork, WrongNetworkWithdrawal))
import           Shelley.Spec.Ledger.STS.Utxow (UtxowPredicateFailure (ConflictingMetaDataHash, InvalidWitnessesUTXOW, MIRInsufficientGenesisSigsUTXOW, MissingScriptWitnessesUTXOW, MissingTxBodyMetaDataHash, MissingTxMetaData, MissingVKeyWitnessesUTXOW, ScriptWitnessNotValidatingUTXOW, UtxoFailure))

