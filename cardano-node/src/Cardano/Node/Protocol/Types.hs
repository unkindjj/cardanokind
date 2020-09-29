{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Node.Protocol.Types
  ( Protocol(..)
  , SomeConsensusProtocol(..)
  , SomeConsensusProtocolConstraints
  ) where

import           Cardano.Prelude

import           Control.Monad.Fail (fail)
import           Data.Aeson

--import           Cardano.Api.Byron (Protocol)
import qualified Cardano.Api.Byron as Byron
import           Cardano.Api.Shelley hiding (Protocol (..))

import           Cardano.Tracing.Constraints (TraceConstraints)
import           Cardano.Tracing.Metrics (HasKESMetricsData)

data Protocol = ByronProtocol
              | ShelleyProtocol
              | CardanoProtocol
  deriving (Eq, Show, Generic)

deriving instance NFData Protocol
deriving instance NoUnexpectedThunks Protocol

instance FromJSON Protocol where
  parseJSON =
    withText "Protocol" $ \str -> case str of

      -- The new names
      "Byron" -> pure ByronProtocol
      "Shelley" -> pure ShelleyProtocol
      "Cardano" -> pure CardanoProtocol

      -- The old names
      "RealPBFT" -> pure ByronProtocol
      "TPraos" -> pure ShelleyProtocol

      _ -> fail $ "Parsing of Protocol failed. "
                <> show str <> " is not a valid protocol"

type SomeConsensusProtocolConstraints blk =
     ( HasKESMetricsData blk
     , RunNode blk
     , TraceConstraints blk
     )


data SomeConsensusProtocol where

     SomeConsensusProtocol :: SomeConsensusProtocolConstraints blk
                           => Byron.Protocol IO blk (BlockProtocol blk)
                           -> SomeConsensusProtocol
