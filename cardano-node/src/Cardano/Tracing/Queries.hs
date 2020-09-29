{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Tracing.Queries
  (LedgerQueries(..))
where

import           Prelude (Int, error, (.))

import qualified Data.Map.Strict as Map

import           Cardano.Api.Byron
import           Cardano.Api.Shelley
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Unary



class LedgerQueries blk where
  ledgerUtxoSize :: LedgerState blk -> Int

instance LedgerQueries ByronBlock where
  ledgerUtxoSize = Map.size . unUTxO . cvsUtxo . byronLedgerState

instance LedgerQueries (ShelleyBlock era) where
  ledgerUtxoSize =
      (\(UTxO xs)-> Map.size xs)
    . _utxo
    . _utxoState
    . esLState
    . nesEs
    . shelleyLedgerState

instance (LedgerQueries x, NoHardForks x)
      => LedgerQueries (HardForkBlock '[x]) where
  ledgerUtxoSize = ledgerUtxoSize . project

instance LedgerQueries (CardanoBlock c) where
  ledgerUtxoSize = \case
    Consensus.LedgerStateByron   ledgerByron   -> ledgerUtxoSize ledgerByron
    Consensus.LedgerStateShelley ledgerShelley -> ledgerUtxoSize ledgerShelley
    _ -> error "ledgerUtxoSize:  unhandled CardanoBlock case"
