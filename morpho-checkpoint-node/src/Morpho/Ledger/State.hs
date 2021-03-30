{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Morpho.Ledger.State
  ( -- * State of the mock ledger
    MorphoState (..),
    MorphoTransactionError (..),
    MorphoError (..),
    MorphoLedgerConfig (..),

    -- * Genesis state
    genesisMorphoState,
  )
where

import Cardano.Prelude
import Codec.Serialise (Serialise)
import qualified Data.Map as M
import Morpho.Crypto.ECDSASignature
import Morpho.Ledger.PowTypes
import NoThunks.Class
import Ouroboros.Consensus.Block.Abstract hiding (blockNo)
import Ouroboros.Consensus.BlockchainTime
import Ouroboros.Consensus.Config.SecurityParam
import Ouroboros.Network.Block
  ( genesisPoint,
  )

{-------------------------------------------------------------------------------
  State of the ledger
-------------------------------------------------------------------------------}

data MorphoState blk = MorphoState
  { lastCheckpoint :: !Checkpoint,
    checkpointAt :: !(Point blk), -- when the checkpoint was created, used to determine if the checkpoint should be pushed to PoW Node
    currentVotes :: !(Map PublicKey Vote),
    morphoTip :: !(Point blk)
  }
  deriving (Show, Eq, Generic, NoThunks)

deriving instance Serialise (HeaderHash l) => Serialise (MorphoState l)

data MorphoTransactionError
  = MorphoWrongDistance
  | MorphoInvalidSignature
  | MorphoDuplicateVote
  | MorphoUnknownPublicKey
  deriving (Show, Eq, Generic, NoThunks, Serialise)

data MorphoError blk
  = MorphoTransactionError Vote MorphoTransactionError
  | MorphoInvalidHash (ChainHash blk) (ChainHash blk)
  deriving (Generic, NoThunks)

data MorphoLedgerConfig = MorphoLedgerConfig
  { checkpointingInterval :: Int,
    securityParam :: SecurityParam,
    requiredMajority :: Int,
    fedPubKeys :: [PublicKey],
    -- Used to reconstruct `EraParams`
    slotLength :: !SlotLength,
    -- TODO: Should we move this to the block config ?
    --       See MorphoBlockConfig for more informations.
    nodeKeyPair :: !KeyPair
  }
  deriving stock (Show, Eq, Generic)
  deriving (NoThunks)

deriving instance StandardHash blk => Show (MorphoError blk)

deriving instance StandardHash blk => Eq (MorphoError blk)

deriving instance Serialise (HeaderHash blk) => Serialise (MorphoError blk)

{-------------------------------------------------------------------------------
  Genesis
-------------------------------------------------------------------------------}

genesisMorphoState :: MorphoState blk
genesisMorphoState =
  MorphoState
    { lastCheckpoint = genesisCheckpoint,
      checkpointAt = genesisPoint,
      currentVotes = M.empty,
      morphoTip = genesisPoint
    }
