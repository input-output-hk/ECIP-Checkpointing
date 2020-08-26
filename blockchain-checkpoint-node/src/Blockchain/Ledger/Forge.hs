{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Blockchain.Ledger.Forge (forgeMorpho) where

import           Prelude

import           Codec.Serialise (Serialise (..), serialise)
import           Crypto.Random (MonadRandom)
import           Data.Word
import qualified Data.ByteString.Lazy as Lazy

import           Cardano.Crypto.Hash

import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Network.Block (BlockNo, ChainHash, SlotNo)

import           Blockchain.Ledger.Block
import           Blockchain.Ledger.Run

forgeMorpho :: forall p c m ext.
               ( HasNodeState p m
               , MonadRandom m
               , MorphoCrypto c
               , RunMorphoBlock p c ext
               , p ~ (Bft c)
               )
            => NodeConfig p
            -> SlotNo                         -- ^ Current slot
            -> BlockNo                        -- ^ Current block number
            -> ChainHash (MorphoBlock c ext)  -- ^ Previous hash
            -> [GenTx (MorphoBlock c ext)]    -- ^ Txs to add in the block
            -> IsLeader p                     -- ^ Proof we are slot leader
            -> m (MorphoBlock c ext)
forgeMorpho cfg curSlot curBlock prevHash txs proof =
    forgeExt cfg proof $ MorphoBlock {
        morphoHeader = mkMorphoHeader encode stdHeader ()
      , morphoBody   = body
      }
  where
    body :: MorphoBody
    body = MorphoBody { morphoTxs = morphoGenTx <$> txs }

    stdHeader :: MorphoStdHeader c ext
    stdHeader = MorphoStdHeader {
          morphoPrev      = prevHash
        , morphoSlotNo    = curSlot
        , morphoBlockNo   = curBlock
        , morphoBodyHash  = hash body
        , morphoBlockSize = bodySize
        }

    -- We use the size of the body, not of the whole block (= header + body),
    -- since the header size is fixed and this size is only used for
    -- prioritisation.
    bodySize :: Word64
    bodySize = fromIntegral $ Lazy.length $ serialise body
