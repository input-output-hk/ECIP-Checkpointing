{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module Blockchain.Ledger.Run (
    RunMorphoProtocol(..)
  , RunMorphoBlock(..)
  , constructProtocolMagicId
  ) where

import           Prelude
import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Crypto.Random (MonadRandom)
import           Data.Hashable (hash)
import           GHC.Stack

import           Cardano.Crypto (ProtocolMagicId (..))

import           Ouroboros.Consensus.Protocol.Abstract

import           Blockchain.Ledger.Block

-- | The part of 'RunMock' that depends only on @p@
class RunMorphoProtocol p where
  protocolMagicId  ::           NodeConfig p -> ProtocolMagicId
  encodeChainState ::           NodeConfig p -> ChainState p -> Encoding
  decodeChainState :: forall s. NodeConfig p -> Decoder s (ChainState p)

-- | Protocol specific functionality required to run consensus with blocks
class RunMorphoProtocol p => RunMorphoBlock p c ext where
  -- | Construct the protocol specific part of the block
  --
  -- This is used in 'forgeMorpho', which takes care of the generic part of
  -- the block.
  forgeExt :: (HasNodeState p m, MonadRandom m)
           => NodeConfig p
           -> IsLeader p
           -> MorphoBlock' c ext ()
           -> m (MorphoBlock c ext)

-- | Construct protocol magic ID depending on where in the code this is called
--
-- The sole purpose of this is to make sure that these protocols have
-- different IDs from each other and from regular protocols.
constructProtocolMagicId :: HasCallStack => ProtocolMagicId
constructProtocolMagicId =
    ProtocolMagicId $ fromIntegral $ hash (prettyCallStack callStack)
