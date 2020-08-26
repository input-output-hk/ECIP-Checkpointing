{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Blockchain.Node.RunNode () where

import           Prelude

import           Codec.Serialise (Serialise, decode, encode)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime (..))
import           Data.Typeable (Typeable)

import           Ouroboros.Network.Magic (NetworkMagic (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime (SystemStart (..))
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Node.Run.Abstract
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..),
                     protocolSecurityParam)

import           Ouroboros.Storage.Common (EpochSize (..))
import           Ouroboros.Consensus.Protocol.BFT

import Blockchain.Ledger.Forge
import Blockchain.Ledger.Block
import Blockchain.Ledger.Run


{-------------------------------------------------------------------------------
  RunNode instance for the mock ledger
-------------------------------------------------------------------------------}

instance ( ProtocolLedgerView (MorphoBlock MorphoMockCrypto ext)
           -- The below constraint seems redundant but is not! When removed,
           -- some of the tests loop, but only when compiled with @-O2@ ; with
           -- @-O0@ it is perfectly fine. ghc bug?!
         , SupportedBlock (MorphoBlock MorphoMockCrypto ext)
         , Typeable ext
         , Serialise ext
         , RunMorphoBlock (BlockProtocol (MorphoBlock MorphoMockCrypto ext))
                        MorphoMockCrypto
                        ext
         , BlockProtocol (MorphoBlock MorphoMockCrypto ext) ~ Bft MorphoMockCrypto
         ) => RunNode (MorphoBlock MorphoMockCrypto ext) where
  nodeForgeBlock          = forgeMorpho
  nodeBlockMatchesHeader  = matchesMorphoHeader
  nodeBlockFetchSize      = fromIntegral . morphoBlockSize . morphoHeaderStd
  nodeIsEBB               = const Nothing
  nodeEpochSize           = \_ cfg _ -> return $
    EpochSize $ 10 * maxRollbacks (protocolSecurityParam cfg)
  nodeStartTime           = \_ _ -> SystemStart dummyDate
    where
      --  This doesn't matter much
      dummyDate = UTCTime (fromGregorian 2019 8 13) 0
  nodeNetworkMagic        = \_ _ -> NetworkMagic 0x0000ffff

  nodeProtocolMagicId     = const protocolMagicId
  nodeHashInfo            = const morphoBlockHashInfo
  nodeCheckIntegrity      = \_ _ -> True

  nodeEncodeBlockWithInfo = const morphoBlockBinaryInfo
  nodeEncodeHeader        = const encode
  nodeEncodeGenTx         =       encode
  nodeEncodeGenTxId       =       encode
  nodeEncodeHeaderHash    = const encode
  nodeEncodeLedgerState   = const encode
  nodeEncodeChainState    = const encodeChainState
  nodeEncodeApplyTxError  = const encode

  nodeDecodeBlock         = const (const <$> decode)
  nodeDecodeHeader        = const (const <$> decode)
  nodeDecodeGenTx         =       decode
  nodeDecodeGenTxId       =       decode
  nodeDecodeHeaderHash    = const decode
  nodeDecodeLedgerState   = const decode
  nodeDecodeChainState    = const decodeChainState
  nodeDecodeApplyTxError  = const decode
