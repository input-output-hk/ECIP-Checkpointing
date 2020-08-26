{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Blockchain.Ledger.BFT (
    MorphoBftBlock
  , MorphoBftHeader
  , MorphoBftExt(..)
  , SignedMorphoBft(..)
  ) where

import           Prelude  

import           Codec.Serialise (Serialise (..))
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)

import           Cardano.Binary (ToCBOR (..))
import           Cardano.Crypto.DSIGN
import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Protocol.BFT
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense

import           Blockchain.Ledger.Run
import           Blockchain.Ledger.Block

{-------------------------------------------------------------------------------
  Instantiate the @ext@ to suit BFT
-------------------------------------------------------------------------------}

-- | Morpho block extended with the fields required for BFT
--
-- @c@  is crypto used for the block itself
-- @c'@ is crypto used for the consensus protocol
type MorphoBftBlock c c' = MorphoBlock c (MorphoBftExt c c')

-- | Header for BFT
type MorphoBftHeader c c' = MorphoHeader c (MorphoBftExt c c')

-- | Block extension required for BFT
newtype MorphoBftExt c c' = MorphoBftExt {
      morphoBftExt :: BftFields c' (SignedMorphoBft c c')
    }
  deriving (Condense, Show, Eq, NoUnexpectedThunks)

-- | Part of the block that gets signed
data SignedMorphoBft c c' = SignedMorphoBft {
      signedMorphoBft :: MorphoStdHeader c (MorphoBftExt c c')
    }
  deriving (Generic)

type instance BlockProtocol (MorphoBftBlock  c c') = Bft c'
type instance BlockProtocol (MorphoBftHeader c c') = BlockProtocol (MorphoBftBlock c c')

-- | Sanity check that block and header type synonyms agree
_morphoBFtHeader :: MorphoBftBlock c c' -> MorphoBftHeader c c'
_morphoBFtHeader = morphoHeader

{-------------------------------------------------------------------------------
  Evidence that MorphoBlock can support BFT
-------------------------------------------------------------------------------}

instance SignedHeader (MorphoBftHeader c c') where
  type Signed (MorphoBftHeader c c') = SignedMorphoBft c c'

  headerSigned = SignedMorphoBft . morphoHeaderStd

instance ( MorphoCrypto c
         , BftCrypto c'
         , Signable (BftDSIGN c') (SignedMorphoBft c c')
         ) => HeaderSupportsBft c' (MorphoBftHeader c c') where
  headerBftFields _ = morphoBftExt . morphoHeaderExt

instance RunMorphoProtocol (Bft c') where
  protocolMagicId  = const constructProtocolMagicId
  encodeChainState = const encode
  decodeChainState = const decode

instance ( MorphoCrypto c
         , BftCrypto c'
         , Signable (BftDSIGN c') (SignedMorphoBft c c')
         )
      => RunMorphoBlock (Bft c') c (MorphoBftExt c c') where
  forgeExt cfg () MorphoBlock{..} = do
      ext :: MorphoBftExt c c' <- fmap MorphoBftExt $
        forgeBftFields cfg $
          SignedMorphoBft {
              signedMorphoBft = morphoHeaderStd
            }
      return MorphoBlock {
          morphoHeader = mkMorphoHeader encode morphoHeaderStd ext
        , morphoBody   = morphoBody
        }
    where
      MorphoHeader{..} = morphoHeader

instance ( MorphoCrypto c
         , BftCrypto c'
         , Signable (BftDSIGN c') (SignedMorphoBft c c')
         ) => SupportedBlock (MorphoBftBlock c c')

instance ( MorphoCrypto c
         , BftCrypto c'
         , Signable (BftDSIGN c') (SignedMorphoBft c c')
         ) => ProtocolLedgerView (MorphoBftBlock c c') where
  protocolLedgerView _ _ = ()
  anachronisticProtocolLedgerView _ _ _ = Right ()

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance BftCrypto c' => Serialise (MorphoBftExt c c') where
  encode (MorphoBftExt BftFields{..}) = mconcat [
        encodeSignedDSIGN bftSignature
      ]
  decode = do
      bftSignature <- decodeSignedDSIGN
      return $ MorphoBftExt BftFields{..}

instance MorphoCrypto c => Serialise (SignedMorphoBft c c')
instance (Typeable c', MorphoCrypto c) => ToCBOR (SignedMorphoBft c c') where
  toCBOR = encode
