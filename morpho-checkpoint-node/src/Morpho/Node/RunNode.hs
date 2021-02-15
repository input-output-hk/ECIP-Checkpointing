{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Morpho.Node.RunNode
  (
  )
where

import Cardano.Crypto.DSIGN.Class
import Cardano.Prelude
import Morpho.Ledger.Block
import Morpho.Ledger.Forge ()
import Morpho.Ledger.Serialise ()
import Morpho.Ledger.State
import Morpho.Ledger.Update
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Config.SupportsNode
import Ouroboros.Consensus.Forecast
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.SupportsPeerSelection
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Node.InitStorage
import Ouroboros.Consensus.Node.Run
import Ouroboros.Consensus.Protocol.BFT
import Ouroboros.Consensus.Protocol.Signed
import Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import Ouroboros.Consensus.Util

{-------------------------------------------------------------------------------
  RunNode instance for the Morpho ledger
-------------------------------------------------------------------------------}

instance ConfigSupportsNode (MorphoBlock h c) where
  getSystemStart = systemStart
  getNetworkMagic = networkMagic

instance SignedHeader (Header (MorphoBlock h c)) where
  headerSigned = morphoHeaderStd

type instance Signed (Header (MorphoBlock h c)) = MorphoStdHeader h c

instance
  forall h c.
  ( HashAlgorithm h,
    BftCrypto c,
    Signable (BftDSIGN c) (MorphoStdHeader h c),
    MorphoStateDefaultConstraints h c
  ) =>
  BlockSupportsProtocol (MorphoBlock h c)
  where
  validateView _ = bftValidateView morphoBftFields

instance
  (HashAlgorithm h, Signable (BftDSIGN c) (MorphoStdHeader h c), MorphoStateDefaultConstraints h c) =>
  LedgerSupportsProtocol (MorphoBlock h c)
  where
  protocolLedgerView _ _ = TickedTrivial
  ledgerViewForecastAt _ = trivialForecast

instance InspectLedger (MorphoBlock h c)

instance LedgerSupportsPeerSelection (MorphoBlock h c) where
  getPeers = const []

instance (HashAlgorithm h, BftCrypto c) => NodeInitStorage (MorphoBlock h c) where
  nodeImmutableDbChunkInfo (MorphoStorageConfig sParam) =
    -- TODO: keep as big as possible without creating too big chunk files.
    simpleChunkInfo $ EpochSize (1000 * maxRollbacks sParam)
  nodeCheckIntegrity _ block = blockMatchesHeader (getHeader block) block

instance (Typeable h, Typeable c) => ShowProxy (GenTx (MorphoBlock h c))

instance (Typeable h, Typeable c) => ShowProxy (TxId (GenTx (MorphoBlock h c)))

instance (Typeable h, Typeable c) => ShowProxy (MorphoBlock h c)

instance (Typeable h, Typeable c) => ShowProxy (MorphoError (MorphoBlock h c))

instance (Typeable h, Typeable c) => ShowProxy (Header (MorphoBlock h c))

instance (Typeable h, Typeable c) => ShowProxy (Query (MorphoBlock h c))

type instance CannotForge (MorphoBlock h c) = ()

type instance ForgeStateUpdateError (MorphoBlock h c) = Void

instance
  ( MorphoStateDefaultConstraints h c,
    HashAlgorithm h,
    Signable (BftDSIGN c) (MorphoStdHeader h c)
  ) =>
  RunNode (MorphoBlock h c)
