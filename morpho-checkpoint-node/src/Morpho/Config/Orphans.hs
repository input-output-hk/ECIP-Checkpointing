{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Morpho.Config.Orphans where

import Cardano.BM.Data.Tracer (TracingVerbosity (..))
import Cardano.Prelude
import Data.Aeson
import Data.Scientific (isInteger, toBoundedInteger)
import qualified Data.Text as T
import Ouroboros.Consensus.BlockchainTime
import Prelude (show)

deriving instance Show TracingVerbosity

instance FromJSON SlotLength where
  parseJSON (Number i)
    | isInteger i =
      maybe
        (panic "Paring of slot duration failed: integer overflow.")
        pure
        (slotLengthFromSec . toInteger <$> (toBoundedInteger i :: Maybe Int))
    | otherwise =
      panic $
        "Parsing of slot duration failed: "
          <> "the specified duration is not an integer."
  parseJSON invalid =
    panic $
      "Parsing of slot duration failed due to type mismatch. "
        <> "Encountered: "
        <> T.pack (Prelude.show invalid)
