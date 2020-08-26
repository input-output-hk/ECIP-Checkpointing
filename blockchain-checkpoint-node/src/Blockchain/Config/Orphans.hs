{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Blockchain.Config.Orphans where


import Cardano.Prelude

import Cardano.BM.Data.Tracer (TracingVerbosity (..))
import Data.Aeson
import Data.Scientific (isInteger, toBoundedInteger)
import Data.Maybe (maybe)
import Ouroboros.Consensus.BlockchainTime
import qualified Cardano.Chain.Update as Update
import qualified Data.Text as T
import qualified Prelude

deriving instance Show TracingVerbosity

instance FromJSON Update.ApplicationName where
  parseJSON (String x) = pure $ Update.ApplicationName x
  parseJSON invalid  =
    panic $ "Parsing of application name failed due to type mismatch. "
    <> "Encountered: " <> (T.pack $ Prelude.show invalid)

instance FromJSON SlotLength where
  parseJSON (Number i)
    | isInteger i = maybe
        (panic $ "Paring of slot duration failed: integer overflow.")
        pure
        (slotLengthFromSec . toInteger <$> (toBoundedInteger i :: Maybe Int))
    | otherwise = panic $ "Parsing of slot duration failed: "
                  <> "the specified duration is not an integer."
  parseJSON invalid =
    panic $ "Parsing of slot duration failed due to type mismatch. "
    <> "Encountered: " <> (T.pack $ Prelude.show invalid)
