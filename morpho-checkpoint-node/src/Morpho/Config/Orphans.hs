{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Morpho.Config.Orphans where

import Cardano.Prelude
import Data.Aeson
import Ouroboros.Consensus.BlockchainTime

instance FromJSON SlotLength where
  parseJSON value = slotLengthFromSec <$> parseJSON value
