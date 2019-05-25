module Advantages (
    Level(..),
    Description,
    Advantage(..)
) where

import BuildPoints (BuildPoint(..))
--import Data.Map (fromFoldable)

data Level = Level Int | Fixed
type Description = String
data Advantage = Advantage Description Level BuildPoint

