module Attributes where

import Level (Level, getLevel)
{-import Data.Ord
import Data.HeytingAlgebra         -}
import Prelude

  
newtype Attribute = Attribute Level 
newtype BuildPoint = BuildPoint Int

type Attributes = {strength :: Attribute, dexterity :: Attribute, intelligence :: Attribute, health :: Attribute}

getPointCost :: Attribute -> BuildPoint
getPointCost (Attribute level) =  getPointCostFromValue levelValue
    where
        levelValue = getLevel level 
        getPointCostFromValue x
            | x >= 1 && x < 8 = BuildPoint $ (-90) + x * 10
            | x == 8 = BuildPoint (-15)
            | x == 9 = BuildPoint (-10)
            | x >= 10 && x <14 = BuildPoint ((x-10) * 10)
            | x == 14 = BuildPoint 45
            | x == 15 = BuildPoint 60
            | x == 16 = BuildPoint 80
            | otherwise = BuildPoint (100 + ((x - 17) * 25))