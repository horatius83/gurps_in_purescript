module World (
    World,
    DivinityType(..)
) where

import TechLevel (TechLevel)

data DivinityType = Secular | Divine

type World = 
    {   techLevel :: TechLevel
    ,   divinityType :: DivinityType
    }
