module TechLevel (
    TechLevel(..)
) where
  
import Data.Show (class Show)

newtype TechLevel = TechLevel Int

instance showTechLevel :: Show TechLevel where
    show (TechLevel t) = 
        case t of
            0 -> "Stone Age"
            1 -> "Bronze Age"
            2 -> "Iron Age"
            3 -> "Medieval"
            4 -> "Renaissance/Colonial"
            5 -> "Industrial Revolution"
            6 -> "World War I/World War II"
            7 -> "Modern"
            8 -> "Space Age"
            9 -> "Diamond Age"
            10 -> "Kardashev Type 1"
            11 -> "FTL radio"
            12 -> "Slow FTL"
            13 -> "Fast FTL"
            14 -> "Kardashev Type 2" 
            15 -> "Kardeshev Type 3"
            _ -> "Transcendent"
