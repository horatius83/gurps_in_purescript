module TechLevel (
    TechLevel(..),
    createTechLevel
) where
  
import Data.Maybe (Maybe(..))
import Prelude

newtype TechLevel = TechLevel Int

createTechLevel :: Int -> Maybe TechLevel
createTechLevel level | level >= 0 = Just $ TechLevel level
                      | otherwise = Nothing

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
            8 -> "Early Spacefaring"
            9 -> "Solar System Colonization"
            10 -> "Low FTL"
            11 -> "Star System Colonization"
            12 -> "Fast FTL"
            13 -> "Galactic Colonization"
            14 -> "Kardashev Type 2" 
            15 -> "Kardeshev Type 3"
            _ -> "Transcendent"
