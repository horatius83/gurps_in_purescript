module Level (
    createLevel,
    addLevel,
    Level
)
where
  
import Data.Maybe (Maybe(..))
import Prelude

newtype Level = Level Int

createLevel :: Int -> Maybe Level
createLevel x | x < 1 = Nothing
              | otherwise = Just (Level x)

addLevel :: Level -> Int -> Maybe Level
addLevel (Level x) y = createLevel $ x + y