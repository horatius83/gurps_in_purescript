module Advantages (
    Level(..),
    Description,
    Name,
    Advantage(..)
) where

import BuildPoints (BuildPoint(..))
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
import Prelude

data Level = Level | Fixed | SecularWorld | DivineWorld
type Description = String
type Name = String
data Advantage = Advantage Name Description Level BuildPoint

advantageMap :: Map String Advantage
advantageMap = fromFoldable (map toTuple advantages)
    where
        advantages = [
            Advantage "Absolute Direction" "Absolute Direction" Fixed (BuildPoint 5),
            Advantage "Absolute Timing" "Absolute Timing" Fixed (BuildPoint 5),
            Advantage "Acute Hearing" "+1 bonus/lvl hearing" Level (BuildPoint 2),
            Advantage "Acute Taste and Smell" "+1 bonus/lvl taste/smell" Level (BuildPoint 2),
            Advantage "Acute Vision" "+1 bonus/lvl sight" Level (BuildPoint 2),
            Advantage "Alertness" "+1 bonus/lvl any sense" Level (BuildPoint 5),
            Advantage "Ambidexterity" "Do not suffer -4 DX penalty for off-hand use" Fixed (BuildPoint 10),
            Advantage "Animal Empathy" "+2 reaction wild/+4 tame/+4 any animal skill/dificulty killing animals unless hunting (+3 bonus)" Fixed (BuildPoint 5),
            Advantage "Charisma" "+1 reaction/lvl on any intelligent creature" Level (BuildPoint 5)
        ]
        toTuple a@(Advantage name _ _ _) = Tuple name a