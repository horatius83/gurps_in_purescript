module Attributes where

import Prelude

import BuildPoints (BuildPoint(..))
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Level (Level, getLevel)
import TechLevel (TechLevel(..))
  
-- Basic attributes
newtype Attribute = Attribute Level 

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


-- Physical Appearance
data Handedness = Left | Right | Ambidexterous

newtype Reaction = Reaction Int

data Appearance = Hideous | Ugly | Unattractive | Average | Attracive | Beautiful | Gorgeous

getAppearanceReactionModifier :: Appearance -> Reaction
getAppearanceReactionModifier a = Reaction 
    case a of
        Hideous -> (-4)
        Ugly -> (-2)
        Unattractive -> (-1)
        Average -> 0
        Attracive -> 1
        Beautiful -> 4
        Gorgeous -> 6

getAppearanceBp :: Appearance -> BuildPoint
getAppearanceBp a = BuildPoint
    case a of
        Hideous -> (-20)
        Ugly -> (-10)
        Unattractive -> (-5)
        Average -> 0
        Attracive -> 5
        Beautiful -> 15
        Gorgeous -> 25

-- Wealth and Status
data Wealth = Destitute | Poor | Struggling | LowerMiddleClass | UpperMiddleClass | Wealthy | VeryWealthy | UltraWealthy
data Money = Money Int

getWealthBp :: Wealth -> BuildPoint
getWealthBp w = BuildPoint
    case w of
        Destitute -> (-25)
        Poor -> (-15)
        Struggling -> (-10)
        LowerMiddleClass -> 0
        UpperMiddleClass -> 10
        Wealthy -> 20
        VeryWealthy -> 30
        UltraWealthy -> 50

getAverageWealthPerTechLevel :: TechLevel -> Money
getAverageWealthPerTechLevel (TechLevel tl) = Money (floor fpr)
    where
        fp = toNumber tl 
        fpr = (1.0 + fp) * 1500.0

getStartingWealth :: Wealth -> TechLevel -> Money
getStartingWealth wealth tl = multiplyMoney multiplier avgAmount
    where 
        multiplyMoney x (Money m) = Money (floor (x * (toNumber m)))
        avgAmount = getAverageWealthPerTechLevel tl
        multiplier =
            case wealth of
                Destitute -> 0.0
                Poor -> 0.2
                Struggling -> 0.5
                LowerMiddleClass -> 1.0
                UpperMiddleClass -> 2.0
                Wealthy -> 5.0
                VeryWealthy -> 20.0
                UltraWealthy -> 100.0

-- Reputation
newtype ReputationModifier = ReputationModifier Int
data SizeOfGroup = Everyone | Large | Small
data FrequencyOfRecognition = Always | Sometimes | Rarely
type NameOfGroup = String 
data Group = Group NameOfGroup SizeOfGroup FrequencyOfRecognition
data Reputation = Reputation ReputationModifier Group

createReputationModifier :: Int -> Maybe ReputationModifier
createReputationModifier x | x > -5 && x < 5 = Just $ ReputationModifier x
                           | otherwise = Nothing

getReputationBp :: Reputation -> BuildPoint
getReputationBp (Reputation (ReputationModifier m) (Group _ sizeOfGroup freq)) = BuildPoint bps
    where
        sizeOfGroupModifier = case sizeOfGroup of
            Everyone -> 1.0
            Large -> 0.5
            Small -> 0.33
        frequencyModifier = case freq of
            Always -> 1.0
            Sometimes -> 0.5
            Rarely -> 0.33
        bps = floor $ sizeOfGroupModifier * frequencyModifier * (toNumber m) * 5.0

-- Status
newtype Status = Status Int

createStatus :: Int -> Maybe Status
createStatus x | x >= -4 && x <= 8 = Just $ Status x
               | otherwise = Nothing

getStatusBp :: Status -> BuildPoint
getStatusBp (Status x) = BuildPoint $ x * 5

