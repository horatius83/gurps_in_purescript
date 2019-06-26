module Advantages (
    Level(..),
    LevelCap,
    Description,
    Name,
    Advantage(..)
) where

import Prelude

import BuildPoints (BuildPoint(..))
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
import TechLevel (TechLevel(..))
import World (DivinityType(..), World)

data LevelCap = None | LevelCap Int
data Level = Level LevelCap BuildPoint | Fixed BuildPoint | WorldCalculation (World -> BuildPoint)
type Description = String
type Name = String
data Advantage = Advantage Name Description Level

advantageMap :: Map String Advantage
advantageMap = fromFoldable (map toTuple advantages)
    where
        advantages = [
            Advantage "Absolute Direction" "Absolute Direction" (Fixed (BuildPoint 5)),
            Advantage "Absolute Timing" "Absolute Timing" (Fixed (BuildPoint 5)),
            Advantage "Acute Hearing" "+1 bonus/lvl hearing" (Level None (BuildPoint 2)),
            Advantage "Acute Taste and Smell" "+1 bonus/lvl taste/smell" (Level None (BuildPoint 2)),
            Advantage "Acute Vision" "+1 bonus/lvl sight" (Level None (BuildPoint 2)),
            Advantage "Alertness" "+1 bonus/lvl any sense" (Level None (BuildPoint 5)),
            Advantage "Ambidexterity" "Do not suffer -4 DX penalty for off-hand use" (Fixed (BuildPoint 10)),
            Advantage "Animal Empathy" "+2 reaction wild/+4 tame/+4 any animal skill/dificulty killing animals unless hunting (+3 bonus)" (Fixed (BuildPoint 5)),
            Advantage "Charisma" "+1 reaction/lvl on any intelligent creature" (Level None (BuildPoint 5)),
            Advantage "Clerical Investment" "+1 reaction from parishioners, in divine worls can call on aid from divine" (WorldCalculation clericalCost),
            Advantage "Combat Reflexes" "+1 to active defense in combat, +1 to fast draw, +2 to fright check, never freeze, sides gets +1 initiative (+2 if leader), +6 on IQ roles to recover stun" (Fixed (BuildPoint 15)),
            Advantage "Common Sense" "GM rolls against your IQ, if successful they warn you of stupid moves" (Fixed (BuildPoint 10)),
            Advantage "Danger Sense" "GM rolls against you IQ any time there is an ambush/etc, if successful warns you" (Fixed (BuildPoint 15)),
            Advantage "Double-Jointed" "+3 on any climbing, escape or mechanic roll" (Fixed (BuildPoint 5)),
            Advantage "Eidetic Memory" "Lvl 1: Points in regular mental skills double, +1 magic spells. Lvl 2: Points x4, +2 magic spells" (Level (LevelCap 2) (BuildPoint 30)),
            Advantage "Empathy" "GM tells you how you 'feel' about a person" (Fixed (BuildPoint 15)),
            Advantage "High Pain Threshold" "Not stunned when hit in combat (except for head trauma / crits), +3 resist torture, will + 3 to ignore pain in other situations" (Fixed (BuildPoint 10)),
            Advantage "Immunity to Disease" "Immune to all viruses/bacteria/fungus (not parasites), need HT 12 or above" (Fixed (BuildPoint 10)),
            Advantage "Intuition" "Usually guess right. GM adds IQ to # of right choices, subtracts # of wrong choices (within reason)" (Fixed (BuildPoint 15)),
            Advantage "Language Talent" "Whenever you learn a language, add level of Language Talent to IQ (stacks with Eidetic Memory)" (Level None (BuildPoint 2)),
            Advantage "Law Enforcement Powers" "Lvl 1: Cop, Lvl2: FBI, Lvl3: CIA comes with Duty disadvantage and Reputation" (Level (LevelCap 3) (BuildPoint 5)),
            Advantage "Lightning Calculator" "Can do math in head instantly" (Fixed (BuildPoint 5)),
            Advantage "Literacy" "Can read (depends on tech level whether this takes BP)" (WorldCalculation literacyCost),
            Advantage "Longevity" "Fail aging rolls on a natural 17 or 18. Get no points for taking Age as disadvantage" (Fixed (BuildPoint 5)),
            Advantage "Luck" "Lvl 1: Once every hour of play, may roll 3 dice and choose best result, Lvl 2: ...every 30 minutes" (Level (LevelCap 2) (BuildPoint 15))
        ]
        toTuple a@(Advantage name _ _) = Tuple name a
        clericalCost {divinityType: Secular} = BuildPoint 5
        clericalCost {divinityType: Divine} = BuildPoint 10
        literacyCost {techLevel: (TechLevel x)} | x <= 4 = BuildPoint 10
                                    | otherwise = BuildPoint 0