{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.DearImGui.Events

where

import Data.Functor.Identity (Identity(..))


import Data.Dependent.Map (DMap)
import Data.Dependent.Sum ((==>))
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.GADT.Show (GShow(..))
import Data.GADT.Show.TH (deriveGShow)

import qualified Data.Dependent.Map as DMap

import Data.ByteString (ByteString)
import Text.Show (showParen)