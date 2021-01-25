{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module Reflex.DearImgGui.Events
  ( RBEvent(..)
  , RBKey(..)
  , MouseData(..)
  , dearImgGuiEventToRBEvent
  )
where

import Data.Functor.Identity (Identity(..))

import DearImgGui.Types (DearImgGuiEvent(..), Location(..))
import Graphics.Vty.Input.Events

import Data.Dependent.Map (DMap)
import Data.Dependent.Sum ((==>))
import Data.GADT.Compare (GEq(..), GCompare(..), GOrdering(..), (:~:)(..))
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.GADT.Show (GShow(..))
import Data.GADT.Show.TH (deriveGShow)

import qualified Data.Dependent.Map as DMap

import Data.ByteString (ByteString)
import Text.Show (showParen)