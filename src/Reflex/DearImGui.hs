{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Reflex.DearImgGui (
    runReflexDearImgGuiApp
  , switchReflexDearImgGuiApp
  , ReflexDearImgGuiApp(..)
  , module Reflex.DearImgGui.Types
  , module Reflex.DearImgGui.Events
  ) where

import Control.Monad (void, forever)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO, writeTVar)

import Reflex
import Reflex.Host.Basic

import DearImgGui.Types (DearImgGuiEvent(..), EventM)
import DearImgGui.Main (App(..), customMain, continue, halt, suspendAndResume)
import DearImgGui.BChan (newBChan, writeBChan)


import Reflex.DearImgGui.Types
import Reflex.DearImgGui.Events
