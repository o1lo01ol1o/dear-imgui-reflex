{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.DearImGui where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO, writeTVar)
import Control.Exception
-- import DearImGui.Types (DearImGuiEvent(..), EventM)
-- import DearImGui.Main (App(..), customMain, continue, halt, suspendAndResume)
-- import DearImGui.BChan (newBChan, writeBChan)

import Control.Exception (bracket_)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed (managed, managed_, runManaged)
import Data.Coerce (coerce)
import Data.Text (Text)
import DearImGui
import qualified DearImGui
import DearImGui.OpenGL
  ( openGL2Init,
    openGL2NewFrame,
    openGL2RenderDrawData,
    openGL2Shutdown,
  )
import DearImGui.SDL
  ( pollEventWithImGui,
    sdl2NewFrame,
    sdl2Shutdown,
  )
import DearImGui.SDL.OpenGL (sdl2InitForOpenGL)
import Graphics.GL
import Reflex
import Reflex.DearImGui.Events
import Reflex.DearImGui.Types
import Reflex.Host.Headless
import SDL
  ( Window,
    WindowConfig,
    createWindow,
    destroyWindow,
    glCreateContext,
    glDeleteContext,
    glSwapWindow,
    initializeAll,
  )


