{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forM_, guard, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Managed
import Control.Monad.Reader (MonadReader (..), runReaderT)
import DearImGui hiding (button, separator, text)
import qualified DearImGui
import DearImGui.OpenGL
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Graphics.GL
import Reflex
import Reflex.DearImGui
import Reflex.DearImGui.Types
import Reflex.SDL2
import SDL

main :: IO ()
main = do
  initializeAll
  runManaged $ do
    let cfg =
          defaultWindow
            { windowGraphicsContext = OpenGLContext defaultOpenGL,
              windowResizable = True,
              windowHighDPI = True,
              windowInitialSize = V2 640 480
            }
    window <- managed $ bracket (createWindow "dear-imgui-reflex" cfg) destroyWindow
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext
    r <- managed $ bracket (createRenderer window (-1) defaultRenderer) destroyRenderer
    rendererDrawBlendMode r $= BlendAlphaBlend
    _ <- managed $ bracket createContext destroyContext
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown
    _ <- managed_ $ bracket_ openGL2Init openGL2Shutdown
    liftIO $ host $ runReaderT (app window) r

app :: (ReflexSDL2 t m, MonadReader Renderer m) => Window -> m ()
app win = do
  (_, dynActions) <- runDynamicWriterT guest
  performEvent_ $
    ffor (updated dynActions) $ \actions -> do
      sequence_ [untilNothingM pollEventWithImGui, openGL2NewFrame, sdl2NewFrame win, newFrame]
      sequence_ actions
      sequence_ [glClear GL_COLOR_BUFFER_BIT, render, openGL2RenderDrawData =<< getDrawData, glSwapWindow win]
  where
    untilNothingM m = m >>= maybe (return ()) (\_ -> untilNothingM m)

guest :: (ReflexSDL2 t m, DynamicWriter t [ImGuiAction m] m, MonadReader Renderer m) => m ()
guest = do
  eC <- window "Bar Window" $ do
              text "Bar"
              button "click me"
          
  void . holdView (window "The button is yet unclicked" $ pure ()) $
    ffor eC $
      const
        ( window "There was a click!" $ do
            void . childWindow "And?" $ do
              text "See how we react?"
              separator $ text "We react with a separator!"
              button "And another button!"
        )