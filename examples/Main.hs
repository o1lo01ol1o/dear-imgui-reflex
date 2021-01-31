{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (forM_, guard, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Managed
import Control.Monad.Reader (MonadReader (..), runReaderT)
import DearImGui hiding (button, combo, dragFloat, dragFloat4, progressBar, selectable, separator, sliderFloat2, smallButton, text)
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

data ASum
  = OneThing
  | Another
  | AndAnother
  deriving (Eq, Enum, Bounded, Show)

guest :: (ReflexSDL2 t m, DynamicWriter t [ImGuiAction m] m, MonadReader Renderer m) => m ()
guest = do
  eC <- window "Bar Window" $ do
    text "Bar"
    button "click me"
  void . holdView (window "The button is yet unclicked" $ pure ()) $
    ffor eC $
      const
        ( window "There was a click!" $ do
            childWindow "And?" $ do
              text "See how we react?"
              separator $ pure ()
              selectionE <- comboFromBoundedEnum @ASum "A multi-select" "ASum value" show
              holdViewVoid (pure ()) $
                ffor selectionE $ \selection -> do
                  text . constDyn $ "You selected:  " <> show selection
              separator $ pure ()
              valueE <- dragFloat4 "A floating Selector" (0, 0, 0, 0) (constDyn $ 1 / 2) (-10) 10
              holdViewVoid (pure ()) $
                ffor valueE $ \value -> do
                  text . constDyn $ "You selected:  " <> show value
              separator $ pure ()
              svalueE <- sliderFloat2 "A sliding Selector" (0, 0) (-10) 10
              holdViewVoid (pure ()) $
                ffor svalueE $ \value -> do
                  text . constDyn $ "You selected:  " <> show value
              separator $ pure ()
              colorE <- colorPicker "A colorPicker" (ImVec3 0 0 0)
              holdViewVoid (pure ()) $
                ffor colorE $ \(ImVec3 x y z) -> do
                  text . constDyn $ "You selected:  " <> show (x, y, z)
        )
  void . holdView (window "Progress" $ progressBar (constDyn 0) (constDyn $ Just "0%")) $
    ffor eC $ const (window "Progress!" $ progressBar (constDyn 0.5) (constDyn $ Just "50%"))