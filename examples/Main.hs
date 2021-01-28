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
import DearImGui hiding (button)
import qualified DearImGui
import DearImGui.OpenGL
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Graphics.GL
import Reflex
import Reflex.DearImGui
import Reflex.SDL2
import SDL

main :: IO ()
main = do
  initializeAll
  runManaged $ do
    let ogl = defaultOpenGL {glProfile = Core Debug 3 3}
        cfg =
          defaultWindow
            { windowGraphicsContext = OpenGLContext defaultOpenGL,
              windowResizable = True,
              windowHighDPI = True,
              windowInitialSize = V2 640 480
            }
    window <- managed $ bracket (createWindow "reflex-sdl2-exe" cfg) destroyWindow
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext

    pure $ putStrLn "creating renderer..."
    r <- managed $ bracket (createRenderer window (-1) defaultRenderer) destroyRenderer
    -- rendererDrawBlendMode r $= BlendAlphaBlend
    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext

    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown

    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL2Init openGL2Shutdown
    liftIO $ host $ runReaderT (app window) r

-- liftIO $ testy window
type Layer m = Performable m ()

testy :: Window -> IO ()
testy w = do
  -- Process the event loop
  untilNothingM pollEventWithImGui

  -- Tell ImGui we're starting a new frame
  openGL2NewFrame
  sdl2NewFrame w
  newFrame

  -- Build the GUI
  bracket_ (begin "Hello, ImGui!") end $ do
    -- Add a text widget
    text "Hello, ImGui!"

    -- Add a button widget, and call 'putStrLn' when it's clicked
    DearImGui.button "Clickety Click" >>= \case
      False -> return ()
      True -> putStrLn "Ow!"

  -- Show the ImGui demo window
  showDemoWindow

  -- Render
  glClear GL_COLOR_BUFFER_BIT

  render
  openGL2RenderDrawData =<< getDrawData

  glSwapWindow w

  testy w
  where
    untilNothingM m = m >>= maybe (return ()) (\_ -> untilNothingM m)

mainLoop :: MonadIO m => m a -> Window -> m b
mainLoop ma win = do
  -- Process the event loop
  untilNothingM pollEventWithImGui
  -- Tell ImGui we're starting a new frame
  openGL2NewFrame
  sdl2NewFrame win
  newFrame
  ma
  glClear GL_COLOR_BUFFER_BIT
  render
  openGL2RenderDrawData =<< getDrawData
  glSwapWindow win
  mainLoop ma win
  where
    untilNothingM m = m >>= maybe (return ()) (\_ -> untilNothingM m)

app :: (ReflexSDL2 t m, MonadReader Renderer m) => Window -> m ()
app = mainLoop guest

guest :: (ReflexSDL2 t m, MonadReader Renderer m) => m ()
guest = do
  eC <- withWindow "bar" $ do
    text "Bar"
    button "clickcc"
  void . holdView (withWindow "Nothing" $ pure ()) $
    ffor (traceEvent "bar" eC) $ const ( withWindow "Something" $ text "True")

-- main :: IO ()
-- main = runReflexImGui title config (withWindow "Text" $  button "Click!")
--   where
--     title = "Hello, Dear ImGui!"
--     config = defaultWindow {windowGraphicsContext = OpenGLContext defaultOpenGL}