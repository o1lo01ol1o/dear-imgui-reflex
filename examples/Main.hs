{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracket_)
import Control.Monad (forM_, guard, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Managed
  ( MonadIO (liftIO),
    managed,
    managed_,
    runManaged,
  )
import Control.Monad.Reader (MonadReader (..), runReaderT)
import Data.Constraint.Extras (Has, has)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Sum (DSum (..), (==>))
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity (..))
import Data.Some (Some (..))
import DearImGui
  ( ImVec3 (ImVec3),
    createContext,
    destroyContext,
    getDrawData,
    newFrame,
    render,
  )
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
  ( DynamicWriter,
    PerformEvent (performEvent_),
    Reflex (updated),
    constDyn,
    ffor,
    runDynamicWriterT,
  )
import Reflex.DearImGui ()
import Reflex.DearImGui.Types
import Reflex.SDL2
  ( BlendMode (BlendAlphaBlend),
    MonadIO (liftIO),
    ReflexSDL2,
    Renderer,
    V2 (V2),
    Window,
    WindowConfig
      ( windowGraphicsContext,
        windowHighDPI,
        windowInitialSize,
        windowResizable
      ),
    WindowGraphicsContext (OpenGLContext),
    createRenderer,
    createWindow,
    defaultOpenGL,
    defaultRenderer,
    defaultWindow,
    destroyRenderer,
    destroyWindow,
    glCreateContext,
    glDeleteContext,
    glSwapWindow,
    holdView,
    host,
    initializeAll,
    rendererDrawBlendMode,
    ($=),
  )
import SDL
  ( BlendMode (BlendAlphaBlend),
    Renderer,
    V2 (V2),
    Window,
    WindowConfig
      ( windowGraphicsContext,
        windowHighDPI,
        windowInitialSize,
        windowResizable
      ),
    WindowGraphicsContext (OpenGLContext),
    createRenderer,
    createWindow,
    defaultOpenGL,
    defaultRenderer,
    defaultWindow,
    destroyRenderer,
    destroyWindow,
    glCreateContext,
    glDeleteContext,
    glSwapWindow,
    initializeAll,
    rendererDrawBlendMode,
    ($=),
  )

data FileOptions
  = NewFile
  | OpenFile
  | EtcFile
  deriving (Eq, Enum, Bounded, Show)

data EdtOptions = EditFoo | EditBar deriving (Eq, Enum, Bounded, Show)

data AMenu a where
  File :: AMenu FileOptions
  Edit :: AMenu EdtOptions
  DoesAnyoneUseMenusLikeThisAnymore :: AMenu Bool

deriving instance Show (AMenu a)

deriveArgDict ''AMenu

data ASum
  = OneThing
  | Another
  | AndAnother
  deriving (Eq, Enum, Bounded, Show)

showSome :: (forall a. Show a => Show (f a), Has Show f) => Some f -> String
showSome (Some f) = has @Show f $ show f

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
  menuE <- menuBarFromBoundedEnum (constDyn [Some File, Some Edit, Some DoesAnyoneUseMenusLikeThisAnymore]) showSome
  eC <- window "Bar Window" $ do
    holdViewVoid (pure ()) $
      ffor menuE $
        \case
          Just (File :=> Identity NewFile) -> text . constDyn $ "You selected new file!"
          Just (File :=> Identity OpenFile) -> text . constDyn $ "You selected open file!"
          Just (File :=> Identity EtcFile) -> text . constDyn $ "You selected etc file!"
          Just (Edit :=> Identity EditFoo) -> text . constDyn $ "You selected edit foo!"
          Just (Edit :=> Identity EditBar) -> text . constDyn $ "You selected edit bar!"
          Just (DoesAnyoneUseMenusLikeThisAnymore :=> Identity True) -> text . constDyn $ "Really?"
          Just (DoesAnyoneUseMenusLikeThisAnymore :=> Identity False) -> text . constDyn $ "¯\\_(ツ)_/¯"
    button "Click me!"
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
              separator $ pure ()
              plotHistogram "Histogram" (constDyn [0 .. 100])
        )
  void . holdView (window "Progress" $ progressBar (constDyn 0) (constDyn $ Just "0%")) $
    ffor eC $
      const
        ( window "Progress!" $ progressBar (constDyn 0.5) (constDyn $ Just "50%")
        )