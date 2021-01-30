{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.DearImGui.Types where

import Control.Lens.TH (makeLenses, makePrisms)
import Control.Monad.Exception (MonadAsyncException, MonadException)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict (StateT (StateT, runStateT))
import Data.Bifunctor (Bifunctor (first, second))
import Data.Kind
import Data.Text (Text)
import DearImGui
import DearImGui.OpenGL
import DearImGui.SDL
import GHC.Generics (Generic)
import Graphics.GL
import Reflex hiding (Additive)
import Reflex.Host.Class
import Reflex.SDL2

type ImGuiAction m = Performable m ()

type ImGuiSDLReflex t m =
  ( MonadIO (Performable m),
    MonadIO m,
    Adjustable t m,
    PostBuild t m,
    PerformEvent t m,
    TriggerEvent t m,
    HasSDL2Events t m,
    DynamicWriter t [Performable m ()] m,
    MonadHold t m,
    MonadFix m
  )

commitActions ::
  (DynamicWriter t [ImGuiAction m] m) =>
  Dynamic t [ImGuiAction m] ->
  m ()
commitActions = tellDyn

commitAction ::
  (ReflexSDL2 t m, DynamicWriter t [ImGuiAction m] m) =>
  Dynamic t (ImGuiAction m) ->
  m ()
commitAction = tellDyn . fmap pure

ffor2 :: Reflex t => Dynamic t a -> Dynamic t b -> (a -> b -> c) -> Dynamic t c
ffor2 a b f = zipDynWith f a b

ffor2up ::
  Reflex t => Dynamic t a -> Dynamic t b1 -> ((a, b1) -> b) -> Dynamic t b
ffor2up a b = ffor (zipDyn a b)

-- | Every DearImGui call needs to be driven by the FRP network
-- This defines the stream of signals propagate the calls.
sdlPropagation ::
  (ImGuiSDLReflex t m) =>
  m (Dynamic t ())
sdlPropagation = do
  evMotionData <- getAnySDLEvent
  holdDyn () (() <$ evMotionData)

constPropagation ::
  ( ImGuiSDLReflex t m
  ) =>
  Performable m () ->
  m ()
constPropagation f = do
  dState <- sdlPropagation
  commitAction $ ffor dState (const f)

mkButton ::
  ( ImGuiSDLReflex t m
  ) =>
  IO Bool ->
  m (Event t ())
mkButton f = do
  dState <- sdlPropagation
  (eState, eFire) <- newTriggerEvent
  commitAction $ ffor dState (const . liftIO $ f >>= (\c -> if c then eFire () else pure ()))
  pure eState

-- | Wraps `DearImgui.button` and returns an Event t () that fires on click.
button ::
  ( ImGuiSDLReflex t m
  ) =>
  String ->
  m (Event t ())
button t = mkButton (DearImGui.button t)

data EndWhen = EndOnlyWhenTrue | EndAlways

-- | Wraps a child between two functions when the first is True
mkWrappedWhen ::
  ( ImGuiSDLReflex t m
  ) =>
  EndWhen ->
  IO Bool ->
  Performable m () ->
  m a2 ->
  m (Event t a2)
mkWrappedWhen endW fb fe child = do
  dState <- sdlPropagation
  (eOpen, eFire) <- newTriggerEvent
  commitAction $
    ffor
      dState
      ( \_ -> do
          bool <- liftIO fb
          liftIO $ eFire bool
      )
  mDE <- holdView (handleClosed dState) $
    ffor eOpen $ \isOpen ->
      if isOpen
        then do
          child' <- child
          commitAction $ ffor dState (const fe)
          pure child'
        else handleClosed dState
  pure $ updated mDE
  where
    handleClosed dState = do
      child' <- child
      handleEnd dState endW
      pure child'
    handleEnd dState EndAlways = commitAction $ ffor dState (const fe)
    handleEnd _ EndOnlyWhenTrue = pure ()

-- | Wraps `DearImgui.begin` and `DearImgui.end`
-- TODO: clicking on the minimize button will only render the update on the next sdl event.  We need to merge a dynamic that watches the closed /open state of this and then combines update with the sdl event dynamic.
window ::
  ( ImGuiSDLReflex t m
  ) =>
  String ->
  m a ->
  m a
window t child = do
  dState <- sdlPropagation
  commitAction $ ffor dState (const . void $ DearImGui.begin t)
  child' <- child -- would be nice to only issue the childs events if the window is open, but this will make the result of this function into an `Event` which is probably too much hassle to be worth it, esp since conditionally rendering the window as a whole is relatively easy in reflex.
  commitAction $ ffor dState (const DearImGui.end)
  pure child'

childWindow ::
  ( ImGuiSDLReflex t m
  ) =>
  String ->
  m a ->
  m a
childWindow t child = do
  dState <- sdlPropagation
  commitAction $ ffor dState (const . void $ DearImGui.beginChild t)
  child' <- child -- would be nice to only issue the childs events if the window is open, but this will make the result of this function into an `Event` which is probably too much hassle to be worth it, esp since conditionally rendering the window as a whole is relatively easy in reflex.
  commitAction $ ffor dState (const DearImGui.endChild)
  pure child'

-- mkWrappedWhen EndOnlyWhenTrue (DearImGui.beginChild t) DearImGui.beginChild

-- | Wraps `DearImgui.text`
text ::
  ( ImGuiSDLReflex t m
  ) =>
  String ->
  m ()
text t = do
  dState <- sdlPropagation
  commitAction $ ffor dState (const $ DearImGui.text t)

-- | Wraps `DearImgui.separator`
separator ::
  ( ImGuiSDLReflex t m
  ) =>
  m b ->
  m b
separator child = do
  constPropagation DearImGui.separator
  child

-- | Wraps `DearImgui.sameLine`
sameLine ::
  ( ImGuiSDLReflex t m
  ) =>
  m b ->
  m b
sameLine child = do
  constPropagation DearImGui.sameLine
  child
