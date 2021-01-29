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
import Data.Bifunctor (Bifunctor (second))
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

type ImGuiSDLReflex t m = (MonadIO (Performable m),
    MonadIO m,
    Adjustable t m,
    PostBuild t m,
    PerformEvent t m,
    TriggerEvent t m,
    HasSDL2Events t m,
    DynamicWriter t [Performable m ()] m,
    MonadHold t m,
    MonadFix m)

commitActions ::
  (ReflexSDL2 t m, DynamicWriter t [ImGuiAction m] m) =>
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

button ::
  ( ImGuiSDLReflex t m
  ) =>
  String ->
  m (Event t ())
button t = do
  evMotionData <- getAnySDLEvent
  (eState, eFire) <- newTriggerEvent
  dState <- holdDyn () (() <$ evMotionData)
  commitAction $ ffor dState (\_ -> liftIO $ DearImGui.button t >>= (\c -> if c then eFire () else pure ()))
  pure eState

window ::
  ( ImGuiSDLReflex t m
  ) =>
  String ->
  m b ->
  m b
window t child = do
  evMotionData <- getAnySDLEvent
  dState <- holdDyn () (() <$ evMotionData)
  commitAction $ ffor (dState) (\_ -> void $ DearImGui.begin t)
  child' <- child
  commitAction $ ffor (dState) (const DearImGui.end)
  pure child'


text ::
  ( ImGuiSDLReflex t m
  ) =>
  String ->
  m () 
text t = do 
  evMotionData <- getAnySDLEvent
  dState <- holdDyn () (() <$ evMotionData)
  commitAction $ ffor (dState) (\_ -> DearImGui.text t)
