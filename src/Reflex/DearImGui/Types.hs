{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.DearImGui.Types where

import Control.Monad.Reader (MonadFix, MonadIO (..), void)
import Data.IORef (newIORef, readIORef)
import Data.Maybe (catMaybes)
import qualified DearImGui
import GHC.IORef (IORef)
import Reflex
  ( Adjustable,
    DynamicWriter (..),
    MonadHold (holdDyn),
    PerformEvent (Performable),
    PostBuild,
    Reflex (Dynamic, Event, updated),
    TriggerEvent (newTriggerEvent),
    accumDyn,
    constDyn,
    ffor,
    zipDyn,
    zipDynWith,
  )
import Reflex.SDL2
  ( HasSDL2Events (getTicksEvent),
    ReflexSDL2,
    holdView,
  )

-- | Strict, inductive tuple.
data a :/\ b = !a :/\ !b

zipDyn' :: Reflex t => Dynamic t a -> Dynamic t b -> Dynamic t (a :/\ b)
zipDyn' = zipDynWith (:/\)

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

-- | Every DearImGui call needs to be driven by the FRP network
-- This defines the stream of signals propagate the calls on tick.
-- This should be used sparingly.
sdlTick ::
  (ImGuiSDLReflex t m) =>
  m (Dynamic t ())
sdlTick = do
  evTick <- getTicksEvent
  holdDyn () (() <$ evTick)

tickPropagation ::
  ( ImGuiSDLReflex t m
  ) =>
  Performable m () ->
  m ()
tickPropagation f = do
  dState <- sdlTick
  commitAction $ ffor dState (const f)

mkButton ::
  ( ImGuiSDLReflex t m
  ) =>
  Dynamic t a ->
  (a -> IO Bool) ->
  m (Event t ())
mkButton tD f = do
  (eState, eFire) <- newTriggerEvent
  commitAction $ ffor tD (\t -> liftIO $ f t >>= (\c -> if c then eFire () else pure ()))
  pure eState

-- | Wraps `DearImgui.button` and returns an `Event t ()` that fires on click.
button ::
  ( ImGuiSDLReflex t m
  ) =>
  Dynamic t String ->
  m (Event t ())
button tD = mkButton tD DearImGui.button

-- | Wraps `DearImgui.begin` and `DearImgui.end`
-- This draws windows at every `sdlTick` as we have to drive the FRP network with something.
window ::
  ( ImGuiSDLReflex t m
  ) =>
  Dynamic t String ->
  m a ->
  m a
window tD child = do
  tickD <- sdlTick
  commitAction $ ffor (zipDyn' tD tickD) (\(a :/\ _) -> void $ DearImGui.begin a)
  child' <- child -- would be nice to only issue the childs events if the window is open, but this will make the result of this function into an `Event` which is probably too much hassle to be worth it, esp since conditionally rendering the window as a whole is relatively easy in reflex.
  commitAction $ ffor (zipDyn' tD tickD) (const DearImGui.end)
  pure child'

-- | Wraps `DearImgui.beginChild` and `DearImgui.endChild`
childWindow ::
  ( ImGuiSDLReflex t m
  ) =>
  Dynamic t String ->
  m a ->
  m a
childWindow tD child = do
  commitAction $ ffor tD (void . DearImGui.beginChild)
  child' <- child -- see the comment on `window`
  commitAction $ ffor tD (const DearImGui.endChild)
  pure child'

-- | Wraps `DearImgui.text`
text ::
  ( ImGuiSDLReflex t m
  ) =>
  Dynamic t String ->
  m ()
text tD = do
  commitAction $ ffor tD DearImGui.text

-- | Wraps `DearImgui.separator`
separator ::
  ( ImGuiSDLReflex t m
  ) =>
  m b ->
  m b
separator child = do
  tickPropagation DearImGui.separator
  child

-- | Wraps `DearImgui.sameLine`
sameLine ::
  ( ImGuiSDLReflex t m
  ) =>
  m b ->
  m b
sameLine child = do
  tickPropagation DearImGui.sameLine
  child

-- | Wraps `DearImgui.smallButton` and returns an `Event t ()` that fires on click.
smallButton ::
  ( ImGuiSDLReflex t m
  ) =>
  Dynamic t String ->
  m (Event t ())
smallButton tD = mkButton tD DearImGui.smallButton

-- | Wraps `DearImgui.arrowButton` and returns an `Event t ()` that fires on click.
arrowButton ::
  ( ImGuiSDLReflex t m
  ) =>
  Dynamic t String ->
  Dynamic t DearImGui.ImGuiDir ->
  m (Event t ())
arrowButton tD dD = mkButton (zipDyn tD dD) (uncurry DearImGui.arrowButton)

-- | Wraps `DearImgui.checkbox` and returns an `Event t Bool` that represents the state of the checkbox
checkbox ::
  ( ImGuiSDLReflex t m
  ) =>
  Bool ->
  Dynamic t String ->
  m (Event t Bool)
checkbox initValue labelD = do
  ref <- liftIO $ newIORef initValue
  (eClicked, eFire) <- newTriggerEvent
  commitAction $
    ffor
      labelD
      ( \label -> liftIO $ do
          void $ DearImGui.checkbox label ref
          v <- readIORef ref
          eFire v
      )
  pure eClicked

-- | Wraps `DearImgui.progressBar`
progressBar ::
  ( ImGuiSDLReflex t m
  ) =>
  Dynamic t Float ->
  Dynamic t (Maybe String) ->
  m ()
progressBar progressD overlayD = do
  let stD = zipDyn progressD overlayD
  commitAction $ ffor stD (uncurry DearImGui.progressBar)

-- | Wraps `DearImgui.bullet`
bullet ::
  ( ImGuiSDLReflex t m
  ) =>
  m ()
bullet = do
  dState <- sdlTick
  commitAction $ ffor dState (const DearImGui.bullet)

nothingBurger ::
  ( ImGuiSDLReflex t m
  ) =>
  m (Event t (Maybe a))
nothingBurger = pure . updated $ constDyn Nothing

-- | Given a `Bounded a`, `Enum a`, take a lable, an initial display, a way to to show that `a` and generate a `DearImgUI.combo` dropdown that allows the user to
-- select a value of `a`
comboFromBoundedEnum ::
  (Enum a, Bounded a, ImGuiSDLReflex t m) =>
  String ->
  String ->
  (a -> String) ->
  m (Event t (Maybe a))
comboFromBoundedEnum label previewValue showFn = do
  (eOpen, eFire) <- newTriggerEvent
  dState' <- accumDyn (`maybe` showFn) previewValue eOpen
  commitAction $ ffor dState' (liftIO . combo' eFire)
  pure eOpen
  where
    combo' fireFn pv' = do
      DearImGui.beginCombo label pv'
        >>= whenTrue
          ( do
              v <- safeHead . catMaybes <$> traverse mkSelectable [minBound .. maxBound]
              _ <- maybe (pure ()) (fireFn . Just) v -- Only update the event if we have a selection.  Otherewise future redraws will set it to Nothing
              DearImGui.endCombo
          )
    maybeTrue a True = Just a
    maybeTrue _ False = Nothing
    mkSelectable a = maybeTrue a <$> DearImGui.selectable (showFn a)
    safeHead [] = Nothing
    safeHead (x : _) = Just x

whenTrue :: IO () -> Bool -> IO ()
whenTrue f True = f
whenTrue _ False = pure ()

holdViewVoid :: (ImGuiSDLReflex t m) => m a -> Event t (m a) -> m ()
holdViewVoid d = void . holdView d

{-# INLINEABLE zipf #-}
zipf :: (t1 -> t2 -> t3) -> (t4 -> t5 -> t2) -> t1 -> t4 -> t5 -> t3
zipf fa fb a b c = c'
  where
    !b' = fb b c
    !c' = fa a b'

{-# INLINEABLE zipDyn3 #-}
zipDyn3 ::
  (Reflex t) =>
  Dynamic t a1 ->
  Dynamic t a2 ->
  Dynamic t b ->
  Dynamic t (a1 :/\ (a2 :/\ b))
zipDyn3 = zipf zipDyn' zipDyn'

{-# INLINEABLE zipDyn4 #-}
zipDyn4 ::
  Reflex t =>
  Dynamic t a1 ->
  Dynamic t a2 ->
  Dynamic t a ->
  Dynamic t b ->
  Dynamic t (a1 :/\ (a2 :/\ (a :/\ b)))
zipDyn4 a = zipf (zipDyn3 a) zipDyn'

dragFloat' ::
  (ImGuiSDLReflex t m) =>
  (String -> IORef a -> Float -> Float -> Float -> IO Bool) ->
  Dynamic t String ->
  a ->
  Dynamic t Float ->
  Dynamic t Float ->
  Dynamic t Float ->
  m (Event t a)
dragFloat' f descD initValue speedD minValueD maxValueD = do
  ref <- liftIO $ newIORef initValue
  (eValue, eFire) <- newTriggerEvent
  let argD = zipDyn4 descD speedD minValueD maxValueD
  commitAction $
    ffor
      argD
      ( \(desc :/\ (speed :/\ (minValue :/\ maxValue))) -> liftIO $ do
          f desc ref speed minValue maxValue
            >>= whenTrue
              ( do
                  v <- readIORef ref
                  eFire v
              )
      )
  pure eValue

dragFloat ::
  (ImGuiSDLReflex t m) =>
  Dynamic t String ->
  Float ->
  Dynamic t Float ->
  Dynamic t Float ->
  Dynamic t Float ->
  m (Event t Float)
dragFloat = dragFloat' DearImGui.dragFloat

dragFloat2 ::
  (ImGuiSDLReflex t m) =>
  Dynamic t String ->
  (Float, Float) ->
  Dynamic t Float ->
  Dynamic t Float ->
  Dynamic t Float ->
  m (Event t (Float, Float))
dragFloat2 = dragFloat' DearImGui.dragFloat2

dragFloat3 ::
  (ImGuiSDLReflex t m) =>
  Dynamic t String ->
  (Float, Float, Float) ->
  Dynamic t Float ->
  Dynamic t Float ->
  Dynamic t Float ->
  m (Event t (Float, Float, Float))
dragFloat3 = dragFloat' DearImGui.dragFloat3

dragFloat4 ::
  (ImGuiSDLReflex t m) =>
  Dynamic t String ->
  (Float, Float, Float, Float) ->
  Dynamic t Float ->
  Dynamic t Float ->
  Dynamic t Float ->
  m (Event t (Float, Float, Float, Float))
dragFloat4 = dragFloat' DearImGui.dragFloat4

sliderFloat' ::
  (ImGuiSDLReflex t m) =>
  (String -> IORef a -> Float -> Float -> IO Bool) ->
  Dynamic t String ->
  a ->
  Dynamic t Float ->
  Dynamic t Float ->
  m (Event t a)
sliderFloat' f descD initValue minValueD maxValueD = do
  ref <- liftIO $ newIORef initValue
  (eValue, eFire) <- newTriggerEvent
  let argD = zipDyn3 descD minValueD maxValueD
  commitAction $
    ffor
      argD
      ( \(desc :/\ (minValue :/\ maxValue)) -> liftIO $ do
          f desc ref minValue maxValue
            >>= whenTrue
              ( do
                  v <- readIORef ref
                  eFire v
              )
      )
  pure eValue

sliderFloat ::
  (ImGuiSDLReflex t m) =>
  Dynamic t String ->
  Float ->
  Dynamic t Float ->
  Dynamic t Float ->
  m (Event t Float)
sliderFloat = sliderFloat' DearImGui.sliderFloat

sliderFloat2 ::
  (ImGuiSDLReflex t m) =>
  Dynamic t String ->
  (Float, Float) ->
  Dynamic t Float ->
  Dynamic t Float ->
  m (Event t (Float, Float))
sliderFloat2 = sliderFloat' DearImGui.sliderFloat2

sliderFloat3 ::
  (ImGuiSDLReflex t m) =>
  Dynamic t String ->
  (Float, Float, Float) ->
  Dynamic t Float ->
  Dynamic t Float ->
  m (Event t (Float, Float, Float))
sliderFloat3 = sliderFloat' DearImGui.sliderFloat3

sliderFloat4 ::
  (ImGuiSDLReflex t m) =>
  Dynamic t String ->
  (Float, Float, Float, Float) ->
  Dynamic t Float ->
  Dynamic t Float ->
  m (Event t (Float, Float, Float, Float))
sliderFloat4 = sliderFloat' DearImGui.sliderFloat4

colorPicker ::
  (ImGuiSDLReflex t m) =>
  Dynamic t String ->
  DearImGui.ImVec3 ->
  m (Event t DearImGui.ImVec3)
colorPicker descD initValue = do
  ref <- liftIO $ newIORef initValue
  (eValue, eFire) <- newTriggerEvent
  commitAction $
    ffor
      descD
      ( \desc -> liftIO $ do
          DearImGui.colorPicker3 desc ref
            >>= whenTrue
              ( do
                  v <- readIORef ref
                  eFire v
              )
      )
  pure eValue