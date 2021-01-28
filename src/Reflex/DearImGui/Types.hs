{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
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
import GHC.Generics (Generic)
import Reflex hiding (Additive)
import Reflex.Host.Class

-- data StaticSpecBuilderEnv t

-- data ElementSpec = Label Text | Button Text deriving stock (Generic)

-- data ImGuiSpec
--   = Window Text [ElementSpec]
--   | NilSpec
--   | BothSpec ImGuiSpec ImGuiSpec
--   deriving stock (Generic)

-- instance Semigroup ImGuiSpec where
--   (<>) a b = a `BothSpec` b

data ImGuiWidgetCtx t = ImGuiWidgetCtx

--   { _imGuiWidgetCtx_input :: Event t (Either () Text)
--   }

type RenderActions = IO ()

-- | A class for widgets that can produce IO () actions that correspond to dear-imgui renders
class (Reflex t, Monad m) => RenderWriter t m | m -> t where
  -- | Send images upstream for rendering
  tellRenders :: Behavior t [RenderActions] -> m ()

instance (Monad m, Reflex t) => RenderWriter t (BehaviorWriterT t [RenderActions] m) where
  tellRenders = tellBehavior

newtype ImGuiWidget t m a = ImGuiWidget
  { unImGuiWidget :: BehaviorWriterT t [RenderActions] (ReaderT (ImGuiWidgetCtx t) m) a
  }
  deriving stock
    (Functor, Generic)
  deriving newtype
    ( Applicative,
      Monad,
      MonadSample t,
      MonadHold t,
      MonadFix,
      NotReady t,
      RenderWriter t,
      PostBuild t,
      TriggerEvent t,
      MonadReflexCreateTrigger t,
      MonadIO
    )

deriving newtype instance PerformEvent t m => PerformEvent t (ImGuiWidget t m)

instance (Adjustable t m, MonadHold t m, Reflex t) => Adjustable t (ImGuiWidget t m) where
  runWithReplace a0 a' = ImGuiWidget $ runWithReplace (unImGuiWidget a0) $ fmap unImGuiWidget a'
  traverseIntMapWithKeyWithAdjust f dm0 dm' =
    ImGuiWidget $
      traverseIntMapWithKeyWithAdjust (\k v -> unImGuiWidget (f k v)) dm0 dm'
  traverseDMapWithKeyWithAdjust f dm0 dm' = ImGuiWidget $ do
    traverseDMapWithKeyWithAdjust (\k v -> unImGuiWidget (f k v)) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = ImGuiWidget $ do
    traverseDMapWithKeyWithAdjustWithMove (\k v -> unImGuiWidget (f k v)) dm0 dm'

-- | The output of a 'ImGuiWidget'
newtype ImGuiWidgetOut t = ImGuiWidgetOut
  { _imGuiWidgetOut_shutdown :: Event t ()
  }

instance MonadTrans (ImGuiWidget t) where
  lift f = ImGuiWidget $ lift $ lift f

-- | Runs a 'ImGuiWidget' with a given context
runImGuiWidget ::
  (Reflex t, Monad m) =>
  ImGuiWidgetCtx t ->
  ImGuiWidget t m a ->
  m (a, Behavior t [RenderActions])
runImGuiWidget ctx w = runReaderT (runBehaviorWriterT (unImGuiWidget w)) ctx

-- pane title child =  ImGuiWidget $ do
--   ctx <- lift ask
--   let ra = second (fmap (fmap ((:[]) . mconcat)) . sequenceA . sequenceA ). sequenceA $ withWindow' title ctx child
--   let (results, actionsM) = ra
--   actions <- liftIO actionsM
--   tellRenders actions
--   return results

-- withWindow' :: (Reflex t) => String -> ImGuiWidgetCtx t -> ImGuiWidget t IO a -> IO (a, Behavior t [RenderActions])
-- withWindow' t ctx child = do
--     void $ DearImGui.begin t
--     (r,a) <- runImGuiWidget ctx child
--     DearImGui.end
--     pure (r,a)

button' :: (TriggerEvent t m, Reflex t) => String -> ImGuiWidget t m (Event t ())
button' t = ImGuiWidget $ do
  (eState, eFire) <- newTriggerEvent
  let act = DearImGui.button t >>= (\c -> if c then eFire () else pure ())
  tellRenders $ constant [act]
  pure eState

withWindow :: (Monad m, Reflex t) => String -> ImGuiWidget t m a -> ImGuiWidget t m a
withWindow t child = ImGuiWidget $ do
  ctx <- ask
  let b = void $ DearImGui.begin t
  (results, actions) <- lift . lift $ runImGuiWidget ctx child
  let e = DearImGui.end
  tellRenders $ constant [b]
  tellRenders actions
  tellRenders $ constant [e]
  pure results

imGuiWidget :: (Reflex t, TriggerEvent t m) => ImGuiWidget t m (Event t ())
imGuiWidget = withWindow "foo" $ button' "fooBtn"