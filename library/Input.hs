{-# LANGUAGE RankNTypes, Arrows #-}

module Input ( InputW
             , InputData(..)
             , Moving(..)
             , Rotating(..)
             , makeWire
             , defInputData
             ) where

import Prelude hiding ((.), id)

import Control.Monad.IO.Class
import Control.Monad.Reader

import Control.Wire.Core

import FRP.Netwire

import Graphics.UI.GLFW as GLFW
import Control.Wire.Unsafe.Event (Event(..)
                                 , event)

type ReaderW m a = ReaderT GLFW.Window m a
type InputW a = forall s m . (MonadIO m) => Wire s () (ReaderT GLFW.Window m) () a

newtype EventW a = EventW { unEventW :: Event a}

instance (Show a) => Show (EventW a) where
  show = event ("NoEvent") (("Event " ++) . show) . unEventW

instance (Eq a) => Eq (EventW a) where
  (==) (EventW a') (EventW b') = eventEq a' b'
    where
      eventEq :: (Eq a) => Event a -> Event a -> Bool
      eventEq (Event a) (Event b) = a == b
      eventEq (Event _) _ = False
      eventEq _ (Event _) = False
      eventEq _ _ = True

data Moving
  = ML
  | MR
  deriving (Eq, Show)

data Rotating
  = CW
  | CCW
  deriving (Eq, Show)

data InputData = InputData { inputDataMoving :: EventW (Maybe Moving)
                           , inputDataRotating :: EventW (Maybe Rotating)
                           , inputDataQuickFall :: EventW ()
                           , inputDataSlowFall :: Bool
                           , inputDataHold :: EventW ()
                           } deriving (Show, Eq)

defInputData :: InputData
defInputData = InputData (EventW $ NoEvent) (EventW . Event $ Nothing) (EventW $ NoEvent) True (EventW $ NoEvent)

keyState :: (MonadIO m) => GLFW.Key -> ReaderW m GLFW.KeyState
keyState k = do
  w <- ask
  liftIO $ GLFW.getKey w k

isKeyState :: (MonadIO m)   => GLFW.Key -> GLFW.KeyState -> ReaderW m Bool
isKeyState k st = keyState k >>= return . (== st)

isKeyStateWire :: GLFW.Key -> GLFW.KeyState -> InputW Bool
isKeyStateWire k st = mkGen_ $ \_ -> keyState k >>= return . Right . (== st)

makeWire :: (MonadIO m) => GLFW.Window -> Wire s () m () InputData
makeWire w = mapWire (\r -> runReaderT r w) inputWire

inputWire :: InputW InputData
inputWire = proc _ -> do
  idM <- movingWire -< ()
  idR <- rotatingWire -< ()
  idQF <- quickFallWire -< ()
  idSF <- slowFallWire -< ()
  idH <- holdWire -< ()
  returnA -< InputData idM idR idQF idSF idH

movingWire :: InputW (EventW (Maybe Moving))
movingWire = proc _ -> do
  mm <- io -< ()
  returnA -< EventW . maybe NoEvent (const . Event $ mm) $ mm
    where
      io :: InputW (Maybe Moving)
      io = mkGen_ $ \_ -> do
        lSt <- (&&) <$> isKeyState GLFW.Key'Left GLFW.KeyState'Pressed <*> isKeyState GLFW.Key'Up GLFW.KeyState'Released
        rSt <- (&&) <$> isKeyState GLFW.Key'Right GLFW.KeyState'Pressed <*> isKeyState GLFW.Key'Up GLFW.KeyState'Released
        return . Right $ case (lSt, rSt) of
          (True, True) -> Nothing
          (True, _) -> Just ML
          (_, True) -> Just MR
          _ -> Nothing


slowFallWire :: InputW Bool
slowFallWire = isKeyStateWire GLFW.Key'Down GLFW.KeyState'Released

quickFallWire :: InputW (EventW ())
quickFallWire = proc _ -> do
  st <- slowFallWire -< ()
  returnA -< EventW $ if not st then Event () else NoEvent

holdWire :: InputW (EventW ())
holdWire = proc _ -> do
  h <- io -< ()
  returnA -< EventW $ if h then Event () else NoEvent
    where
      io :: InputW Bool
      io = mkGen_ $ \_ -> ((.).(.)) Right (||) <$> isKeyState GLFW.Key'LeftShift GLFW.KeyState'Pressed <*> isKeyState GLFW.Key'RightShift GLFW.KeyState'Pressed

rotatingWire :: InputW (EventW (Maybe Rotating))
rotatingWire = proc _ -> do
  r <- io -< ()
  returnA -< EventW $ Event r
    where
      io :: InputW (Maybe Rotating)
      io = mkGen_ $ \_ -> do
        upSt <- isKeyState Key'Up KeyState'Pressed
        leftSt <- isKeyState Key'Left KeyState'Pressed
        rightSt <- isKeyState Key'Right KeyState'Pressed
        return . Right $ case (upSt, leftSt, rightSt) of
          (True, True, True) -> Nothing
          (True, True, _) -> Just CCW
          (True, _, True) -> Just CW
          _ -> Nothing
