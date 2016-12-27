module Main where

import Input ( InputData
             , makeWire
             , defInputData
             )

import Control.Wire hiding ( when
                           , unless
                           , (.)
                           , id
                           , loop
                           , now
                           )

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad ( unless
                     , when)

import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL.GL as GL

winHeight :: Int
winHeight = 420

winWidth :: Int
winWidth = 620

winTitle :: String
winTitle = "Netwire FRP Game"

fps :: Double
fps = 60

resize :: Int -> Int -> IO ()
resize w h = do
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  GL.ortho (-10) 410 610 (-10) 0 1
  GL.matrixMode $= GL.Modelview 0

quit :: GLFW.Window -> IO ()
quit w = do
  print "Terminating ..."
  GLFW.destroyWindow w
  GLFW.terminate

initialize :: IO ()
initialize = do
  print "Initializing ..."

main :: IO ()
main = do
  initialize
  _ <- GLFW.init
  Just w <- GLFW.createWindow winWidth winHeight winTitle Nothing Nothing
  GLFW.makeContextCurrent (Just w)
  GLFW.setWindowSizeCallback w $ Just (\_ -> resize)
  resize winWidth winHeight
  finally (loop w (makeWire w) clockSession_) (quit w)

    where
      loop :: GLFW.Window -> Wire s () IO () InputData -> Session IO s -> IO ()
      loop win wire session = do
        (ds, session') <- stepSession session
        (v, wire') <- stepWire wire ds (Right ())
        Just now <- GLFW.getTime
        when (v /= Right defInputData) $ do
          print v
        -- GL.clearColor $= GL.Color4 0 0 0 1
        -- GL.loadIdentity
        -- GLFW.swapBuffers w
        GLFW.pollEvents -- This comes really handy when it comes to checking recent events
        Just fpsLeft <- fmap ((recip fps) + now -) <$> GLFW.getTime
        esc <- (any id . fmap (== GLFW.KeyState'Pressed)) <$> (sequence . fmap (GLFW.getKey win)) [ GLFW.Key'Escape
                                                                                                  , GLFW.Key'Q
                                                                                                  ]
        unless (esc) $ do
          when (fpsLeft > 0) $
            threadDelay (truncate $ 1000000*fpsLeft)
          loop win wire' session'

