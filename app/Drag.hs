{-# LANGUAGE RecursiveDo #-}

module Main where

import qualified Snap
import qualified Snappy

main :: IO ()
main = do
  element <- Snap.getElementById "app"
  snap <- Snap.new element
  rec c <-
        Snappy.circle
          snap
          (Snappy.draggable c 50 Snappy.dragDX)
          (Snappy.draggable c 45 Snappy.dragDY)
          (pure 20)
  pure ()
