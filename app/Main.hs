{-# LANGUAGE RecursiveDo #-}

module Main where

import qualified Snap
import qualified Snappy

main = do
  element <- Snap.getElementById "app"
  snap <- Snap.new element
  rec c <-
        Snappy.circle
          snap
          (Snappy.draggable c 50 Snappy.dragDX)
          (Snappy.draggable c 45 Snappy.dragDY)
          (pure 20)
  _ <-
    Snappy.text
      snap
      (pure 10)
      (pure 20)
      (Snappy.zipDynamics
         (\x y -> "(x,y) = " ++ show (x,y))
         (Snappy.circleX c)
         (Snappy.circleY c))
  rec button <-
        Snappy.text
          snap
          (pure 10)
          (pure 100)
          (Snappy.eventToDynamic
           "Click me!"
           (fmap
              snd
              (Snappy.foldEvent
                 (\(clicks,_) _ ->
                    (clicks + 1
                    ,case clicks of
                       0 -> "You clicked me!"
                       1 -> "Yep, that's how you click."
                       _ -> "Alright. I got it."))
                 (0 :: Int,"")
                 (Snappy.textClicked button))))
  return ()
