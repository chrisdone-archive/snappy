{-# LANGUAGE RecursiveDo #-}

module Main where

import qualified Snap
import qualified Snappy

main :: IO ()
main = do
  element <- Snap.getElementById "app"
  snap <- Snap.new element
  rec button <-
        Snappy.text
          snap
          (pure 10)
          (pure 100)
          (Snappy.eventToDynamic
           "Click me!"
           (fmap
              snd
              (Snappy.scanEvent
                 (\(clicks,_) _ ->
                    (clicks + 1
                    ,case clicks of
                       0 -> "You clicked me!"
                       1 -> "Yep, that's how you click."
                       _ -> "Alright. I got it."))
                 (0 :: Int,"")
                 (Snappy.textClicked button))))
  pure ()
