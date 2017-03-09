{-# LANGUAGE RecursiveDo, LambdaCase #-}

module Main where

import qualified Snap
import qualified Snappy

main :: IO ()
main = do
  element <- Snap.getElementById "app"
  snap <- Snap.new element
  canvas <-
    Snappy.rect snap (pure 0) (pure 0) (pure 800) (pure 600) (pure "#ffffff")
  let selectionDrag =
        Snappy.mapMaybeEvent snd
           (Snappy.scanEvent
              (\state@(mstart, _) event ->
                case event of
                  Snappy.DragStart pos -> (Just pos, Nothing)
                  Snappy.DragStop -> (Nothing, Just (Snappy.Rec 0 0 0 0))
                  Snappy.Dragging drag ->
                    case mstart of
                      Nothing -> state
                      Just start -> (Just start, Just (rectangle start drag)))
              (Nothing, Nothing)
              (Snappy.rectDrag canvas))
        where rectangle (Snappy.Pos x y) (Snappy.Drag dx dy) =
                Snappy.Rec
                { Snappy.recX = if dx < 0 then x + dx else x
                , Snappy.recY = if dy < 0 then y + dy else y
                , Snappy.recW = if dx < 0 then -dx else dx
                , Snappy.recH = if dy < 0 then -dy else dy
                }
  _ <-
    Snappy.rect
      snap
      (Snappy.eventToDynamic 0 (fmap Snappy.recX selectionDrag))
      (Snappy.eventToDynamic 0 (fmap Snappy.recY selectionDrag))
      (Snappy.eventToDynamic 0 (fmap Snappy.recW selectionDrag))
      (Snappy.eventToDynamic 0 (fmap Snappy.recH selectionDrag))
      (pure "#1C90F3")
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
              (Snappy.scanEvent
                 (\(clicks,_) _ ->
                    (clicks + 1
                    ,case clicks of
                       0 -> "You clicked me!"
                       1 -> "Yep, that's how you click."
                       _ -> "Alright. I got it."))
                 (0 :: Int,"")
                 (Snappy.textClicked button))))
  Snappy.bindEvent (Snappy.textClicked button)
                   (\_ -> putStrLn "Clicked!")
