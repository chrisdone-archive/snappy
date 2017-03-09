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
      fromRect f = Snappy.eventToDynamic 0 (fmap f selectionDrag)
  _ <-
    Snappy.rect
      snap
      (fromRect Snappy.recX)
      (fromRect Snappy.recY)
      (fromRect Snappy.recW)
      (fromRect Snappy.recH)
      (pure "#1C90F3")
  pure ()
