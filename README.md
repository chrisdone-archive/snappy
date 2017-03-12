# snappy

Go here for demonstrations: http://chrisdone.com/toys/

Source for demonstrations: https://github.com/chrisdone/snappy/tree/master/app

## Drag the ball around and see the text updated live

![](http://i.imgur.com/1hVKxZ6.gif)

```haskell
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
```

## Click events and state

![](http://i.imgur.com/NqtDLa1.gif)

```haskell
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
```
