# snappy

Go here for demonstrations: http://chrisdone.com/toys/

Source for demonstrations: https://github.com/chrisdone/snappy/tree/master/app

## Drag the ball around and see the text updated live

[](http://i.imgur.com/BfZZfDW.gif)

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
