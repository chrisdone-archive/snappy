{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | A reactive Snap SVG interface.

module Snappy where

import           Control.Concurrent
import           Control.Monad
import           Data.IORef
import qualified Snap
import           System.IO.Unsafe

--------------------------------------------------------------------------------
-- Event type

data Event a = forall origin s. Event
  { eventSubscribers :: IORef [origin -> IO ()]
  , eventFromOrigin :: s -> origin -> (a,s)
  , eventState :: s
  }

instance Functor Event where
  fmap f (Event subscribers fromOrigin state) =
    Event
      subscribers
      (\s origin ->
         let (a', s') = fromOrigin s origin
         in (f a', s'))
      state
  {-# INLINE fmap #-}

-- | Zip events that occur in tandem within the given time frame.
zipEvents :: (a -> b -> c) -> Event a -> Event b -> Event c
zipEvents f e1 e2 =
  unsafePerformIO
    (do subscribersRef <- newIORef mempty
        tvar1 <- newEmptyMVar
        tvar2 <- newEmptyMVar
        let !ev = Event subscribersRef (\s (a, b) -> (f a b, s)) ()
        let listen e us them pair =
              bindEvent
                e
                (\a -> do
                   result <- tryTakeMVar them
                   case result of
                     Nothing -> putMVar us a
                     Just b -> do
                       subscribers <- readIORef subscribersRef
                       mapM_ ($ (pair a b)) subscribers)
        listen e1 tvar1 tvar2 (\a b -> (a, b))
        listen e2 tvar2 tvar1 (\a b -> (b, a))
        pure ev)
{-# NOINLINE zipEvents #-}

foldEvent :: (s -> a -> s) -> s -> Event a -> Event s
foldEvent f nil (Event subscribers fromOrigin oldState) =
  Event
    subscribers
    (\s origin ->
       let (a, _) = fromOrigin oldState origin
           s' = f s a
       in (s', s'))
    nil
{-# INLINE foldEvent #-}

bindEvent :: Event a -> (a -> IO ()) -> IO ()
bindEvent Event {..} m = do
  do stateRef <- newIORef eventState
     modifyIORef
       eventSubscribers
       (++ [ \v -> do
               s <- readIORef stateRef
               let (v', s') = eventFromOrigin s v
               m v'
               writeIORef stateRef s'
               pure ()
           ])

--------------------------------------------------------------------------------
-- Dynamic type

data Dynamic a = Dynamic
  { dynDefault :: a
  , dynEvent :: Maybe (Event a)
  }

instance Functor Dynamic where
  fmap f (Dynamic def event) =
    Dynamic (f def) (fmap (fmap f) event)
  {-# INLINE fmap #-}

instance Applicative Dynamic where
  pure a = Dynamic {dynDefault = a, dynEvent = Nothing}
  {-# INLINE pure #-}
  f <*> a =
    Dynamic
    { dynDefault = dynDefault f (dynDefault a)
    , dynEvent =
        case (dynEvent f, dynEvent a) of
          (Nothing, Nothing) -> Nothing
          (Just fevent, Nothing) -> Just (fmap ($ dynDefault a) fevent)
          (Nothing, Just aevent) -> Just (fmap (dynDefault f) aevent)
          (Just fevent, Just aevent) -> Just (zipEvents ($) fevent aevent)
    }
  {-# INLINE (<*>) #-}

zipDynamics
  :: (a -> b -> c)
  -> Snappy.Dynamic a
  -> Snappy.Dynamic b
  -> Snappy.Dynamic c
zipDynamics f d1 d2 =
  Dynamic
  { dynDefault = f (dynDefault d1) (dynDefault d2)
  , dynEvent =
      case (dynEvent d1, dynEvent d2) of
        (Nothing, Nothing) -> Nothing
        (Just d1event, Nothing) -> Just (fmap (\a -> f a (dynDefault d2)) d1event)
        (Nothing, Just d2event) -> Just (fmap (\a -> f (dynDefault d1) a) d2event)
        (Just d1event, Just d2event) -> Just (zipEvents f d1event d2event)
  }

foldDynamic :: (s -> a -> s) -> s -> Event a -> Dynamic s
foldDynamic f nil e =
  Dynamic {dynDefault = nil
          ,dynEvent =  Just (foldEvent f nil e)}

bindDynamic :: Dynamic a -> (a -> IO ()) -> IO ()
bindDynamic (Dynamic _ event) m =
  maybe
    (return ())
    (\e ->
       void
         (forkIO
            (do yield
                bindEvent e m)))
    event

dynamicDef :: Dynamic a -> a
dynamicDef (Dynamic def _) = def

eventToDynamic :: a -> Event a -> Dynamic a
eventToDynamic d e = Dynamic {dynDefault = d, dynEvent = Just e}

--------------------------------------------------------------------------------
-- Drag event

data DragEvent = DragStart | Dragging Drag | DragStop

data Drag = Drag
  { dragDX :: Double
  , dragDY :: Double
  }

dragEvent :: Snap.HasDrag d => d -> IO (Event DragEvent)
dragEvent d = do
  subscribersRef <- newIORef mempty
  Snap.drag
    d
    (\dx dy -> do
       subscribers <- readIORef subscribersRef
       mapM_ (\subscriber -> subscriber (Dragging (Drag dx dy))) subscribers)
    (\_ _ -> do
       subscribers <- readIORef subscribersRef
       mapM_ (\subscriber -> subscriber DragStart) subscribers)
    (\_ -> do
       subscribers <- readIORef subscribersRef
       mapM_ (\subscriber -> subscriber DragStop) subscribers)
  st <- newIORef ()
  pure
    (Event
     { eventSubscribers = subscribersRef
     , eventFromOrigin = \s origin -> (origin, s)
     , eventState = st
     })

-- | A dynamic which makes a draggable thing move around upon drag at
-- the given axis.
draggable
  :: Circle
  -> Double
  -> (Drag -> Double)
  -> Dynamic Double
draggable c initial get =
  fmap
    snd
    (foldDynamic
       (\(origin, new) event ->
          case event of
            Dragging pos -> (origin, origin + get pos)
            _ -> (new, new))
       (initial, initial)
       (circleDrag c))

--------------------------------------------------------------------------------
-- Click event

data ClickEvent = ClickEvent
  { clickX :: !Double
  , clickY :: !Double
  , clickModifier :: !Snap.Modifier
  } deriving (Show)

clickEvent :: Snap.HasClick d => d -> IO (Event ClickEvent)
clickEvent d = do
  subscribersRef <- newIORef mempty
  Snap.singleClick
    d
    (\_event modifier x y -> do
       subscribers <- readIORef subscribersRef
       mapM_ (\subscriber -> subscriber (ClickEvent x y modifier)) subscribers)
  st <- newIORef ()
  pure
    (Event
     { eventSubscribers = subscribersRef
     , eventFromOrigin = \s origin -> (origin, s)
     , eventState = st
     })

--------------------------------------------------------------------------------
-- Circle object

data Circle = Circle
  { circleObject :: Snap.Circle
  , circleDrag :: Event DragEvent
  , circleX :: Dynamic Double
  , circleY :: Dynamic Double
  }

circle :: Snap.Snap -> Dynamic Double -> Dynamic Double -> Dynamic Double -> IO Circle
circle snap xdynamic ydynamic rdynamic = do
  let x = dynamicDef xdynamic
      y = dynamicDef ydynamic
  c <- Snap.circle snap x y (dynamicDef rdynamic)
  drag <- dragEvent c
  t <- Snap.newMatrix
  xLast <- newIORef 0
  bindDynamic
    xdynamic
    (\x' -> do
       undo <- readIORef xLast
       Snap.translate t (-undo + (x' - x)) 0
       Snap.transform c t
       writeIORef xLast (x' - x))
  yLast <- newIORef 0
  bindDynamic
    ydynamic
    (\y' -> do
       undo <- readIORef yLast
       Snap.translate t 0 (-undo + (y' - y))
       Snap.transform c t
       writeIORef yLast (y' - y))
  pure (Circle c drag xdynamic ydynamic)

--------------------------------------------------------------------------------
-- Text object

data Text = Text
  { textObject :: Snap.Text
  , textClicked :: Event ClickEvent
  }

text :: Snap.Snap -> Dynamic Double -> Dynamic Double -> Dynamic String -> IO Text
text snap xdynamic ydynamic tdynamic = do
  let x = dynamicDef xdynamic
      y = dynamicDef ydynamic
  c <- Snap.text snap x y (dynamicDef tdynamic)
  t <- Snap.newMatrix
  xLast <- newIORef 0
  bindDynamic
    xdynamic
    (\x' -> do
       undo <- readIORef xLast
       Snap.translate t (-undo + (x' - x)) 0
       Snap.transform c t
       writeIORef xLast (x' - x))
  yLast <- newIORef 0
  bindDynamic
    ydynamic
    (\y' -> do
       undo <- readIORef yLast
       Snap.translate t 0 (-undo + (y' - y))
       Snap.transform c t
       writeIORef yLast (y' - y))
  bindDynamic tdynamic (\t' -> Snap.setAttr c "#text" t')
  clickev <- clickEvent c
  pure (Text c clickev)
