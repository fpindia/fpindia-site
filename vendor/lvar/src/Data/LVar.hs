{-# LANGUAGE DeriveAnyClass #-}

-- | @LVar@ is like @Control.Concurrent.STM.TMVar@ but with a capability for
-- listening to its changes.
module Data.LVar
  ( -- * Types
    LVar,
    ListenerId,

    -- * Creating a LVar
    new,
    empty,

    -- * Modifying a LVar
    get,
    set,
    modify,

    -- * Listening to a LVar
    addListener,
    listenNext,
    removeListener,
  )
where

import Control.Exception (throw)
import qualified Data.Map.Strict as Map
import Prelude hiding (empty, get, modify)

-- A mutable variable (like @TMVar@), changes to which can be listened to from
-- multiple threads.
data LVar a = LVar
  { -- | A value that changes over time
    lvarCurrent :: TMVar a,
    -- | Subscribers listening on changes to the value
    lvarListeners :: TMVar (Map ListenerId (TMVar ()))
  }

type ListenerId = Int

-- | Create a new @LVar@ with the given initial value
new :: forall a m. MonadIO m => a -> m (LVar a)
new val = do
  LVar <$> newTMVarIO val <*> newTMVarIO mempty

-- | Like @new@, but there is no initial value. A @get@ will block until an
-- initial value is set using @set@ or @modify@
empty :: MonadIO m => m (LVar a)
empty =
  LVar <$> newEmptyTMVarIO <*> newTMVarIO mempty

-- | Get the value of the @LVar@
get :: MonadIO m => LVar a -> m a
get v =
  atomically $ readTMVar $ lvarCurrent v

-- | Set the @LVar@ value; active listeners are automatically notifed.
set :: MonadIO m => LVar a -> a -> m ()
set v val = do
  atomically $ do
    let var = lvarCurrent v
    isEmptyTMVar var >>= \case
      True -> putTMVar var val
      False -> void $ swapTMVar var val
    notifyListeners v

-- | Modify the @LVar@ value; active listeners are automatically notified.
modify :: MonadIO m => LVar a -> (a -> a) -> m ()
modify v f = do
  atomically $ do
    curr <- readTMVar (lvarCurrent v)
    void $ swapTMVar (lvarCurrent v) (f curr)
    notifyListeners v

notifyListeners :: LVar a -> STM ()
notifyListeners v' = do
  subs <- readTMVar $ lvarListeners v'
  forM_ (Map.elems subs) $ \subVar -> do
    tryPutTMVar subVar ()

data ListenerDead = ListenerDead
  deriving (Exception, Show)

-- | Create a listener for changes to the @LVar@, as they are set by @set@ or
-- @modify@ from this time onwards.
--
-- You must call @listenNext@ to get the next updated value (or current value if
-- there is one).
--
-- Returns a @ListenerId@ that can be used to stop listening later (via
-- @removeListener@)
addListener ::
  MonadIO m =>
  LVar a ->
  m ListenerId
addListener v = do
  atomically $ do
    subs <- readTMVar $ lvarListeners v
    let nextIdx = maybe 1 (succ . fst) $ Map.lookupMax subs
    notify <-
      tryReadTMVar (lvarCurrent v) >>= \case
        Nothing -> newEmptyTMVar
        -- As a value is already available, send that as first notification.
        --
        -- NOTE: Creating a TMVar that is "full" ensures that we send a current
        -- (which is not empty) value on @listenNext@).
        Just _ -> newTMVar ()
    void $ swapTMVar (lvarListeners v) $ Map.insert nextIdx notify subs
    pure nextIdx

-- | Listen for the next value update (since the last @listenNext@ or
-- @addListener@). Unless the @LVar@ was empty when @addListener@ was invoked,
-- the first invocation of @listenNext@ will return the current value even if
-- there wasn't an update.  Therefore, the *first* call to @listenNext@ will
-- *always* return immediately, unless the @LVar@ is empty.
--
-- Call this in a loop to listen on a series of updates.
--
-- Throws @ListenerDead@ if called with a @ListenerId@ that got already removed
-- by @removeListener@.
listenNext :: MonadIO m => LVar a -> ListenerId -> m a
listenNext v idx = do
  atomically $ do
    lookupListener v idx >>= \case
      Nothing ->
        -- FIXME: can we avoid this by design?
        throw ListenerDead
      Just listenVar -> do
        takeTMVar listenVar
        readTMVar (lvarCurrent v)
  where
    lookupListener :: LVar a -> ListenerId -> STM (Maybe (TMVar ()))
    lookupListener v' lId = do
      Map.lookup lId <$> readTMVar (lvarListeners v')

-- | Stop listening to the @LVar@
removeListener :: MonadIO m => LVar a -> ListenerId -> m ()
removeListener v lId = do
  atomically $ do
    subs <- readTMVar $ lvarListeners v
    whenJust (Map.lookup lId subs) $ \_sub -> do
      void $ swapTMVar (lvarListeners v) $ Map.delete lId subs
