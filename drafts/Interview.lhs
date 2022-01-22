---
title: continuing the coding interview (random experiments for post)
published: 2021-06-27
---
\begin{code}
{-# LANGUAGE BlockArguments
  , LambdaCase
  , InstanceSigs
  , PostfixOperators
  , RankNTypes
  , ScopedTypeVariables
  , AllowAmbiguousTypes
  , TupleSections
  , NoImplicitPrelude
  , MultiParamTypeClasses
  , FunctionalDependencies
  , KindSignatures
  , TypeFamilies
  , PolyKinds
  , FlexibleContexts
  , UndecidableInstances
  , DataKinds
  , TypeOperators
  , ImplicitParams
  , FlexibleInstances#-}
module Interview where

import Prelude hiding (null, (.), (+))
import Data.Functor
import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Fail
import Control.Monad
import Debug.Trace
import Data.IORef
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits
import Data.Proxy
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.Array.IO

\end{code}
--newtype ContT r m a = ContT {runContT :: (a -> m r) -> m r} 

--(.<) :: (b -> c) -> (a -> b) -> (a -> c)
--f .< g = \x -> f (g x)

--instance Functor (ContT r m) where
--    fmap f c = ContT $ \k -> runContT c (k .< f)

--instance Applicative (ContT r m) where
--    pure x = ContT ($ x)
--    f <*> x = ContT \k -> runContT f \f' -> runContT x (k .< f')

--instance Monad (ContT r m) where
--    return = pure
--    x >>= f = ContT \k -> runContT x $ \a -> runContT (f a) k

--instance (Alternative m) => Alternative (ContT r m) where
--    empty = ContT (\_ -> empty)
--    a <|> b = ContT $ liftA2 (<|>) (runContT a) (runContT b)

--instance MonadTrans (ContT r) where
--    lift :: Monad m => m a -> ContT r m a
--    lift m = ContT (m >>=)

--instance MonadIO m => MonadIO (ContT r m) where
--    liftIO :: IO a -> ContT r m a
--    liftIO = lift .< liftIO 

--instance MonadFail m => MonadFail (ContT r m) where
--    fail :: String -> ContT r m a
--    fail = ContT .< const .< fail

--mapContT :: (m r -> m r) -> ContT r m a -> ContT r m a
--mapContT f c = ContT \k -> runContT c (f .< k) 

--todo = undefined

--withContT :: ((b -> m r) -> a -> m r) -> ContT r m a -> ContT r m b
--withContT f c = ContT (runContT c .< f)

--evalContT :: Applicative m => ContT r m r -> m r
--evalContT c = runContT c pure

--callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
--callCC f = ContT \k -> 
--  let z = f (\x -> ContT \_ -> k x)
--  in 
--  runContT z k

--shiftT :: Monad m => ((a -> m r) -> ContT r m r) -> ContT r m a
--shiftT f = ContT $ evalContT .< f

--resetT :: Monad m => ContT r m r -> ContT r' m r
--resetT = lift .< evalContT
 
--liftLocal :: Monad m => m r' -> ((r' -> r') -> m r -> m r) -> (r' -> r') -> ContT r m a -> ContT r m a
--liftLocal ask local rr c = ContT \k -> do
--    r <- ask
--    local rr $ runContT c (local (const r) .< k)

--newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

--instance Functor f => Functor (ReaderT r f) where
--    fmap f r = ReaderT (fmap f .< runReaderT r)

--instance Applicative f => Applicative (ReaderT r f) where
--    pure x = ReaderT (const (pure x))
--    f <*> x = ReaderT (\r -> runReaderT f r <*> runReaderT x r)

--instance Monad m => Monad (ReaderT r m) where
--    x >>= f = ReaderT \r -> do
--        a <- runReaderT x r
--        runReaderT (f a) r

--ask :: Monad m => ReaderT r m r
--ask = ReaderT pure

--asks :: Applicative m => (a1 -> a2) -> ReaderT a1 m a2
--asks f = ReaderT (pure .< f)

--local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
--local f r = ReaderT $ runReaderT r .< f

--data DerefCmd a = Read | Write a

--newtype ArrPtr m a = ArrPtr {getPtr :: DerefCmd a -> m a}
    
--class DerefMut m a b c where
--    poke :: a -> b -> m c
--    peek :: a -> m b

--instance MonadIO m => DerefMut m (IORef a) a () where
--    peek = liftIO .< readIORef 
--    poke r = liftIO .<  writeIORef r

--instance DerefMut m (ArrPtr m a) a a where
--    peek = flip getPtr Read
--    poke x a = getPtr x (Write a)

--class MonadIO m => Deref s m where
--    type family Target s
--    deref :: MonadIO m => s -> m (Target s) 

--instance MonadIO m => Deref [a] m where
--     type Target [a] = [a]
--     deref = pure

--instance (Deref s m) => Deref (IORef s) m where
--    type Target (IORef s) = Target s
--    deref s = do
--        r <- liftIO $ readIORef s
--        deref r

--instance (Deref s m) => Deref (ContT s m s) m where
--    type Target (ContT s m s) = Target s
--    deref cs = evalContT cs >>= deref

--foreach :: forall k s (r :: k) (m :: k -> *) (t :: * -> *) u a b.
--  (Deref s (ContT r m), MonadIO (ContT r m), Foldable t,
--   Target s ~ t u) =>
--     s
--     -> (ContT r m a -> ContT r m b -> u -> ContT r m ())
--     -> ContT r m ()
--foreach xs f = do
--    xs' <- deref xs
--    callCC \break -> 
--      forM_ xs' \x -> 
--        callCC \continue -> f (break ()) (continue ()) x

--type Imperative r a = ContT r IO a

--(=:) :: IORef a -> a -> Imperative r ()
--ref =: a = liftIO $ writeIORef ref a

--(.) :: a -> (a -> b) -> b -- IORef a -> (IORef a -> Imperative r b) -> Imperative r b
--r . f = f r
--infixl 9 .

--set :: (Eq k, Hashable k) => (k, v) -> IORef (M.HashMap k v) -> Imperative r ()
--set (k, v) r = liftIO $ modifyIORef' r (M.insert k v)
--nil = False
--null = error "Null pointer dereference!"

--get :: (Eq k, Hashable k) => k -> IORef (M.HashMap k v) -> Imperative r (v, Bool)
--get k r = verbose .< M.lookup k <$> liftIO (readIORef r)

--verbose :: Maybe a -> (a, Bool)
--verbose = maybe (null, True) (, False)

--begin :: forall k a (r :: k) (m :: k -> *) b.
--  ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
--begin = callCC

--end :: ContT r m r
--end = pure null


--f ?: x = f x
--infixr 9 ?:

--if' :: Bool -> ContT a IO () -> ContT a IO ()
--if' = when

--println :: (MonadIO m, Show a) => a -> m ()
--println = liftIO .< print

--enumerate :: (Num a, Enum a, MonadIO f) => p -> IORef [b] -> f [(a, b)]
--enumerate _ xs = zip [0..] <$> liftIO (readIORef xs)

----new :: (a -> b) -> a -> Imperative r (IORef b)
----new f = liftIO <| newIORef <| f

---- new :: Default a b => (c -> a) -> c -> Imperative r (IORef b) 
---- new f = liftIO <| newIORef <| into <| f

--class Default a b where
--    into :: a -> b
--    new :: MonadIO m => (c -> a) -> c -> m (IORef b)
--    new f = liftIO .< newIORef .< into .< f
--    from :: MonadIO m => c -> (c -> a) -> m (IORef b)
--    from = flip new

--instance Default m m where
--    into = id
 

--instance Monoid m => Default () m where
--     into = const mempty
 
--newtype HashMap a = HashMap a

--newtype List a = List a

--newtype Str a = Str a

--newtype Array a = Array a

--instance Default a (M.HashMap k v) => Default (HashMap a) (M.HashMap k v) where
--    into (HashMap h) = into h

--instance Default a [b] => Default (List a) [b] where
--    into (List a) = into a

--instance Default a [Char] => Default (Str a) [Char] where
--    into (Str a) = into a

--array :: MArray IOUArray a IO => [a] 
--    -> Imperative r ([Int] -> ArrPtr (ContT r IO) a)
--array xs = do
--    arr :: IOUArray Int a <- liftIO $ newListArray (0, length xs) xs
--    let getter :: Int -> DerefCmd a -> Imperative r a
--        getter i Read = liftIO $ readArray arr i
--        getter i (Write x) = liftIO $ writeArray arr i x $> x
--    pure (\[i] -> ArrPtr (getter i))

--arrTest = do
--    arr <- array[1..10]
--    p <- arr[2] Read
--    pure arr
--using :: (t1 -> t2) -> t1 -> t2
--using f x = f x    
--infixr 9 `using`

--(===) :: Eq a => a -> a -> Bool
--(===) = (==)

--(+) :: Semigroup a => a -> a -> a
--(+) = (<>)
--infixl 0 +

--toString :: Show a => () -> a -> String
--toString _ = show

--twoSum :: Int -> [Int] -> Imperative (Int, Int) (Int, Int)
--                                -- return :: (Int, Int) -> ContT (Int, Int) IO ()
--twoSum target nums = begin `using` \return -> do {
--    -- IORef (HashMap Int Int)
--    seen <- new HashMap();
--    -- IORef [Int]
--    _xs:: IORef [Int] <- List .from(nums);
--    xs <- new List(nums);
--    -- [(Int, Int)]
--    indices <- xs.enumerate();
--    foreach indices `using` \break continue (i, x) -> do {
--        -- (Int | ⊥, Bool), i.e. Maybe with the side effect of crashing on Nothing
--        (res, err) <- seen.get(target - x);
--        -- when
--        if' (err === nil)?:
--            return (res, i);
--        seen.set(x, i);

--        println("testing " + x.toString() + " at index " + i.toString());
--    };
--    -- without a proper return, throw a null ptr error
--    return (0, 0);
--    end
--}

---- embed a language that encapsulates transition sttates for dfa
---- e<|g<| given a certain amount of states, encode typeclass transformations between each state

--\end{code}
--this is something