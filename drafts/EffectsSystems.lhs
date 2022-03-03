---
title: a type cosmonaut's guide to effects systems
tags: programming, haskell
published: 2022-01-15
---

\ignore {
\begin{code}
{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, UndecidableInstances, TypeFamilies, GADTs, DerivingVia, GeneralisedNewtypeDeriving, BlockArguments, MultiParamTypeClasses , TypeOperators, DeriveFunctor, InstanceSigs, ScopedTypeVariables, DataKinds, PolyKinds, StandaloneDeriving, TypeApplications, UndecidableSuperClasses, LambdaCase, TupleSections #-}
{-# LANGUAGE PartialTypeSignatures, AllowAmbiguousTypes #-}
module EffectsSystems where
import Control.Monad
import Control.Monad.State hiding (MonadTrans)
import Data.Kind
import Control.Monad.Reader hiding (MonadTrans)
import Data.Foldable (find, traverse_)
import Data.Function
import Control.Exception
import Data.Functor
import Control.Monad.Except (MonadError (throwError, catchError), runExceptT, ExceptT (ExceptT))
import Text.Read (readMaybe)
import Data.Functor.Identity (Identity (Identity))
import Data.Coerce (coerce)
import Control.Monad.ST
import Data.STRef

\end{code}
}

I've been playing around with type-level ~~fuckery~~ programming lately, and something in my head finally clicked when it came to libraries like [freer-simple](https://hackage.haskell.org/package/freer-simple) and [polysemy](https://hackage.haskell.org/package/polysemy). For context, Haskell applications usually take one of a few approaches to managing complex contextual interactions, like unlifting everything to `ReaderT + IO`, monad transformer stacks, or algebraic effects systems. I'm partial to monad transformers for their composability, but there are a few issues that impact UX and extensibility. 

Let's say you had a simple CRUD app, that needs to be able to log errors, make database calls, and respond to HTTP requests.

You might define a typeclass like:

```haskell
class HasConnection m where
  getConnection :: m Connection

-- or if you're fancy:
-- class HasConnection env wheree
--   getConnection :: Lens' env Connection
```

and then write any DB-accessing code to be polymorphic over the surrounding context, as long as there's a way to access a database connection:

```haskell
selectFrom :: (HasConnection m, MonadIO m) => Table a -> m [a]
selectFrom table = do
  conn <- getConnection
  runBeamSqlite conn do
    rows <- runSelectReturningList (select ...)
    ...
  
getUserById 
  :: (HasConnection m, MonadIO m) 
  => Int -> m (Maybe User)
getUserById id = do
  users <- selectFrom userTable 
  pure $ find ((id ==) . userId) users
```

To run your app, you could then wrap up any required context into a product type and thread that data along implicitly using a `ReaderT` effect:

```haskell
-- class MonadReader r m | m -> r where
--   asks :: (r -> a) -> m a
--   ask :: m r
--   ask = asks id
--
-- newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}
-- instance Applicative m => MonadReader r (ReaderT r m) where 
--   asks f = ReaderT (pure . f)

data AppCtx = AppCtx 
  { _dbConn :: SomeConnection
  , _numThreads :: Int
  , _apiToken :: Text
  } 

instance MonadReader AppCtx m => HasConnection m where
  getConnection = asks _dbConn
```

And let's assume that there are some other ops in the meantime that can fail, so we want some way of exiting early. Cue `ExceptT` and `MonadError`:

```haskell
-- class MonadError err m where 
--   throwError :: forall a. e -> m a
--   catchError :: m a -> (e -> m a) -> m a 
-- newtype ExceptT e m a = ExceptT {runExceptT :: m (Either e a)}
server 
  :: (HasConnection m, MonadIO m, MonadError String m)
  => m ()
```

Monad transformers are meant to stack, so after creating your app context:

```haskell
connectDB :: Text -> IO (Either String SomeConnection)

mkAppCtx :: Text -> IO (Either String AppCtx)
mkAppCtx tok = do
  eitherConn <- connectDB "Data Source=:memory:"
  forM eitherConn \conn -> do
    threads <- getNumCapabilities
    pure $ AppCtx conn threads tok
```

It's a simple matter of instantiating the exact order of effects you want, then tearing those pieces down until you reach IO:

```haskell
-- ignoring other details because I'm lazy

runApp :: IO ()
runApp = do
  Right ctx <- mkAppCtx "some token"
  -- at this point, server is inferred to have the type
  -- ExceptT String (ReaderT AppCtx IO) ()
  serverRes <- runExceptT $ runReaderT server ctx
  either putStrLn pure serverRes 
```

You could also flip this stack around since `mkAppCtx` returns an `IO (Either ..)` value:

```haskell
runApp' :: IO (Either String ())
runApp' = runExceptT do
  ctx <- ExceptT $ mkAppCtx "some token"
  -- server :: ReaderT AppCtx (ExceptT String IO) ()
  runReaderT server ctx
```

Semantically and representationally, `server` in both cases is the same, even though their (wrapped) types differ.

Expanding `server` from both examples:

```haskell
server1 :: ExceptT String (ReaderT AppCtx IO) ()
 = ReaderT AppCtx IO (Either String ()) 
 = AppCtx -> IO (Either String ())

server2 :: ReaderT AppCtx (ExceptT String IO) ()
 = ReaderT AppCtx (IO (Either String ()))
 = (AppCtx -> IO (Either String ()))
```

The problem is that not every permutation of a series of monad transformers is equivalent. Let's say you had some stateful effect that could throw an error, like running a block cipher, parsing, etc:

\begin{code}
-- newtype StateT s m a 
--  = StateT {runStateT :: s -> m (a, s)}
-- evalStateT :: StateT s m a -> m a
-- execStateT :: StateT s m a -> m s
-- class MonadState s m where
--   get :: m s
--   put :: s -> m ()
--   modify :: (s -> s) -> m ()
newtype SomeState = SomeState Int deriving newtype (Eq, Show)

someStateOp :: (MonadState SomeState m, MonadIO m, MonadError String m) => m ()
someStateOp = do
  SomeState cur <- get
  when (cur == 69) $ throwError "not cool" 
  liftIO $ print cur
  put (SomeState $ cur + 1)
\end{code}

We could run this in two ways: layer the `ExceptT` on top of the `StateT`, or the other way around.

\begin{code}
res1 :: IO (Either String ((), SomeState))
res1 = runExceptT (runStateT someStateOp (SomeState 0))

res2 :: IO (Either String (), SomeState)
res2 = runStateT (runExceptT someStateOp) (SomeState 0)
\end{code}

What the hell?? Flipping the order of the two effects results in two different types, and differing semantics altogether. If `StateT` is on the outside, we run the risk of losing our state entirely if an error is thrown. 

Expanded:
```haskell
stateOp1 :: StateT SomeState (ExceptT String IO) ()
  = SomeState -> ExceptT String IO ((), SomeState)
  = SomeState -> IO (Either String ((), SomeState)) 

stateOp2 :: ExceptT String (StateT SomeState IO) ()
  = StateT SomeState IO (Either String ())
  = SomeState -> IO (Either String (), SomeState)
```

There's also the issue of extensibility - `mtl`-style transformer stacks are implemented using a type class for each effect (`MonadState`, `MonadError`, `MonadCont`, etc.). Monad transformers are parameterized by the ability to `lift` an operation from a lower monad into a higher one, and polymorphism is obtained by creating instances for each transformer, for each effect. If you have a `StateT s` wrapper somewhere in your stack, then the stack implements `MonadState s` at that level. A `ReaderT env` on top of that `StateT` would provide `MonadReader env`, as well as `MonadState s` through an instance on `ReaderT env`. Here's an example:

```haskell
-- class MonadTrans (t :: (Type -> Type) -> Type -> Type) where
--   lift :: m a -> t m a

newtype IdT m a = IdT {getIdT :: m a} 
  deriving newtype (Functor, Applicative, Monad)

instance MonadTrans IdT where
  lift = IdT

instance MonadState s m => MonadState s (IdT m) where
  get = lift get
  put = lift . put

instance MonadError e m => MonadError e (IdT m) where
  throwError = lift . throwError
  catchError (IdT act) k = lift $ catchError act (getIdT . k)

-- and so on
```

It's pretty easy to see that this is a ton of boilerplate, and introducing a new effect means writing another row of (pretty trivial) instances. Work on the order of `n^2` is pretty bad. This is worse when you add `MonadIO` into the mix, requiring another set of identical `liftIO = lift . liftIO` instance bodies.

<hr>
<h2>introducing extensible effects, operationally</h2>

So what exactly is an "effect"? It doesn't really make sense for a purely functional language to modify the surrounding world, destructively update global variables, etc.

I like to think of effects more as passing around/manipulating contexts - in the monad transformer approach, every "layer" of your stack carries some semantic information:

\begin{code}
-- I have a monadic action that needs an environment `r`
newtype ReaderT' r m a = ReaderT' (r -> m a)

-- I have a monadic action that affects some implicit state `s`, so gimme one
newtype StateT' s m a = StateT' (s -> m (a, s))

-- I have an action that might fail with an error `e`
newtype ExceptT' e m a = ExceptT' (m (Either e a))

-- I have an `a`, gimme something to do with it
newtype ContT' r m a = ContT' ((a -> m r) -> m r)
\end{code}

and the *composition* of those layers creates a stack of contexts that interact generically:

\begin{code}
readerExcState :: 
  ReaderT' String 
    (ExceptT' String 
      (StateT' Int Identity)) 
  ()
readerExcState = 
    ReaderT' \env -> 
      ExceptT' $ 
        StateT' \st -> Identity 
          (Left ("thanks for the string: " <> env), st + 1)

-- these definitions are isomorphic
flattenedReadExcSt :: String -> Int -> (Either String (), Int)
flattenedReadExcSt = coerce readerExcState
\end{code}

The idea behind extensible effects is a sort of inversion of control - while monad transformers capture this idea of "here's a context, and a set of operations you can do on that context", effects libraries expand that to "here's a set of operations I want to do, figure out a context for that and how to achieve them". We can achieve a first iteration using an [Operational](https://themonadreader.files.wordpress.com/2010/01/issue15.pdf#page=37&zoom=auto,-13,725) monad, transforming things into a more inspectable form.

Let's say you wanted to encode a stateful operation. Instead of using a black-box function from a state `s` to a result and new state `(a, s)`, you can distill your intention down to two commands:

\begin{code}
data StateCmd s a where
  Get :: StateCmd s s
  Put :: s -> StateCmd s ()
\end{code}

and decompose the idea of monad into its operations:

\begin{code}
data Prog cmd a where
  Pure :: a -> Prog cmd a
  Bind :: Prog cmd a -> (a -> Prog cmd b) -> Prog cmd b
  Command :: cmd a -> Prog cmd a

instance Functor (Prog cmd) where
  -- every Monad is a Functor because
  -- fmap f ma = ma >>= (pure . f)
  fmap f prog = Bind prog (Pure . f)

instance Applicative (Prog cmd) where
  pure :: a -> Prog cmd a
  pure = Pure
  (<*>) :: Prog cmd (a -> b) -> Prog cmd a -> Prog cmd b
  -- literally the definition of `ap`
  progab <*> proga = Bind progab 
    \ab -> Bind proga 
      \a -> Pure (ab a)

instance Monad (Prog cmd) where
  (>>=) :: Prog cmd a -> (a -> Prog cmd b) -> Prog cmd b
  (>>=) = Bind

-- a monolithic function that you can't introspect
stateFunc :: State Int Int
stateFunc = do
  curState <- get
  put $ if even curState then curState `div` 2 else curState - 1
  pure (curState * 3)

-- an AST
stateProg :: Prog (StateCmd Int) Int
stateProg = do
  curState <- Command Get
  Command $ Put (if even curState then curState `div` 2 else curState - 1)
  Pure (curState * 3)
\end{code}

With your defunctionalized monad, you're now free to interpret that AST into whatever monad you wanted. If you want to emulate the regular `State` monad, you could just pass around a state manually:

\begin{code}
interpretState :: Prog (StateCmd s) a -> s -> (a, s) 
interpretState (Pure a) s = (a, s)
interpretState (Bind prog next) s = 
  let (a, s') = interpretState prog s
      nextProg = next a
   in interpretState nextProg s'
-- the GADT definitions give us witnesses for the type of `a`
interpretState (Command Get) s = (s, s)
interpretState (Command (Put newState)) _ = ((), newState)
\end{code}

Or you could interpret it using a mutable reference in the `ST` monad, which can be much [faster due to lower GC pressure](https://www.reddit.com/r/haskell/comments/1rcc8t/performance_of_the_st_monad_over_the_state_monad/). On a side note, there's a strong similarity here to a simple `ReaderT (STRef st s) (ST st) a`. Instead of threading immutable values, we use an immutable reference to one mutable variable:

\begin{code}
interpretST :: forall a s. Prog (StateCmd s) a -> s -> (a, s)
interpretST prog initial = runST foldProg where 
  -- we need explicit type signatures because of runST's Rank 2 type
  foldProg :: forall st. ST st (a, s)
  foldProg = do
    stateRef <- newSTRef initial
    let loop :: Prog (StateCmd s) b -> ST st b
        loop = \case
          Pure a -> pure a
          Bind cur next -> do
            output <- loop cur
            loop (next output)
          Command Get -> readSTRef stateRef
          Command (Put new) -> writeSTRef stateRef new
    res <- loop prog
    finalState <- readSTRef stateRef
    pure (res, finalState)
\end{code}

This approach works just fine, but if we want to compete with monad transformers, we need a way to express *multiple* effects.

<hr>
<h3>decomposing more than a single type</h3>

There's been a ton of research/functional pearls published on the use of a combination of type-indexed functors and free monads to write expressive code with minimal boilerplate. My first intro to the topic was [Data types a la carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf) by Wouter Sweirstra - he explores the use of fixed-points over functors along with typed unions to create syntax trees that can be interpreted generically, introspected, and open for extension as needed.


The gist of the paper is that you can define a simple coproduct (aka a disjoint union) of two functors:

\begin{code}
-- compare with Either (f a) (g a)
-- the inhabitants of (f :+: g) a = count(f a) + count(g a)
data (f :+: g) a = InL (f a) | InR (g a) deriving (Functor, Eq, Show)
infixr 7 :+:
\end{code}

Along with a fixed point over functors:

\begin{code}
-- try expressing this in Rust if you hate your computer
newtype Fix f = Fix (f (Fix f)) 
\end{code}

to create what's essentially a list of nested contexts. This list can be folded into a summary value using an [F-algebra](https://www.schoolofhaskell.com/user/bartosz/understanding-algebras), much in the same way as regular lists:

\begin{code}
foldExpr :: Functor f => (f a -> a) -> Fix f -> a
foldExpr fold (Fix f) = fold (foldExpr fold <$> f)
\end{code}

Here's some [further](https://bartoszmilewski.com/2020/04/09/initial-algebra-as-directed-colimit/) [reading](https://bartoszmilewski.com/2017/02/28/f-algebras/) from epic-polish-category-theory-man if you're interested.

Now I bet you're thinking, "how does this even terminate???", and you're right if `f` is instantiated to some functor like `Identity`. The trick is to use functors with a phantom type as your base case (forming the leaves of the expression):

\begin{code}
newtype Val a = Val Int 
  deriving newtype (Eq, Show, Num)
  deriving stock Functor
\end{code}

And the nodes:

\begin{code}
data Add a = Add a a deriving (Eq, Show, Functor)
\end{code}

Now the coproduct of these two types is:

\begin{code}
type ValOrAdd = (Val :+: Add)
\end{code}

This type is isomorphic to what we'd usually reach for in these situations, a simple unified sum type:

\begin{code}
data AST = ASTVal Int | ASTAdd AST AST
\end{code}

Notice that we're basically just defining extensible, open sum types instead of putting everything in one place.

If we want to add another operation, we can burrito up another layer inductively since `(:+:)` is itself a functor:

\begin{code}
data Mul a = Mul a a deriving (Eq, Show, Functor)

type ValOrAddOrMul = (Val :+: Add :+: Mul)
\end{code}

Similar to our monad transformer stacks, the order of our effects is just the order of our coproduct. The difference is that we've centralized things to revolve around compositions of `:+:`, instead of treating each node as a distinct type with its own effect AND implementation.

For clarity, we know that given a coproduct `f :+: g`, we can always inject an `f` into that coproduct, but can't always get an `f` back out.

\begin{code}

class (Functor sub, Functor sup) => sub :<: sup where
  -- inject
  inj :: sub a -> sup a
  -- project
  prj :: sup a -> Maybe (sub a)

-- (:<:) is basically a `Prism` over coproducts,
-- you can always lift and sometimes unlift
-- coprod :: forall sub sup a. (Functor sub, Functor sup, sub :<: sup) => Prism' (sup a) (sub a)
-- coprod = prism inj \sup -> maybe (Left sup) Right (prj sup)

-- compare with `lift` from `MonadTrans`
instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = InL
  prj (InL fa) = Just fa
  prj (InR ga) = Nothing

\end{code}

We can inject a functor into an singleton coproduct containing only itself, and always get it back out:

\begin{code}
-- reflexivity
instance (Functor f) => f :<: f where
  inj = id
  prj = Just
\end{code}

And define these injections on arbitrarily nested coproducts, given that the functor we want to inject exists somewhere:

\begin{code}
-- induction on a list of functors: `f` isn't at the front, but exists in the rest of the list
instance {-# OVERLAPPABLE #-} 
  ( Functor f
  , Functor head
  , Functor tail
  , f :<: tail
  ) => f :<: (head :+: tail) where
  -- compare with `lift . lift`
  inj = InR . inj
  prj (InL _ ) = Nothing
  prj (InR ta) = prj ta
\end{code}

Within a chain `f :+: g :+: h :+: ...`, there's only ever a single value. We're defining the equivalent of `union`s in other languages, but with some more structure in terms of type order.

\begin{code}
-- called `inject` in the original paper
liftFix :: (g :<: f) => g (Fix f) -> Fix f
liftFix = Fix . inj

-- Val (Fix f) is our base case, since it's a phantom type
val :: Val :<: f => Int -> Fix f
val = liftFix . Val

-- a binary AST node
add :: Add :<: f => Fix f -> Fix f -> Fix f
add l r = liftFix (Add l r)
\end{code}

Now, we can use type classes to implement our effects in a manner that only requires `n` instances. That's a whole order of magnitude less, wew:

\begin{code}
class Functor f => Eval f where
  evalAlg :: f Int -> Int

instance Eval Val where
  evalAlg (Val n) = n

instance Eval Add where
  evalAlg (Add l r) = l + r

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlg (InL f) = evalAlg f 
  evalAlg (InR g) = evalAlg g
\end{code}

And now, to fold any arbitrary arithmetic tree:

\begin{code}
evalFix :: Eval f => Fix f -> Int
evalFix = foldExpr evalAlg

ten :: Fix (Add :+: Val)
ten = val 1 `add` val 2 `add` val 3 `add` val 4

-- >>> evalFix ten
-- 10
\end{code}

Yes, this is just [Hutton's Razor](http://www.cs.nott.ac.uk/~pszgmh/exceptions.pdf) with extra steps. A lot of extra steps. But consider a more complicated case where we had more constructors to deal with, and more operations. Adding another constructor would affect every single pattern match site in your code, and lead to a refactoring annoyance, if not a nightmare for larger projects.

In contrast, let's implement multiplication now:

\begin{code}
instance Eval Mul where
  evalAlg (Mul l r) = l * r

mul :: Mul :<: f => Fix f -> Fix f -> Fix f
mul l r = liftFix (Mul l r)

nice :: Fix (Mul :+: Add :+: Val)
nice = (val 24 `add` val 18) `mul` (val 5 `add` val 1 `add` val 4)

-- >>> evalFix nice
-- 420
\end{code}

Any point in your codebase that uses only `Add` or `Val` will be untouched, and you're free to keep hacking away knowing that you haven't left any unhandled cases in your wake.

<hr>

<h2>something something free lunch</h2>

Now that we've written the basis for an extensible, low-boilerplate effects system, it's time to scrap everything and do something cooler.

You might have noticed that `Fix` is just one half of another type that's gotten a lot of attention lately, the `Free` monad:

```haskell
data Free f a
  = Pure a
  | Free (f (Free f a))
  deriving Functor
```

which you can think of as `Fix f` plus a terminating value, solving the problem of types like `Fix Identity` being truly infinite.

I won't go too deeply into free monads since there are [plenty](https://www.tweag.io/blog/2018-02-05-free-monads/) of [resources](https://iohk.io/en/blog/posts/2018/08/07/from-free-algebras-to-free-monads/) [already](http://comonad.com/reader/2008/monads-for-free/) on the topic. The gist of it is that the name `Free` is pretty literal; you get a `Monad` for free from any `Functor`.

```haskell
instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  Pure a >>= f = f a
  Free fa >>= f = Free ((>>= f) <$> fa)
```

This is the traditional representation, and some of its flaws are pretty apparent from the implementation: left-associative `(>>=)`'s require you to traverse down the entire stack, pissing off both the garbage collector and your QA team. It's akin to treating our lists like Python's "lists":

```haskell
goodListIdentity, badListIdentity :: [a] -> [a]
-- O(n)
goodListIdentity = foldr (:) []
-- O(n^2)
badListIdentity  = foldl (\a x -> a <> [x]) []
-- foldl' won't save you

```

You can use a combination of F-algebras and the `Yoneda` lemma to encode a free monad much more cheaply from the bottom-up, given that function composition is cheap as heck (credits to [Edward Kmett's blog](http://comonad.com/reader/2011/free-monads-for-less-2/)):

\begin{code}
-- the spookiest isomorphism in category theory
-- basically a suspended fmap, allowing for traversals through `f` to be delayed until you need them
newtype Yoneda f a = Yoneda {runYoneda :: forall b. (a -> b) -> f b}
  deriving Functor

toYoneda :: Functor f => f a -> Yoneda f a
toYoneda fa = Yoneda (`fmap` fa)

fromYoneda :: Functor f => Yoneda f a -> f a
fromYoneda yon = runYoneda yon id

-- not a Functor, try it for yourself
newtype FAlg f r = FAlg {runFAlg :: (f r -> r) -> r}

newtype Free' f a = Free' (Yoneda (FAlg f) a)
\end{code}

but triply nested newtypes are annoying, so let's expand things into our final representation:

\begin{code}
newtype Free f a = Free 
  { runFree :: forall r. 
    (a -> r) -- extract a pure value
    -> (f r -> r) -- fold f using an F-algebra
    -> r
  }
  deriving Functor

instance Applicative (Free f) where
  pure :: a -> Free f a
  pure x = Free \k _ -> k x

  (<*>) :: forall a b. Free f (a -> b) -> Free f a -> Free f b
  (<*>) = ap
  
instance Monad (Free f) where
  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  fa >>= f = Free 
    \(br :: b -> any) 
     (frr :: f any -> any) -> 
       runFree fa 
        (\a -> runFree (f a) br frr) 
        frr
\end{code}

By 'flipping' our representation around into one that threads around continuations, we end up with pretty heavy asymptotic improvements over the naive form. The F-algebra ((`f r -> r`), previously the `FAlg f r` component) is unused in these instances, but it's going to form the basis for how we'll end up interpreting effects later on.

Now that we have a better way of constructing syntax trees, let's look at our previous encoding of type-level unions. Why not improve our ergonomics a bit?

Side note, this document is a Literate Haskell file and I've had to enable like 2000000 extensions by this point. Kindly turn on `GADTs`, `PolyKinds`, `DataKinds`, and `KindSignatures` if you're following along.

```haskell
-- does every type in the list satisfy the constraint?
type ForAll :: (k -> Constraint) -> [k] -> Constraint
type family ForAll cs items where
  ForAll _ '[] = () -- empty constraint
  ForAll cs (item ': items) = (cs item, ForAll cs items) 
```

\ignore{
\begin{code}
class AllSatisfying (cs :: k -> Constraint) (items :: [k]) where
  type ForAll cs items :: Constraint

-- type families keep freezing my HLS, so the actual implementation is just an associated type instead of an open family
instance AllSatisfying cs '[] where
  type ForAll cs '[] = ()

instance AllSatisfying cs items => AllSatisfying cs (item ': items) where
  type ForAll cs (item ': items) = (cs item, ForAll cs items)
\end{code}}

\begin{code}
-- a list of functors and a single inhabitant
data Union (fs :: [Type -> Type]) a where
  -- end of the list
  Here :: Functor f => f a -> Union '[f] a
  -- either f a, or a union of the list's tail
  There :: (Functor f) => (f :+: Union fs) a -> Union (f ': fs) a

deriving instance Functor `ForAll` fs => Functor (Union fs)

\end{code}

\begin{code}
\end{code}

Previously, our instances for finding a subtype `f` within a coproduct `f :+: g` assumed that the coproduct was built right-associatively, that is, `f :+: g :+: h` implies `f :+: (g :+: h)`. Encoding things with a GADT has two advantages: this structure is enforced, and it's easier to type `[Maybe, Either Char, IO, ...]` than `(Maybe :+: Either Char :+: IO :+: ...)`.

Let's define another class for finding `Union` membership too, although we could reuse `(:<:)` for this:

\begin{code}
class (Functor f , Functor `ForAll` fs) => Member (f :: Type -> Type) (fs :: [Type -> Type]) where
  inject :: f a -> Union fs a 
  project :: Union fs a -> Maybe (f a)

instance (Functor `ForAll` (f ': fs)) => Member f (f ': fs) where
  inject = There . InL

  -- f occurs at the end
  project (Here fa) = Just fa
  -- f occurs at the front, and is inhabited
  project (There (InL fa)) = Just fa
  project _ = Nothing

instance {-# OVERLAPPABLE #-} (Functor g, Member f fs) => Member f (g ': fs) where
  inject = There . InR . inject

  project (There (InR fa)) = project fa
  project _ = Nothing
\end{code}

This is a ton of code at this point with no testing, so let's prove that the implementation of `Union` is well-typed:

\begin{code}
someUnion :: Union [Either String, Maybe, IO, Add, Mul] Int
someUnion = inject (Just 1) 
-- There $ InR $ There $ InL (Just 1)

-- Nothing
someIO :: Maybe (IO Int)
someIO = project someUnion

-- Just (Just 1)
someMaybe :: Maybe (Maybe Int)
someMaybe = project someUnion

-- use another union as a witness to help type inference
injectAs :: Member f fs => f a -> Union fs b -> Union fs a
injectAs fs _ = inject fs

injIO, injectEither 
  :: Union '[Either String, Maybe, IO, Add, Mul] Int

injIO = injectAs (read <$> getLine) someUnion
injectEither = injectAs notAnInt someUnion
  where notAnInt = Left @String "lol u errored out"

-- type error: No instance for Member (Either Text) '[]
-- someEitherText :: Maybe (Either Text Int)
-- someEitherText = project someUnion
\end{code}

It might seem inefficient to have to traverse through the same list repeatedly, but we're giving the compiler plenty of info on the exact path of each type within a `Union`, so we don't need to worry too much.

Now it's finally time to put it all together. First, let's build some utils for our effects library:

We're only ever going to use `Free` parameterized over some `Union` of functors, so let's fix our core monad to that type:

\begin{code}
newtype Eff (fs :: [Type -> Type]) a = Eff 
  { runEff :: forall r. 
    (a -> r) 
      -> (Union fs r -> r) 
      -> r
  }
  deriving (Functor, Applicative, Monad) via Free (Union fs)
-- god I love DerivingVia
\end{code}

Now, we need ways to lift and unlift effects as needed:

\begin{code}
-- compare with `lift` from MonadTrans
liftEff ::
  forall f (fs :: [Type -> Type]) a. 
  (Functor f, Member f fs) 
    => f a -> Eff fs a
liftEff fa = Eff 
  \(ar :: a -> r) 
   (frr :: Union fs r -> r) 
     -> frr $ inject (ar <$> fa)

-- we get this for freeeeeeeee
instance Member IO fs => MonadIO (Eff fs) where
  liftIO :: IO a -> Eff fs a
  liftIO = liftEff

-- given a natural transformation on a `Union`
-- (either adding or removing a functor),
-- lift into a new context
hoist :: (forall x. Union fs x -> Union gs x) 
      -> Eff fs a 
      -> Eff gs a
hoist phi fr = Eff \kp kf -> runEff fr kp (kf . phi)

-- Free monads correspond 1:1 to other monads given a natural transformation
foldEff :: Monad m 
  => (forall x. Union fs x -> m x) -> Eff fs a -> m a
foldEff phi fr = runEff fr pure (join . phi)

-- peel off the top effect of an `Eff` stack by handling it in terms of other effects
-- an `Eff` workflow works in terms of incrementally handling effects
interpret 
  :: forall f fs y. (Functor `ForAll` (f ': fs))
  => (forall x. f x -> Eff fs x)
  -> Eff (f ': fs) y 
  -> Eff fs y
interpret phi = foldEff \union -> Eff \kp kf -> 
  let exec fa = runEff (phi fa) kp kf
  in case union of
    Here fa -> exec fa
    There (InL fa) -> exec fa
    There (InR other) -> kf (kp <$> other)

-- we can escape the Eff monad once all effects have been handled
runFinal :: Monad m => Eff '[m] a -> m a
runFinal = foldEff \case 
  Here fx -> fx
  -- compiler can't infer that the list will never be non-empty
  _ -> error "Unreachable"
\end{code}

Congrats, now you have a fully-featured effects system in 200 lines of code. It's **finally** time to try out some effects now, so we'll use the classic `Teletype` example that everyone likes to reach for:

\begin{code}
data Teletype a
  = PrintLn String a
  | GetLine (String -> a)
  deriving Functor

-- the transformation from an Effect type to helper functions is mechanical 
-- and can be abstracted away with TemplateHaskell

println :: Member Teletype fs => String -> Eff fs ()
println s = liftEff (PrintLn s ())

getLine_ :: Member Teletype fs => Eff fs String
getLine_ = liftEff (GetLine id)

\end{code}

And some other effects for posterity, I guess:

\begin{code}
data FileSystem a 
  = ReadFile FilePath (Maybe String -> a)
  | WriteFile FilePath String a
  deriving Functor

readFile_ :: Member FileSystem fs => FilePath -> Eff fs (Maybe String)
readFile_ path = liftEff (ReadFile path id)

writeFile_ :: Member FileSystem fs => FilePath -> String -> Eff fs ()
writeFile_ path s = liftEff (WriteFile path s ())

newtype Error e a = Error e deriving (Functor)

throwErr :: Member (Error e) fs => e -> Eff fs a
throwErr err = liftEff (Error err)
\end{code}

And we'll try seeing these effects in action now:

\begin{code}
interactiveCat 
  :: (Member FileSystem fs, Member Teletype fs, Member (Error String) fs)
  => Eff fs ()
interactiveCat = do
  numFiles <- readMaybe <$> getLine_
  case numFiles of
    Nothing -> throwErr @String 
      "Couldn't parse the number of files you want me to read!!"
    Just n -> replicateM_ n do
      path <- getLine_
      mbFile <- readFile_ path
      body <- maybe (throwErr @String "Couldn't locate file!!") pure mbFile
      traverse_ println (lines body)

ww :: IO ()
ww = interactiveCat
  & interpretFS
  & interpret interpretTTY
  & interpret interpretErr
  & runFinal
  where
    interpretErr :: Error String a -> Eff '[IO] a
    interpretErr = undefined
    interpretFS :: Member IO r => Eff (FileSystem ': r) a -> Eff r a
    interpretFS = interpret \case
      ReadFile path k -> liftIO do
        res <- (Just <$> readFile path) `catch` \(err :: IOException) -> 
          print err $> Nothing
        pure $ k res
      WriteFile path s k -> 
        k <$ liftIO (writeFile path s)
      -- WriteFile path s k -> do
        
    interpretTTY = liftIO . \case
      PrintLn line a -> liftIO (print line) $> a
      GetLine k -> k <$> getLine
\end{code}

todo make interpret polymorphic so we can handle error effects properly
talk about dependency injection/mocking
