\begin{code}
{-# LANGUAGE RankNTypes
  , TupleSections
  , InstanceSigs
  , OverloadedStrings
  , GADTs
  , ScopedTypeVariables
  , LambdaCase
  , DeriveFunctor
  , StandaloneDeriving
  , BlockArguments
  , TypeOperators
  , TypeFamilies
  , FlexibleContexts
  , FlexibleInstances
  , DeriveGeneric
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses 
#-}

module InterviewFree where
import Control.Monad
import Data.IORef
import Data.Functor
import Data.Maybe
import Data.Typeable
import Control.Monad.IO.Class
import GHC.Generics
import Data.String
import Control.Applicative
import Data.Traversable
import Data.Dynamic
import Control.Monad.Free
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.Trans.Cont ( evalContT )
import Control.Monad.Trans.Class
import Control.Monad.Writer
import qualified Data.IntMap as M
import qualified Data.HashMap.Strict as H
import Debug.Trace
import Data.Hashable
import Data.Foldable (traverse_)

default (BASICExpr -> Free BASICFree ())

data BASICVar
  = A
  | B
  | C
  | X
  | Y
  | Z
  | Lit Int
  | Str String
  | PEEK BASICVar
  | Arith Char BASICVar BASICVar
  deriving (Eq, Show, Generic)

(.=) :: BASICVar -> BASICVar -> BASICExpr
(.=) = EQL

instance Hashable BASICVar

instance IsString BASICVar where
  fromString = Str

instance Num BASICVar where
  fromInteger = Lit . fromIntegral
  (+) = Arith '+'
  (*) = Arith '*'
  abs = Arith 'a' 0
  signum = Arith 's' 0
  (-) = Arith '-'


data TO = TO deriving (Eq, Show)

data THEN = THEN deriving (Eq, Show)

data BASICExpr
  = GOTO Int
  | GOSUB Int
  | RETURN
  | PRINT BASICVar 
  | INPUT BASICVar
  | BASICVar := BASICVar
  | FOR BASICExpr TO BASICVar
  | EQL BASICVar BASICVar
  | AND BASICExpr BASICExpr
  | OR BASICExpr BASICExpr
  | IF BASICExpr THEN BASICExpr
  | POKE BASICVar BASICVar
  | NEXT BASICVar
  | STEP BASICExpr Int
  | Inline BASICExpr BASICExpr
  | END
  | DEBUG Char
  
infixr 5 :=

deriving instance Eq BASICExpr
deriving instance Show BASICExpr

data BASICFree a = BASICFree Int BASICExpr a deriving Functor
type BASIC = Free BASICFree

liftBASIC :: Int -> BASICExpr -> BASIC ()
liftBASIC n exp = liftF $ BASICFree n exp ()

-- not really forall
instance forall a. (a ~ ()) => Num (BASICExpr -> BASIC a) where
  fromInteger :: Integer -> BASICExpr -> BASIC a
  fromInteger n exp = liftBASIC (fromIntegral n) exp 

-- bfSomething :: BASICFree a -> BASIC a
-- bfSomething bf = liftF bf
bfTest :: BASIC ()
bfTest = do
  10$ GOTO 15
  15$ X := 1
  20$ FOR (Y:=2) TO X `STEP` 3 ； PRINT "Hello, " ； PRINT "World!"
  25$ NEXT Y
  -- 30$ IF ()
  40$ A := PEEK(X)
  50$ POKE 10 X
  60$ A := PEEK(10)
  70$ FOR (Y:=0) TO X ； POKE Y X ； PRINT "hi" 

-- logging stuff
newtype Log = Log {getLog :: String -> String}

output :: String -> Log
output s = Log (('\n' : s) <>)

instance Show Log where
  show = ($ "") . getLog

instance Semigroup Log where
  Log s1 <> Log s2 = Log $ s1 . s2

instance Monoid Log where
  mempty = Log id

printAST :: BASIC () -> String
printAST = (`getLog` "") . execWriter . iterA \(BASICFree n e accum) -> accum >> tell (fmtLine n e)

fmtLine :: Int -> BASICExpr -> Log
fmtLine n exp = Log $ ((show n <> ": " <> show exp) <>) . ('\n' :)
  
showBasic :: BASIC () -> IO ()
showBasic = mapM_ putStrLn . reverse . lines . printAST
--

forTest = FOR (X:=1) TO 10
forStepTest = FOR (X:=1) TO 10 `STEP` 3

data IndexedCont = IC {
  basicExp :: BASICExpr,
  cont :: BVM ()
}

data Phase = Load | Run deriving (Eq, Show)

data BASICState = BASICState 
  { memory :: M.IntMap Int
  , variables :: H.HashMap BASICVar BASICVar
  , instr :: M.IntMap IndexedCont
  , stack :: [BVM ()]
  , loopStack :: [(BASICVar, Int, Int, BVM ())]
  , flagReg :: Int
  , startCont :: BVM () 
  , vmPhase :: Phase
  , basicError :: Maybe BASICError
  } 

emptyState :: BASICState
emptyState = BASICState mempty mempty mempty [] [] 0 (pure ()) Load Nothing

data BASICError 
  = UninitializedVar BASICVar
  | NullReference
  | TypeMismatch String String
  | HsError String
  deriving (Eq, Show)

newtype BVM a = BVM {runBVM :: ContT () (ReaderT (IORef BASICState) (WriterT Log IO)) a} 
  deriving (Functor, Applicative, Monad, MonadReader (IORef BASICState), MonadCont, MonadIO)

gets :: (BASICState -> a) -> BVM a
gets f = do
  st <- liftIO . readIORef =<< ask
  pure $ f st

get :: BVM BASICState
get = gets id
put :: (MonadReader (IORef a) m, MonadIO m) => a -> m ()
put bs = ask >>= liftIO . (`writeIORef` bs)
modify' :: (BASICState -> BASICState) -> BVM ()
modify' f = gets f >>= put
modify :: (BASICState -> BASICState) -> BVM ()
modify = modify'

-- no catching errors in ContT
throwError :: BASICError -> BVM a
throwError e = do
  state <- get
  modify \bst -> bst {basicError = Just e}
  BVM $ ContT \_ -> pure ()
  
instance MonadFail BVM where
  fail = throwError . HsError

bvmTell :: Log -> BVM ()
bvmTell = BVM . lift . lift . tell

runBASIC :: BASIC () -> IO (Either BASICError Log)
runBASIC b = do
  ref <- newIORef emptyState 
  log <- execWriterT . (`runReaderT` ref) .  evalContT . runBVM . reifyStart $ iterBASIC b
  (`handleErr` log) <$> readIORef ref
  where handleErr basicState log = maybe (Right log) Left $ basicError basicState
        iterBASIC :: BASIC () -> BVM ()
        iterBASIC = foldFree go
        -- interpreter runs in 2 phases: 
          -- 1. indexing all BASICExpr+continuations by line number
          -- 2. jumping back to the beginning, and interpreting expressions now that we have the entire program indexed
        reifyStart :: BVM () -> BVM ()
        reifyStart prog = do
          callCC \k -> do
            -- reify starting continuation so we can jump back to the top
            modify' \bst -> bst {startCont = k ()}
          let chgPhase p = modify' \bst -> let
                (keys, conts) = unzip . M.assocs $ instr bst
                -- shift continuations so that their number is aligned with their own cont
                conts' = M.fromList (zip (tail keys) conts)
                in bst {vmPhase = Run, instr = conts'}
          gets vmPhase >>= \case
            Load -> do
              prog
              chgPhase Run
              join $ gets startCont
            Run -> do 
              instr <- gets instr
              prog

        go :: BASICFree a -> BVM a
        go (BASICFree line exp next) = do
          callCC \k -> do
            gets vmPhase >>= \case
              Load -> do
                let ic = IC exp (k ()) 
                instr' <- gets (M.insert line ic . instr)
                modify' \bst -> bst {instr = instr'}
              Run -> do
                interpretBASIC exp
          pure next

getCont :: Int -> BVM (Maybe (BVM ()))
getCont n = do
  conts <- gets (M.lookup n . instr)
  pure Nothing

handleArith :: Char -> BASICVar -> BASICVar -> BVM Int
handleArith 'a' _ v = abs <$> derefInt v
handleArith 's' _ v = signum <$> derefInt v
handleArith opc a b = op <$> derefInt a <*> derefInt b
  where op = case opc of {'+' -> (+); '-' -> (-); '*' -> (*)}

derefInt :: BASICVar -> BVM Int
derefInt (PEEK addr) = do
  rawAddr <- derefInt addr
  gets (M.lookup rawAddr . memory) >>= maybe (throwError NullReference) pure 

derefInt (Lit n) = pure n
derefInt (Str _) = throwError $ TypeMismatch "String" "Int"
derefInt (Arith opc a b) = handleArith opc a b
derefInt v = do
  Just n <- gets (H.lookup v . variables)
  derefInt n

runBool :: BASICExpr -> BVM Bool
runBool e = interpretBASIC e >> ((< 0) <$> gets flagReg)

runFor :: BASICVar -> BASICVar -> BASICVar -> Int -> BVM ()
runFor lhs rhs end step = do
  interpretBASIC (lhs := rhs)
  lim <- derefInt end
  callCC \k -> modify \bst -> bst {loopStack = (lhs, lim, step, k ()) : loopStack bst}

interpretBASIC :: BASICExpr -> BVM ()
interpretBASIC = \case
  GOTO line -> do
     Just (IC e cont) <- gets (M.lookup line . instr)
     insts <- gets instr
     -- jump back to runBASIC::go
     cont
  GOSUB line -> callCC \k -> do
    -- push current cont onto the "call stack"
    modify' \bst -> bst{stack = k () : stack bst}
    interpretBASIC (GOTO line)
  RETURN -> do
    (cont:conts) <- gets stack
    modify' \bst -> bst {stack = conts}
    cont
  PRINT x -> do
    let puts = bvmTell . output
        printBASIC (Lit n) = puts $ show n
        printBASIC (Str s) = puts s
        printBASIC pk@(PEEK p) = derefInt pk >>= puts . show
        printBASIC bv = gets (H.lookup bv . variables)
          >>= traverse_ (puts . show) 
    printBASIC x
  INPUT target -> do
    input <- Str <$> liftIO getLine
    interpretBASIC $ target := input
  lhs := rhs -> do 
    rhsActual <- case rhs of
      PEEK addr -> Lit <$> derefInt addr
      Arith opc a b -> Lit <$> handleArith opc a b
      _ -> pure rhs
    vars' <- gets (H.insert lhs rhsActual . variables)
    modify \bst -> bst {variables = vars'}
  FOR assign@(lhs := rhs) TO end -> runFor lhs rhs end 1
  STEP (FOR (lhs := rhs) TO end) step -> runFor lhs rhs end step
  NEXT lhs -> do
    ((name, lim, step, cont) : loops) <- gets loopStack
    unless (name == lhs) $ throwError (UninitializedVar lhs)
    val <- derefInt lhs
    unless (val == lim) $ modify' (\bst -> bst {variables = H.insert lhs (Lit (val + step)) $ variables bst})
      >> cont
  POKE addr var -> do
    val <- derefInt var
    addr' <- derefInt addr
    modify' \bst -> bst {memory = M.insert addr' val (memory bst)}

  EQL s t -> do
    let 
      eql (Str s) (Str t) = pure $ s == t
      eql (Str _) _ = pure False
      eql _ (Str _) = pure False
      eql (Lit n) (Lit m) = pure $ n == m
      eql a b = (==) <$> derefInt a <*> derefInt b
    res <- eql s t
    modify \bst -> bst {flagReg = if res then (-1) else 0}
  AND a b -> do
    res <- do 
      r1 <- runBool a
      if r1 then runBool b
      else pure False
    modify \bst -> bst {flagReg = if res then (-1) else 0}
  OR a b -> do
    res <- do
      r1 <- runBool a 
      if r1 then pure True else runBool b
    modify \bst -> bst {flagReg = if res then (-1) else 0}
  IF exp THEN act -> do 
    flag <- runBool exp
    when flag $ interpretBASIC act
  Inline act1 act2 -> interpretBASIC act1 >> interpretBASIC act2
  END -> BVM $ ContT \_ -> pure ()
  DEBUG c -> do 
    s <- case c of
      'v' -> show <$> gets variables
      'm' -> show <$> gets memory
      'k' -> show . M.keys <$> gets instr
      'f' -> show <$> gets flagReg
      'p' -> show <$> gets vmPhase
      'e' -> show <$> gets basicError
    liftIO $ mapM_ putStrLn (lines s) 
    pure ()

(；) :: BASICExpr -> BASICExpr -> BASICExpr
(；) = Inline

xs = GOTO 10 ； PRINT "henlo" ； PRINT "bye"

tst :: BASIC ()
tst = do
  10$ FOR (A:=0) TO 10 ； PRINT "hi" ； NEXT A
  15$ B := 128
  20$ POKE B 0
  30$ FOR (A:=0) TO 30 `STEP` 2
  40$   PRINT (PEEK(B))
  45$   POKE B A
  50$ NEXT A
  60$ X:=0
  70$ X := X+1
  75$ PRINT X
  80$ IF (X .= 10) THEN (GOTO 100)
  85$ IF (X .= 5) THEN (PRINT X ； END)
  90$ GOTO 70
  100 END

endTst :: BASIC ()
endTst = do
  0$ PRINT "HI"
  10$ PRINT "BYE"
  12$ GOTO 17
  15$ END
  17$ PRINT "I'm still here"
  20$ X := 0
  30$ Y := 1
  40$ END

goSubTest :: BASIC ()
goSubTest = do
  0$ X := 0 
  10$ IF (X .= 10) THEN (GOTO 30)
  15$ PRINT X
  20$ GOSUB 100
  25$ GOTO 10
  30$ PRINT "END VALUE OF X: " ； PRINT X
  50$ END
  100$ X := X + 1
  110$ RETURN

\end{code}
