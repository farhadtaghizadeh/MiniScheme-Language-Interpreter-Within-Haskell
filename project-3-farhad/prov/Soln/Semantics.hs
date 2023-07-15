module Semantics (eval, eval_maybe, test, parse) where

-- GHC
import Data.Maybe (fromJust, listToMaybe)
import Control.Monad.Except
import Control.Monad.State

-- ./prov
import Text.Parsec (runParser)
import Parser (expr, terminal)

-- ./src
import Datum
import Interface (SchemeData (proper))
import Printer (printDatums, printDatum)

import Prelude hiding (const, fail)

--------------------------------------------------------------------------------
-- Misc. Helpers

-- Pairwise application of `f`. For example,
--   pairwise (<) [1, 2, 3]
-- is
--   (1 <= 2) && (2 <= 3)
pairwise :: (a -> a -> Bool) -> [a] -> Bool
pairwise f xs = and (zipWith f xs (tail xs))

-- =============================================================================
-- Num util
-- =============================================================================

-- Evaluate a list of expressions [e1, e2, ..., en] and scoop out any numbers.
getNumbers :: [Datum] -> Result [Int]
getNumbers xs = mapM getNum (noNull xs)
  where
    getNum :: Datum -> Result Int
    getNum (Num n) = return n
    getNum x       = fail ("getNum given " ++ show x ++ ", which is not a number.")

-- =============================================================================
-- Bool util
-- =============================================================================

-- Everything except #f is "truthy" in MiniScheme, meaning it behaves as "true"
-- in conditional statements.
truthy :: Datum -> Bool
truthy (Const False) = False
truthy _             = True

-- =============================================================================
-- Evaluating special forms
-- =============================================================================

specialForms :: [String]
specialForms = ["if", "cond", "and", "or", "let*", "lambda", "begin", "set!", "define"]

isSpecialForm :: String -> Bool
isSpecialForm s = s `elem` specialForms

specialFormHandler :: String -> [Datum] -> Result Datum

-- --------------------------------------------------------------------------------
-- -- if & cond conditionals

specialFormHandler "if" [e1, e2, e3, Nil] = do
    d1 <- eval e1
    if truthy d1
      then eval e2
      else eval e3

specialFormHandler "cond" xs = do
  cs <- cases xs
  handleCases cs

  where
    handleCases :: [(Datum, Datum)] -> Result Datum
    handleCases []                         = bad
    handleCases ((Symbol "else", e2) : xs) = eval e2
    handleCases ((e1, e2) : xs)            = do
      d1 <- eval e1
      if truthy d1
        then eval e2
        else handleCases xs

    cases :: [Datum] -> Result [(Datum, Datum)]
    cases ds = sequence (init (fmap toCase ds))
    
    toCase :: Datum -> Result (Datum, Datum)
    toCase (Cons x (Cons y Nil)) = return (x, y)
    toCase xs                    = fail ("bad case: " ++ show xs)
      

--------------------------------------------------------------------------------
-- "and" and "or" booleans

specialFormHandler "and" xs = conjunct xs
  where
    conjunct :: [Datum] -> Result Datum
    conjunct [Nil]    = true
    conjunct [x, Nil] = eval x
    conjunct (x : xs) = do
      r <- eval x
      if truthy r
        then conjunct xs
        else return r

specialFormHandler "or" xs    = disjunct xs
  where
    disjunct :: [Datum] -> Result Datum
    disjunct [Nil]    = false
    disjunct [x, Nil] = eval x
    disjunct (x : xs) = do
      r <- eval x
      if truthy r
        then return r
        else disjunct xs
      
--------------------------------------------------------------------------------
-- Local Assignment & Functions

-- N.B. to students:
-- 
-- This is the first time you have seen `temporary`. It is defined in
-- `Datum.hs`.  When you wrap a monadic action in `temporary`, it means that
-- action will only *temporarily* affect the environment. For example,
-- suppose I bind x to 2 in
--
--   (let [(x 2)] x)
--
-- I *temporarily* put ("x", Num 2) in the environment. But I don't want that
-- binding to persist after I have evaluated the body. So I wrap
--
--  `handleLet bs body`
--
-- in `temporary` so as to say "any bindings I add in `handleLet` will go away
-- once it is done doing its thing."
--
-- You will see `temporary` used below. Note also that this is sort of like
-- what's described in Tasks.md under "Environments and frames", but will likely
-- not be sufficient for implementing state in project 3.

specialFormHandler "let*" [bs, body, Nil] = temporary (handleLet bs body)
  where
    handleLet bs body = do
      binds <- mapM toBind (noNull (flatten bs))
      evalBinds binds
    
    evalBinds :: [(String, Datum)] -> Result Datum
    evalBinds []            = eval body
    evalBinds ((s, e) : bs) = do
      e' <- eval e
      assign s e'
      evalBinds bs
    
    toBind :: Datum -> Result (String, Datum)
    toBind (Cons (Symbol s) (Cons y Nil)) = return (s, y)
    toBind xs                             = fail ("bad case: " ++ show xs)

specialFormHandler "lambda" [Symbol s, body, Nil] =
  return (Lambda (\xs -> temporary (handler xs)))
  where
    handler xs= do
      assign s (proper (noNull xs))
      eval body
    
specialFormHandler "lambda" [(Cons (Symbol s) Nil), body, Nil] =
  return (Lambda (\xs -> temporary (handler xs)))
  where
    handler [x, Nil] = do
      assign s x
      eval body
    
specialFormHandler "lambda" [vs@(Cons _ _), body, Nil] =
  return (Lambda (\xs -> temporary (handler xs)))
  where
    vars = flatten vs
    
    toSymbol :: Datum -> Result String
    toSymbol (Symbol s) = return s
    toSymbol s          = invalidArg "toSymbol" [s]

    handler xs
      | isProper vars   = properHandler xs
      | isImproper vars = improperHandler xs
      | otherwise       = invalidArg "lambda" xs

    properHandler xs
      | length vars == length xs = do
          symbols <- mapM toSymbol (noNull vars)
          mapM (\(v, x) -> assign v x) (zip symbols xs)
          eval body
      | otherwise                = invalidArg "lambda" xs

    improperHandler xs
      | length xs >= length vars = do
            symbols <- mapM toSymbol (noNull vars)
            let m             = length symbols
            let (front, back) = splitAt (m - 1) xs
            mapM (\(v, x) -> assign v x) (zip (init symbols) front)
            assign (last symbols) (proper (noNull back))
            eval body
      | otherwise               = invalidArg "lambda" xs

specialFormHandler "begin" [x, Nil] = eval x
specialFormHandler "begin" (x:xs) = do 
  evalFirst <- eval x
  specialFormHandler "begin" xs 

specialFormHandler "set!" [Symbol s, e, Nil] = do
  getExpr <- eval e
  setFirst <- setter s getExpr
  return Nil

specialFormHandler "define" [Symbol s, e, Nil] = do
  e2 <- eval e 
  defined <- case s of
    [] -> fail ("fail")
    s -> assign s e2
  return Nil

     
specialFormHandler s xs = argsBorked s xs

-- =============================================================================
-- Evaluating primitives
-- =============================================================================

primitives :: [String]
primitives = [
  "eq?", "+", "*", "-", "=", "<", "<=", ">", ">=",
    "cons", "fst", "snd", "list", "number?",
    "boolean?", "pair?", "nil?", "list?","apply"
  ]
isPrim :: String -> Bool
isPrim s = s `elem` primitives

primHandler :: String -> [Datum] -> Result Datum

-- --------------------------------------------------------------------------------
-- -- Arithmetic operators (+, -, *)

primHandler "+" [] = argsBorked "+" []
primHandler "+" xs = do
  ns <- getNumbers xs
  num (sum ns)
  
primHandler "*" [] = argsBorked "*" []
primHandler "*" xs = do
  ns <- getNumbers xs
  num (product ns)

primHandler "-" [] = argsBorked "-" []
primHandler "-" xs = do
  (n : ns) <- getNumbers xs
  num (sum (n : map negate ns))

-- --------------------------------------------------------------------------------
-- -- Integer comparison operators

primHandler "=" [Nil] = argsBorked "=" []
primHandler "=" xs    = do
  ns <- getNumbers xs
  const (pairwise (==) ns)
    
primHandler "<" [Nil] = argsBorked "<" []
primHandler "<" xs    = do
  ns <- getNumbers xs
  const (pairwise (<) ns)  

primHandler "<=" [Nil] = argsBorked "<=" []
primHandler "<=" xs    = do
  ns <- getNumbers xs
  const (pairwise (<=) ns)
  
primHandler ">" [Nil] = argsBorked ">" []
primHandler ">" xs    = do
  ns <- getNumbers xs
  const (pairwise (>) ns)

primHandler ">=" [Nil] = argsBorked ">=" []
primHandler ">=" xs    = do
  ns <- getNumbers xs
  const (pairwise (>=) ns)

-- Operations on pairs & lists

primHandler "cons" [e1, e2, Nil] = cons e1 e2
  
primHandler "fst" [Cons e1 e2, Nil] = return e1

primHandler "snd" [Cons e1 e2, Nil] = return e2

primHandler "list" [Nil] = nil
primHandler "list" xs    = return (proper (init xs))

-- Question operators

primHandler "eq?" [e1, e2, Nil] = const (e1 == e2)
  
primHandler "number?" [(Num _), Nil] = true
primHandler "number?" [_, Nil]       = false
    
primHandler "boolean?" [(Const _), Nil] = true
primHandler "boolean?" [_, Nil]         = false

primHandler "pair?" [(Cons a b), Nil] = true
primHandler "pair?" [_, Nil]          = false

primHandler "nil?" [Nil, Nil] = true
primHandler "nil?" [_, Nil]   = false

primHandler "list?" [(Cons a b), Nil] = const (isProper (flatten (Cons a b)))
primHandler "list?" [_, Nil]          = false

primHandler "apply" [x, Nil] = do
  f <- eval x
  case f of
    PrimOp s handler -> handler [Nil]
    Lambda   handler -> handler [Nil]
primHandler "apply" [x, xs, Nil] = do
  f <- eval x
  case f of
    PrimOp s handler -> handler (flatten xs)
    Lambda   handler -> handler (flatten xs)
primHandler "apply" (x : xs) = do
  f <- eval x
  let args = go xs where
        go [Nil]    = []
        go [x, Nil] = flatten x
        go (x : xs) = x : go xs
  case f of
    PrimOp s handler -> handler args
    Lambda   handler -> handler args
    _                -> bad


primHandler s xs = argsBorked s xs

primOps :: Env
primOps = [map (\ s -> (s, PrimOp s (\ ds -> primHandler s ds))) primitives]

-- =============================================================================
-- Evaluation
-- =============================================================================

initialEnv = primOps
  
eval :: Datum -> Result Datum
eval (Num n)                                   = return (Num n)
eval (Const b)                                 = return (Const b)
eval Nil                                       = return Nil
eval (Cons (Cons (Symbol "lambda") args) rest) = do
  f <- specialFormHandler "lambda" (flatten args)
  case f of
    Lambda handler -> do
      newArgs <- mapM eval flat
      handler newArgs
    _              -> bad
  where
    flat :: [Datum]
    flat = flatten rest
eval (Cons (Symbol s) rest) 
  | not (isProper flat)                        = fail
                                                 ("cannot evaluate " ++ show s ++
                                                  " with improper list as argument: "
                                                  ++ show flat)
  | isSpecialForm s                            = specialFormHandler s flat
  | otherwise                                  = do
      d <- lookupEnv s
      case d of
        (PrimOp s f) -> do
          newArgs <- mapM eval flat
          f newArgs
        (Lambda f)   -> do
          newArgs <- mapM eval flat
          f newArgs
        _ -> fail ("Cannot evaluate primitive operation " ++ s)
  where
    flat :: [Datum]
    flat = flatten rest
eval (Symbol   s)                              = lookupEnv s
eval (PrimOp s f)                              = return (PrimOp s f)
eval (Lambda f)                                = return (Lambda f)
eval d                                         = fail ("I do not know how to evaluate " ++ show d)

eval_either :: Datum -> Either Error Datum
eval_either d = evalStateT (eval d) initialEnv
  
eval_maybe :: Datum -> Maybe Datum
eval_maybe d = case eval_either d of
                     Left  _            -> Nothing
                     Right (Lambda _)   -> Nothing 
                     Right (PrimOp _ _) -> Nothing
                     Right d -> Just d                   

--------------------------------------------------------------------------------
-- Debugging

parse :: String -> Datum
parse s =
    case runParser (terminal (expr True)) () "" s of
        Left err -> error (show err)
        Right d  -> d

test :: String -> Either Error String
test d = fmap printDatum (eval_either (parse d))

--------------------------------------------------------------------------------
-- Example Usage

-- `parse` will show you the `Datum` a MiniScheme expression parses into.
-- >>> parse "(eq? 1 2)"

-- `eval` will evaluate a `Datum`.
-- >>> eval (Cons (Symbol "eq?") (Cons (Num 1) (Cons (Num 2) Nil)))

-- Of course, you can compose these two to get `test`.
-- `test` parses a string and then evaluates it.
-- >>> test "(if (eq? 1 2) #t #f)"
