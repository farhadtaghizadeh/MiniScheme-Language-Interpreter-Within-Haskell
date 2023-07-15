{-# LANGUAGE ExistentialQuantification, FlexibleInstances, TypeFamilies, TypeSynonymInstances, ScopedTypeVariables #-}
module Checker where

import Control.Monad.State
import Data.List (intercalate)
import System.Random
import System.Random.Shuffle (shuffle')
import Test.HUnit hiding (State(..))
import Text.Parsec hiding (label, token, State(..))

import Interface
import Main
import Parser (Parser, token, bracketed, int, terminal, parseExprs)

import Debug.Trace


--------------------------------------------------------------------------------
-- Parsing and representing Scheme expressions, as Scheme expressions

instance SchemeData (Parser ()) where

    symbol s = token (string s) >> return ()

    number i = do j <- token int
                  guard (i == j)

    boolean True = token (string "#t") >> return ()    -- Careful with this one
    boolean False = token (string "#f") >> return ()

    cons d e = bracketed (d >> token (string ".") >> e)

    nil = (token (string "()") <|> token (string "[]")) >> return ()

    proper [] = nil
    proper es = bracketed (go es) where
        go [] = (token (string ".") >> nil) <|> return ()
        go (e : es) = e >> (go es <|> (token (string ".") >> proper es))

    improper [] e = e
    improper ds e = bracketed (go ds) where
        go [d] = d >> token (string ".") >> e
        go (d : ds) = d >> (go ds <|> (token (string ".") >> improper ds e))

instance SchemeData String where
    symbol s = s
    number n = show n
    boolean True = "#t"
    boolean False = "#f"
    cons s t = "(" ++ s ++ " . " ++ t ++ ")"
    nil = "()"
    proper ds = "(" ++ intercalate " " ds ++ ")"
    improper ds d = "(" ++ intercalate " " ds ++ " . " ++ d ++ ")"

---------------------------------------------------------------------------------
-- Some utilities

lambda :: SchemeData a => [String] -> a -> a
lambda vars body = proper [symbol "lambda", proper (map symbol vars), body]

lambdaI :: SchemeData a => [String] -> String -> a -> a
lambdaI vars var body = proper [symbol "lambda", improper (map symbol vars) (symbol var), body]

llet :: SchemeData a => [(String,a)] -> a -> a
llet binds body = proper [symbol "let*" , proper (map (\ (n,v) -> proper [symbol n, v]) binds) , body]

fst', snd' :: SchemeData a => a -> a
fst' x = proper [symbol "fst" , x]
snd' x = proper [symbol "snd" , x]

applyImplicit :: SchemeData a => a -> [a] -> a
applyImplicit f xs = proper (f:xs)

---------------------------------------------------------------------------------
-- Testing requires access to a source of randomness

type TestM = State StdGen

randomly :: (StdGen -> (a, StdGen)) -> TestM a
randomly f = do g <- get
                let (r, g') = f g
                put g'
                return r

shuffle :: [a] -> TestM [a]
shuffle xs = do
  g <- get
  let (_ :: Int,g') = randomR (0,10) g
  put g'
  return (shuffle' xs (length xs) g)

pick :: [a] -> TestM a
pick [] = error "pick from empty list"
pick xs = do i <- randomly (randomR (0, n))
             return (xs !! i)
    where n = length xs - 1

pickFrom :: [TestM a] -> TestM a
pickFrom = join . pick

pickNumber, pickBoolean, pickSymbol :: SchemeData a => TestM a
pickNumber = number <$> pick [-100..100]
pickBoolean = boolean <$> pick [False, True]
pickSymbol =
    do n <- pick [0..9]
       first <- pick alpha
       rest <- replicateM n (pick (alpha ++ ['0'..'9']))
       return (symbol (first : rest))
    where alpha = ['a'..'z'] ++ ['A'..'Z'] -- ++ ['+', '-', '*', '_', '?', '!']

pickSymbolsUnique :: SchemeData a => Int -> TestM [a]
pickSymbolsUnique n' = do
  names <- replicateM n' makeNames
  let names' = zipWith (++) names (map show [0..(n'-1)])
  return (map symbol names')
  where
  alpha = ['a'..'z'] ++ ['A'..'Z'] -- ++ ['+', '-', '*', '_', '?', '!']
  makeNames = do
    n <- pick [0..2]
    first <- pick alpha
    rest <- replicateM n (pick (alpha ++ ['0'..'9']))
    return (first:rest)
pickValue :: SchemeData a => Int -> TestM (a, a)
pickValue depth
    | depth == 0 = pickFrom [d pickNumber, d pickBoolean, pickNil]
    | otherwise  = pickFrom [d pickNumber, d pickBoolean, pickCons, pickNil]
    where d m = (\d -> (d, d)) <$> m
          pickCons = do (c1, d1) <- pickValue (depth - 1)
                        (c2, d2) <- pickValue (depth - 1)
                        return (proper [symbol "cons", c1, c2], cons d1 d2)
          pickNil  = return (proper [symbol "list"], nil)

pickListN :: SchemeData a => Int -> TestM a -> TestM a
pickListN n m = proper <$> replicateM n m

pickList :: SchemeData a => TestM a -> TestM a
pickList m = flip pickListN m =<< pick [0..20]

--------------------------------------------------------------------------------
-- Test cases

class TestCase t where
    check :: Int -> t -> TestM Assertion
    label :: Int -> t -> String

instance (t ~ String, u ~ Maybe String) => TestCase (t, u) where
    check i (prompt, result) =
        return $
        -- trace (prompt ++ " ~~> " ++ show result) $
        case (run d, result) of
            (Just e, Just rs) ->
                let rp :: Parser ()
                    rp = case parseExprs True rs of
                             Left err -> error ("in " ++ rs ++ ": " ++ show err)
                             Right [rp] -> rp
                in case runParser (terminal rp) () "" e of
                       Left _ -> assertString (testCaseStr ++ "\nExpected \"" ++ rs ++ "\", not \"" ++ e ++ "\"")
                       Right _ -> return ()
            (Nothing, Nothing) -> return ()
            (Just e, Nothing) -> assertString (testCaseStr ++ "\nFailure expected, not \"" ++ e ++ "\"")
            (Nothing, Just rs) -> assertString (testCaseStr ++ "\nFailure unexpected, expected \"" ++ rs ++ "\"")
        where d = case parseExprs True prompt of
                     Left err -> error ("in " ++ prompt ++ ": " ++ show err)
                     Right [d] -> d
                     Right _   -> error ("wrong number of expressions: " ++ prompt)
              testCaseStr = init (unlines (("In test case [" ++ show i ++ "]:") : map ("    " ++) (lines prompt)))

    label i (prompt, _) = "Test case [" ++ show i ++ "]: " ++ prompt

instance (t ~ String, u ~ Maybe String) => TestCase (TestM (t, u)) where
    check i tc = tc >>= check i
    label i _  = "Test case [" ++ show i ++ "]: monadic"

data Labeled = forall a. TestCase a => Labeled String a

instance TestCase Labeled where
    check i (Labeled _ tc) = check i tc
    label i (Labeled s _)  = "Test case [" ++ show i ++ "]: " ++ s

data TC = forall a. TestCase a => TC a

instance TestCase TC where
    check n (TC tc) = check n tc
    label i (TC tc) = label i tc

(~~) :: TestCase a => String -> a -> TC
s ~~ m = TC (Labeled s m)
