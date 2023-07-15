{-# LANGUAGE ScopedTypeVariables #-}
module Tests (module Tests) where

import Control.Monad.State
import System.Exit
import System.Environment ( getArgs )
import System.Random (getStdGen, mkStdGen)
import Test.HUnit

import Checker
import Data.List (partition, intercalate)
import Interface
import ChallengeTests
import Text.Read (readMaybe)

type TestCases = [TC]

-- #############################################################################
-- Representing and evaluating simple data.
-- #############################################################################

--------------------------------------------------------------------------------
-- (1) constants
--------------------------------------------------------------------------------
constTests :: TestCases
constTests =
    [ TC ("1", Just "1")
    , TC ("-1", Just "-1")
    , TC ("#t", Just "#t")
    , TC ("#f", Just "#f")
    ]

-- #############################################################################
-- Evaluating Special forms
-- #############################################################################

--------------------------------------------------------------------------------
-- (5) if
--------------------------------------------------------------------------------
ifTests :: TestCases

ifTests =
    [ TC ("(if #t 1 2)", Just "1")
    , TC ("(if #f 1 2)", Just "2")
    , TC ("(if 1 2 3)", Just "2")
    , TC ("(if (if 1 2 3) 2 3)", Just "2")
    , TC ("(if)", Nothing)
    , TC ("(if 1)", Nothing)
    , TC ("(if (if 1) 2 3)", Nothing)
    , TC ("(if #t 1 (if))", Just "1")
    , TC ("(if #f (if) 1)", Just "1")
    ]

--------------------------------------------------------------------------------
-- (6) cond
--------------------------------------------------------------------------------

condTests :: TestCases
condTests =
    [ TC ("(cond [else 1])", Just "1")
    , TC ("(cond [#t 1] [else 2])", Just "1")
    , TC ("(cond [#f 1] [#t 2])", Just "2")
    , TC ("(cond [#t 1] [#f 2])", Just "1")
    , TC ("(cond [(cond [else #t]) 1] [else 2])", Just "1")
    , TC ("(cond [(cond [else #f]) 1] [else 2])", Just "2")
    , TC ("(cond [#t (cond [#f 1] [else 2])] [else 3])", Just "2")
    , TC ("(cond)", Nothing)
    , TC ("(cond [1 2 3])", Nothing)
    , TC ("(cond [else 1] [1 2 3])", Nothing)
    , TC ("(cond [#t 1] [#f (cond)])", Just "1")
    , TC ("(cond [#f (cond)] [#t 1])", Just "1")
    ]

--------------------------------------------------------------------------------
-- (7) and
--------------------------------------------------------------------------------

andTests :: TestCases
andTests =
    [ TC ("(and)", Just "#t")
    , TC ("(and #t)", Just "#t")
    , TC ("(and #f)", Just "#f")
    , TC ("(and #t #f)", Just "#f")
    , TC ("(and #f #t)", Just "#f")
    , TC ("(and 1 2)", Just "2")
    , TC ("(and 3 2 1)", Just "1")
    , TC ("(and (and 3 2) 1)", Just "1")
    , TC ("(and 1 (and 2 3))", Just "3")
    , TC ("(and #f (if))", Just "#f")
    , TC ("(and (if) #f)", Nothing)
    , TC ("(and 1 (if) 2)", Nothing)
    , TC ("(and 1 2 (if))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (8) or.
--------------------------------------------------------------------------------

orTests :: TestCases
orTests =
    [ TC ("(or)", Just "#f")
    , TC ("(or #f #t)", Just "#t")
    , TC ("(or #t #f)", Just "#t")
    , TC ("(or 1 2 3)", Just "1")
    , TC ("(or #f 1 2)", Just "1")
    , TC ("(or #f #f #f)", Just "#f")
    , TC ("(or (or #t #f) #f)", Just "#t")
    , TC ("(or (or 1 2) #f)", Just "1")
    , TC ("(or 1 (or #t #f))", Just "1")
    , TC ("(or 1 (if))", Just "1")
    , TC ("(or (if) 1)", Nothing)
    , TC ("(or #f (if))", Nothing)
    ]

-- #############################################################################
-- Primitive functions
-- #############################################################################

--------------------------------------------------------------------------------
-- (9) eq?
--------------------------------------------------------------------------------

eqTests :: TestCases
eqTests =
    [ TC ("(eq? #t #t)", Just "#t")
    , TC ("(eq? #f #f)", Just "#t")
    , TC ("(eq? 0 #t)", Just "#f")
    , TC ("(eq? 0 #f)", Just "#f")
    , TC ("(eq? (cons #t #f) (cons #t #f))", Just "#t")
    , TC ("(eq? (cons #t #f) (cons #f #t))", Just "#f")
    , TC ("(eq? (cons 1 (cons 2 3)) (cons 1 (cons 3 2)))", Just "#f")
    , TC ("(eq? (cons 1 (cons 2 3)) (cons 1 (cons 2 3)))", Just "#t")
    , TC ("(eq? #t #t #t)", Nothing)
    , TC ("(eq? #t)", Nothing)
    , TC ("(eq? (eq? 1 1) #t)", Just "#t")
    , TC ("(eq? (eq? 1 1) (eq? 1 2))", Just "#f")
    , TC ("(eq?)", Nothing)
    ]

--------------------------------------------------------------------------------
-- (10) +
--------------------------------------------------------------------------------
plusTests :: TestCases
plusTests =
    [ TC ("(+)", Just "0")
    , TC ("(+ 1)", Just "1")
    , TC ("(+ -1)", Just "-1")
    , TC ("(+ 1 2 -1 -2)", Just "0")
    , TC ("(+ (+ 1 2) (+ -1 -2))", Just "0")
    , TC ("(+ 1 (+ 2) (+ 3))", Just "6")
    , TC ("(+ 1 #t)", Nothing)
    , TC ("(+ #t 1)", Nothing)
    , TC ("(+ 1 2 (+) (+ #f))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (11) -
--------------------------------------------------------------------------------
minusTests :: TestCases
minusTests =
    [ TC ("(- 1)", Just "1")
    , TC ("(- -1)", Just "-1")
    , TC ("(- 0 1)", Just "-1")
    , TC ("(- 0 -1)", Just "1")
    , TC ("(- 3 2 1)", Just "0")
    , TC ("(- 3 -2 -1)", Just "6")
    , TC ("(- 3 (- 2))", Just "1")
    , TC ("(- 3 (- 0 2))", Just "5")
    , TC ("(-)", Nothing)
    , TC ("(- 1 (-))", Nothing)
    , TC ("(- 1 #t)", Nothing)
    , TC ("(- 1 (- 1 #t))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (12) *
--------------------------------------------------------------------------------
timesTests :: TestCases
timesTests =
    [ TC ("(*)", Just "1")
    , TC ("(* 1)", Just "1")
    , TC ("(* 2 3)", Just "6")
    , TC ("(* 1 2 3)", Just "6")
    , TC ("(* 1 2 (* 3))", Just "6")
    , TC ("(* (*) 2 (* 3 4))", Just "24")
    , TC ("(* 1 #t)", Nothing)
    , TC ("(* #t 1)", Nothing)
    , TC ("(* (*) #t)", Nothing)
    , TC ("(* (*) (* #t))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (13) =
--------------------------------------------------------------------------------
equalsTests :: TestCases
equalsTests =
    [ TC ("(= 1)", Just "#t")
    , TC ("(= 1 2)", Just "#f")
    , TC ("(= 1 1 2)", Just "#f")
    , TC ("(= 1 1 1)", Just "#t")
    , TC ("(= 1 (+ 0 1) (- 1 0))", Just "#t")
    , TC ("(=)", Nothing)
    , TC ("(= 1 #t)", Nothing)
    , TC ("(= #t)", Nothing)
    , TC ("(= #t #t)", Nothing)
    , TC ("(= 1 (= 1 1))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (14) <, <=, >, and >=
--------------------------------------------------------------------------------
comparisonTests :: TestCases
comparisonTests =
    [ TC ("(< 1)", Just "#t")
    , TC ("(< 1 2)", Just "#t")
    , TC ("(< 1 2 3)", Just "#t")
    , TC ("(< 1 2 2)", Just "#f")
    , TC ("(< -1 1 3)", Just "#t")
    , TC ("(< 3 1 -1)", Just "#f")
    , TC ("(<)", Nothing)
    , TC ("(< 1 #t)", Nothing)
    , TC ("(< #f 1)", Nothing)
    , TC ("(< 1 (< 2 3))", Nothing)
    , TC ("(<= 1)", Just "#t")
    , TC ("(<= 1 2)", Just "#t")
    , TC ("(<= 1 2 3)", Just "#t")
    , TC ("(<= 1 2 2)", Just "#t")
    , TC ("(<= -1 1 3)", Just "#t")
    , TC ("(<= 3 1 -1)", Just "#f")
    , TC ("(<=)", Nothing)
    , TC ("(<= 1 #t)", Nothing)
    , TC ("(<= #f 1)", Nothing)
    , TC ("(<= 1 (<= 2 3))", Nothing)
    , TC ("(> 1)", Just "#t")
    , TC ("(> 1 2)", Just "#f")
    , TC ("(> 1 2 3)", Just "#f")
    , TC ("(> 1 2 2)", Just "#f")
    , TC ("(> -1 1 3)", Just "#f")
    , TC ("(> 3 1 -1)", Just "#t")
    , TC ("(> 3 1 1)", Just "#f")
    , TC ("(>)", Nothing)
    , TC ("(> 1 #t)", Nothing)
    , TC ("(> #f 1)", Nothing)
    , TC ("(> 1 (> 2 3))", Nothing)
    , TC ("(>= 1)", Just "#t")
    , TC ("(>= 1 2)", Just "#f")
    , TC ("(>= 1 2 3)", Just "#f")
    , TC ("(>= 1 2 2)", Just "#f")
    , TC ("(>= -1 1 3)", Just "#f")
    , TC ("(>= 3 1 -1)", Just "#t")
    , TC ("(>= 3 1 1)", Just "#t")
    , TC ("(>=)", Nothing)
    , TC ("(>= 1 #t)", Nothing)
    , TC ("(>= #f 1)", Nothing)
    , TC ("(>= 1 (>= 2 3))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (15) cons
--
-- Note that our parser treats improper lists and nested cons's identically, so
-- "(1 . (2 . 3))" will be parsed as a correct answer to the third test case.
--------------------------------------------------------------------------------
consTests :: TestCases
consTests =
    [ TC ("(cons 1 2)", Just "(1 . 2)")
    , TC ("(cons (cons 1 2) 3)", Just "((1 . 2) . 3)")
    , TC ("(cons 1 (cons 2 3))", Just "(1 2 . 3)")
    , TC ("(cons #t (cons #t #t))", Just "(#t #t . #t)")
    , TC ("(cons 1 (cons #t (cons 3 4)))", Just "(1 #t 3 . 4)")
    , TC ("(cons (cons (-) 1) 2)", Nothing)
    , TC ("(cons (-) (cons 1 2))", Nothing)
    , TC ("(cons)", Nothing)
    , TC ("(cons 1)", Nothing)
    , TC ("(cons 1 2 3)", Nothing)
    ]

--------------------------------------------------------------------------------
-- (16) fst
--------------------------------------------------------------------------------
fstTests :: TestCases
fstTests =
    [ TC ("(fst (cons 1 2))", Just "1")
    , TC ("(fst (cons 2 1))", Just "2")
    , TC ("(fst (cons (cons 1 2) 3))", Just "(1 . 2)")
    , TC ("(fst (cons 1 (cons 2 3)))", Just "1")
    , TC ("(fst (cons #t (cons 2 3)))", Just "#t")
    , TC ("(fst (fst (cons (cons 1 2) 3)))", Just "1")
    , TC ("(fst 1)", Nothing)
    , TC ("(fst #t)", Nothing)
    , TC ("(fst (fst (cons 1 (cons 2 3))))", Nothing)
    , TC ("(fst (cons (if) 1))", Nothing)
    , TC ("(fst)", Nothing)
    , TC ("(fst 1 2)", Nothing)
    , TC ("(fst (cons 1 2) 2)", Nothing)
    , TC ("(fst 1 (cons 1 2))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (17) snd
--------------------------------------------------------------------------------
sndTests :: TestCases
sndTests =
    [ TC ("(snd (cons 1 2))", Just "2")
    , TC ("(snd (cons 2 1))", Just "1")
    , TC ("(snd (cons (cons 1 2) 3))", Just "3")
    , TC ("(snd (cons 1 (cons 2 3)))", Just "(2 . 3)")
    , TC ("(snd (cons 1 (cons #t 3)))", Just "(#t . 3)")
    , TC ("(snd (fst (cons (cons 1 2) 3)))", Just "2")
    , TC ("(snd (snd (cons 1 (cons 2 3))))", Just "3")
    , TC ("(snd 1)", Nothing)
    , TC ("(snd #t)", Nothing)
    , TC ("(snd (snd (cons (cons 1 2) 3)))", Nothing)
    , TC ("(snd (cons 1 (cons 2 (if))))", Nothing)
    , TC ("(snd)", Nothing)
    , TC ("(snd 1 2)", Nothing)
    , TC ("(snd (cons 1 2) 2)", Nothing)
    , TC ("(snd 1 (cons 1 2))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (18) list
--------------------------------------------------------------------------------

listFTests :: TestCases
listFTests =
    [ TC ("(list)", Just "()")
    , TC ("(list 1)", Just "(1)")
    , TC ("(list 1 2)", Just "(1 2)")
    , TC ("(list (list 1 2))", Just "((1 2))")
    , TC ("(list 1 (list 2 3))", Just "(1 (2 3))")
    , TC ("(list #t #f)", Just "(#t #f)")
    , TC ("(fst (list 1 2))", Just "1")
    , TC ("(snd (list 1 2))", Just "(2)")
    , TC ("(list 1 (if) 2)", Nothing)
    , TC ("(list 1 (list 2 (if)))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (19) number?
--------------------------------------------------------------------------------
numberTests :: TestCases
numberTests =
    [ TC ("(number? 1)", Just "#t")
    , TC ("(number? #f)", Just "#f")
    , TC ("(number? (cons 1 2))", Just "#f")
    , TC ("(number? (fst (cons 1 2)))", Just "#t")
    , TC ("(number? (+ 1 2))", Just "#t")
    , TC ("(number? (number? 1))", Just "#f")
    , TC ("(number? (if))", Nothing)
    , TC ("(number?)", Nothing)
    , TC ("(number? 1 2)", Nothing)
    ]

--------------------------------------------------------------------------------
-- (20) boolean?
--------------------------------------------------------------------------------
boolQTests :: TestCases
boolQTests =
    [ TC ("(boolean? #t)", Just "#t")
    , TC ("(boolean? #f)", Just "#t")
    , TC ("(boolean? 1)", Just "#f")
    , TC ("(boolean? 0)", Just "#f")
    , TC ("(boolean? (cons 1 2))", Just "#f")
    , TC ("(boolean? (and 1 2))", Just "#f")
    , TC ("(boolean? (and 1 #f))", Just "#t")
    , TC ("(boolean? (boolean? 1))", Just "#t")
    , TC ("(boolean?)", Nothing)
    , TC ("(boolean? #t #t)", Nothing)
    , TC ("(boolean? (boolean?))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (21) pair?
--------------------------------------------------------------------------------
pairTests :: TestCases
pairTests =
    [ TC ("(pair? 1)", Just "#f")
    , TC ("(pair? #t)", Just "#f")
    , TC ("(pair? (cons 1 2))", Just "#t")
    , TC ("(pair? (list 1 2))", Just "#t")
    , TC ("(pair? (fst (list 1 2)))", Just "#f")
    , TC ("(pair? (snd (list 1 2)))", Just "#t")
    , TC ("(pair? (snd (list 1)))", Just "#f")
    , TC ("(pair? 1 2)", Nothing)
    , TC ("(pair? (cons 1 2) 2)", Nothing)
    , TC ("(pair?)", Nothing)
    , TC ("(pair? (cons (pair? 1)))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (21.5) nil?
--------------------------------------------------------------------------------

nilTests :: TestCases
nilTests =
    [ TC ("(nil? 1)", Just "#f")
    , TC ("(nil? #t)", Just "#f")
    , TC ("(nil? #f)", Just "#f")
    , TC ("(nil? (list 1))", Just "#f")
    , TC ("(nil? (fst (list 1)))", Just "#f")
    , TC ("(nil? (snd (list 1)))", Just "#t")
    , TC ("(nil?)", Nothing)
    , TC ("(nil? (fst (list 1)) (snd (list 1)))", Nothing)
    , TC ("(nil? (nil?))", Nothing)
    , TC ("(nil? (snd (list 1)) (fst (list 1)))", Nothing)
    ]

--------------------------------------------------------------------------------
-- (22) list?
--------------------------------------------------------------------------------
listQTests :: TestCases
listQTests =
    [ TC ("(list? 1)", Just "#f")
    , TC ("(list? #f)", Just "#f")
    , TC ("(list? (list 1 2))", Just "#t")
    , TC ("(list? (cons 1 (list 2 3)))", Just "#t")
    , TC ("(list? (cons (list 1 2) 3))", Just "#f")
    , TC ("(list? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 (cons 11 (cons 12 (cons 13 (cons 14 (cons 15 (cons 16 (cons 17 (cons 18 (cons 19 (cons 20 (list 21))))))))))))))))))))))", Just "#t")
    , TC ("(list? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 (cons 11 (cons 12 (cons 13 (cons 14 (cons 15 (cons 16 (cons 17 (cons 18 (cons 19 (cons 20 21)))))))))))))))))))))", Just "#f")
    ]

-- #############################################################################
-- Project 1 challenge
-- #############################################################################

-- See ChallengeTests.hs

-- #############################################################################
-- Project 2
-- #############################################################################

---------------------------------------------------------------------------------
-- Local definitions

letStarTests :: TestCases
letStarTests =

    [ TC ("(let* ([x 1]) x)", Just "1")
    , TC ("(let* ([x 1]) (+ x x))", Just "2")
    , TC ("(let* ([x 1]) y)", Nothing)
    , TC ("(let* [x 1] x)", Nothing)
    , TC ("(let* ([x 1]) x x)", Nothing)
    , TC ("(let* ([x 1 1]) x)", Nothing)
    , TC ("(let* ([x 1] [y 2]) y)", Just "2")
    , TC ("(let* ([x 1] [y 2]) x)", Just "1")
    , TC ("(let* ([x 1] [y 2]) (+ x y))", Just "3")
    , TC ("(let* ([x 1]) (let* ([y 2]) y))", Just "2")
    , TC ("(let* ([x 1]) (let* ([y 2]) (+ x y)))", Just "3")
    , TC ("(let* ([x 1]) (let* ([y (+ x x)]) y))", Just "2")
    , TC ("(let* ([x 1] [y (+ x x)]) (+ x y))", Just "3")
    ]

primOpVariableTests :: TestCases
primOpVariableTests =
    [ TC ("(let* ([f +]) (f 1 2 3))", Just "6")
    , TC ("(let* ([f and]) (f 1 2 3))", Nothing)
    , TC ("(let* ([eq? +]) (eq? 1 2 3))", Just "6")
    ]

lambdaTests :: TestCases
lambdaTests =
    [ TC ("((lambda (x) x) 1)", Just "1")
    , TC ("((lambda (x y) x) 1 2)", Just "1")
    , TC ("((lambda (x y) y) 1 2)", Just "2")
    , TC ("(lambda 1 x)", Nothing)
    , TC ("(lambda (x 1) x)", Nothing)
    , TC ("(lambda (x . 1) x)", Nothing)
    , TC ("(lambda (x y) x y)", Nothing)
    , TC ("(lambda (x))", Nothing)
    , TC ("((lambda (f x) (f x)) (lambda (x) x) 1)", Just "1")
    , TC ("((lambda x x) 1 2 3)", Just "(1 2 3)")
    , TC ("((lambda x (fst x)) 1 2 3)", Just "1")
    , TC ("((lambda x (fst (snd x))) 1 2 3)", Just "2")
    , TC ("((lambda (x y . z) (+ x y (fst z))) 1 2 3)", Just "6")
    , TC ("((lambda (x y . z) (+ x y)) 1 2)", Just "3")
    , TC ("((lambda (x y . z) (+ x y (fst z))) 1 2)", Nothing)
    , TC ("((lambda (x) ((lambda (y) (+ x y)) 2)) 1)", Just "3")
    ]

letStarLambdaTests :: TestCases
letStarLambdaTests =
    [ TC ("(let* ([f (lambda (x) (+ x 1))] [x 1]) (f x))", Just "2")
    , TC ("((lambda (x) (let* ([y (+ x x)]) (+ y y))) 2)", Just "8")
    , TC ("(let* ([f (lambda (x) (+ x 1))] [g (lambda (y) (+ (f y) (f y)))]) (g 2))", Just "6")
    , TC ("(let* ([x 1] [f (lambda (x y) (+ x y))] [g (lambda (z y) (+ x y))]) (eq? (f 2 1) (g 2 1)))", Just "#f")
    ]

applyTests :: TestCases
applyTests =
    [ TC ("(apply + (list 1 2 3))", Just "6")
    , TC ("(apply and (list 1 2 3))", Nothing)
    , TC ("(apply + 1 2 3 (list))", Just "6")
    , TC ("(apply + 1 (list 2 3))", Just "6")
    , TC ("(apply +)", Just "0")
    , TC ("(apply (lambda (x) x) (list 1))", Just "1")
    , TC ("((lambda (x . y) (apply + x y)) 1 2 3)", Just "6")
    , TC ("((lambda (x .  y) (apply + x x y)) 1 2 3)", Just "7")
    , TC ("(apply (lambda (x . y) (apply + x x y)) 1 (list 2 3))", Just "7")
    , TC ("(apply apply (list + 1 (list 2 3)))", Just "6")
    ]

--------------------------------------------------------------------------------
-- Randomized Definitions

split [] xs vs = []
split (n : ns) xs vs = zip (take n xs) (take n vs) : split ns (drop n xs) (drop n vs)

letStar :: [(String, String)] -> String -> String
letStar ps b = proper $ [symbol "let*"] ++
                        [proper [proper [symbol x, v] | (x, v) <- ps]] ++
                        [b]

letStarRTests :: Int -> TestCases
letStarRTests n =
    replicate n $
    "random number of let*s, no shadowing, single result" ~~
    do n <- pick [1..4]
       is <- replicateM n (pick [1..6])
       (cs, ds) <- unzip <$> replicateM (sum is) (pickValue 5)
       c <- pick ['a'..'z']
       let xs = take (sum is) [c : show i | i <- [1..]]
       z <- pick [0..length ds - 1]
       let s = foldr letStar (symbol (c : show (z + 1))) (split is xs cs)
           t = ds !! z
       return (s, Just t)

letStarNestedRTests :: Int -> TestCases
letStarNestedRTests n =
    replicate n $
    "nested let*" ~~
    do (s, t) <- letR 3
       return (s, Just t)
    where toTC (s, t) = (s, Just t)
          letR :: Int -> TestM (String, String)
          letR depth = do n <- pick [1..2]
                          is <- replicateM n (pick [1..3])
                          (cs, ds) <- unzip <$> replicateM (sum is) (if depth == 0
                                                                     then pickValue 3
                                                                     else pickFrom [ pickValue 3
                                                                                   , letR (depth - 1)])
                          c <- pick ['a'..'z']
                          let xs = take (sum is) [c : show i | i <- [1..]]
                          z <- pick [0..length ds - 1]
                          let s = foldr letStar (symbol (c : show (z + 1))) (split is xs cs)
                              t = ds !! z
                          return (s, t)

letStarRTestsShadowing :: Int -> TestCases
letStarRTestsShadowing n =
    replicate n $
    "random number of let*s, shadowing, multiple results" ~~
    do n <- pick [1..4]
       is <- replicateM n (pick [1..6])
       (cs, ds) <- unzip <$> replicateM (sum is) (pickValue 5)
       xs <- pickSymbolsUnique (max (sum is `div` 2) 1)
       ys <- replicateM (sum is) (pick xs)
       m <- pick [1..4]
       vs <- replicateM m (pick ys)
       let binds = reverse (zip vs ds)
           results = map (fromJust . flip lookup binds) vs
           s = foldr letStar (proper (symbol "list" : vs)) (split is vs cs)
       return (s, Just (proper results))
    where fromJust (Just x) = x

primOpVariableRTests :: Int -> Int -> TestCases
primOpVariableRTests m n = failure ++ success
  where
    failure = replicate m $
      "(let* (f $f) (f n1 n2 ... nm)) for $f in {and, eq?} and n_i in Nat. " ~~
      do n <- pick [5..15]
         xs <-replicateM n (pick [1..15])
         let xs' = map number xs
         f <- pickSymbol
         opSymbol <- pick ["and", "eq?", "cond", "if", "or"]
         return (
           proper [symbol "let*", proper [proper [symbol f, opSymbol]], proper (symbol f : xs')],
           Nothing
           )
    success = replicate n $
      "(let* (f $f) (f n1 n2 ... nm)) for $f in {+, *} and n_i in Nat. " ~~
      do n <- pick [5..15]
         xs <-replicateM n (pick [1..15])
         let xs' = map number xs
         f <- pickSymbol
         (opSymbol, op, e) <- pick [(symbol "+", (+), 0), (symbol "*", (*), 1)]
         return (
           proper [symbol "let*", proper [proper [symbol f, opSymbol]], proper (symbol f : xs')],
           Just . show $ foldr op e xs
           )

lambdaConstRTests :: Int -> TestCases
lambdaConstRTests n = replicate n $
  "random number of arguments, all values, pick 1 for the body" ~~
    do n <- pick [1..10]
       prefix <- pick ['a'..'z']
       let varNames = map ((prefix :) . show) $ [1..n]
       i <- pick [0..(n-1)]
       args :: [(String,String)] <- replicateM n (pickValue 0)
       let lam = lambda varNames (varNames !! i)
       return (applyImplicit lam (map fst args), Just (snd (args !! i)))

lambdaBadParamsRTests :: Int -> TestCases
lambdaBadParamsRTests n = replicate n $
  "random number of arguments, some args malformed" ~~
  do n <- pick [1..15]
     i <- pick [0..(n-1)]
     goodVars :: [String] <- pickSymbolsUnique i
     badVars  :: [String] <- map snd <$> replicateM (n - i) (pickValue 1)
     body <- fst <$> pickValue 2
     return (lambda (goodVars ++ badVars) body, Nothing)

lambdaNestedPermuteRTests :: Int -> TestCases
lambdaNestedPermuteRTests n = replicate n $
  "subtraction with parameters permuted and dropped" ~~
  do n <- pick [2..10]
     params :: [String] <- pickSymbolsUnique n
     args <- replicateM n (pick [-100..100])
     (params1,args1) <- unzip <$> shuffle (zip params args)
     (params2,args2) <- unzip <$> shuffle (zip params args1)
     let val = head args2 - sum (tail args2)
     return (applyImplicit
               (lambda params
                (applyImplicit
                 (lambda params (applyImplicit (symbol "-") params2))
                 params1))
               (map number args)
            , Just (number val))


-- a function which sums its normal arguments and an arbitrarily selected
-- element of its catchall argument
lambdaImproperRTests :: Int -> TestCases
lambdaImproperRTests n = replicate n $
  "lambda with an improper parameter list" ~~
  do n <- pick [1..5]
     m <- pick [1..5]
     params :: [String] <- pickSymbolsUnique (n + 1)
     let catchAll = last params
     idx <- pick [0..(m-1)]
     let z = fst' (foldr ($) catchAll (replicate idx snd'))
     args1 :: [String] <- replicateM n pickNumber
     args2 :: [String] <- replicateM m pickNumber
     let args1Eval :: [Integer] = map read args1
     let args2Eval :: [Integer] = map read args2
     -- let args2' :: String = applyImplicit (symbol "list") args2
     let zEval = args2Eval !! idx
     let fun = lambdaI (init params) catchAll (applyImplicit (symbol "+") (init params ++ [z]))
     return (applyImplicit fun (args1 ++ args2), Just (number (sum (args1Eval ++ [zEval]))))

letStarLambdaRTests :: Int -> Int -> TestCases
letStarLambdaRTests n m =
  (replicate n $
    "let* with lambda, where the second uses the first" ~~
    do fs :: [String] <- pickSymbolsUnique 2
       xs :: [String] <- pickSymbolsUnique 1
       ys :: [String] <- pickSymbolsUnique 2
       let [f1,f2] = fs
       let [x1] = xs
       let [y1,y2] = ys
       let bind1 :: (String,String) = (f1 , lambda [x1] (applyImplicit (symbol "*") [x1,x1]))
       let bind2 :: (String,String) =
             (f2 , lambda [y1,y2]
                     (applyImplicit (symbol "+")
                       [ applyImplicit (symbol f1) [y1]
                       , applyImplicit (symbol f1) [y2]]))
       n1 :: String <- pickNumber
       let n1Eval :: Integer = read n1
       n2 :: String <- pickNumber
       let n2Eval :: Integer = read n2
       return ( llet [bind1,bind2] (applyImplicit (symbol f2) [n1,n2])
              , Just . show $ (n1Eval * n1Eval) + (n2Eval * n2Eval)))
  ++ (replicate m $
     "shadowing of lambda-bound parameters by let" ~~
     do xs :: [String] <- pickSymbolsUnique 2
        let [x1,x2] = xs
        nums <- replicateM 3 pickNumber
        let [n1,n2,n3] = nums
        let [n1Eval,n2Eval,n3Eval] :: [Integer] = map read nums
        let bind :: (String,String) = (x1, applyImplicit (symbol "+") [symbol x1, n3])
        let body = applyImplicit (symbol "-") [x1, x2]
        return ( applyImplicit (lambda xs (llet [bind] body)) [n1,n2]
               , Just . number $ (n1Eval + n3Eval) - n2Eval)
  )

bindingFunctionsTests :: Int -> TestCases
bindingFunctionsTests n =
    replicate n $
    "binding functions in let*" ~~
    do n <- pick [1..6]
       m <- pick [1..8]
       fs <- pickSymbolsUnique m
       (bodies, js) <- unzip <$> replicateM m (lam n)
       (args, vs) <- unzip <$> replicateM n (pickValue 5)
       (f, j) <- pick (zip fs js)
       return (proper [ symbol "let*"
                      , proper [proper [symbol f, body] | (f, body) <- zip fs bodies]
                      , proper (symbol f : args) ], Just (vs !! j))

    where lam n =
            do m <- pick [0..n-1]
               xs <- pickSymbolsUnique n
               return (proper [symbol "lambda",
                               proper xs,
                               xs !! m], m)

applyRTests :: Int -> Int -> TestCases
applyRTests m n = failure ++ success
  where
    inputs =
      do n <- pick [5..15]
         xs <- replicateM n (pick [1..15])
         let xs' :: [String] = map number xs
         (opSymbol, op, e) <- pick [(symbol "+", (+), 0), (symbol "*", (*), 1)]
         return (n, xs, xs', opSymbol, op, e)

    failure = (replicate m $
      "apply int valued prim op to lists" ~~
      do (n, xs, xs', opSymbol, op, e) <- inputs
         return
           (proper [symbol "apply", opSymbol, proper xs'], Nothing)
              )
           ++
      (replicate m $
       "apply without function" ~~
       do (n, xs, xs', opSymbol, op, e) <- inputs
          return
            (proper [symbol "apply", proper xs'], Nothing)
      )

    success = (replicate n $
      "apply int valued prim op to lists" ~~
      do (n, xs, xs', opSymbol, op, e) <- inputs
         return
              (proper [symbol "apply", opSymbol, proper ([symbol "list"] ++ xs')], Just . show . foldr op e $ xs))
      ++
      (replicate n $
       "apply int valued prim op to lists" ~~
       do (n, xs, xs', opSymbol, op, e) <- inputs
          return
              (proper $ [symbol "apply", opSymbol] ++ xs' ++ [proper [symbol "list"]], Just . show . foldr op e $ xs))
      ++
      (replicate n $
       "apply int valued prim op to lists" ~~
       do (n, xs, xs', opSymbol, op, e) <- inputs
          return $
              let (y : ys) = xs' in (proper [symbol "apply", opSymbol, y, proper ([symbol "list"] ++ ys)], Just . show . foldr op e $ xs))
      ++
      (replicate n $
       "apply int valued prim op to lists" ~~
       do (n, xs, xs', opSymbol, op, e) <- inputs
          return $
              let (y : ys) = xs' in
                (proper $ [symbol "apply", symbol "apply", proper ([symbol "list", opSymbol, y, proper (symbol "list" : ys)])],
                 Just . show . foldr op e $ xs))

applyLambdaRTests :: Int -> TestCases
applyLambdaRTests n = success
  where
    apply :: String = symbol "apply"
    lam   :: String = symbol "lambda"
    x     :: String = symbol "x"
    list  :: String = symbol "list"

    success :: TestCases
    success = (replicate n $
      "(apply (lambda (x) x) (list n_1 ... n_m))" ~~
       do
         m <- pick [5..15]
         xs <- replicateM m (pick [1..10])
         let
           xs' :: [String] = map number xs
           lamxx = proper [lam, improper [] x, x]
         return $
                (proper [apply, lamxx, proper (list : xs')],
                 Just (proper xs')))
    --               , TC ("((lambda (x . y) (apply + x y)) 1 2 3)", Just "6")
    -- , TC ("((lambda (x .  y) (apply + x x y)) 1 2 3)", Just "7")
    -- , TC ("(apply (lambda (x . y) (apply + x x y)) 1 (list 2 3))", Just "7")
    -- , TC ("(apply apply (list + 1 (list 2 3)))", Just "6")

bindingFunctionsApplyTests :: Int -> TestCases
bindingFunctionsApplyTests n =
    replicate n $
    "binding functions in let*, with apply" ~~
    do n <- pick [1..6]
       m <- pick [1..8]
       fs <- pickSymbolsUnique m
       (bodies, js) <- unzip <$> replicateM m (lam n)
       (args, vs) <- unzip <$> replicateM n (pickValue 5)
       (f, j) <- pick (zip fs js)
       k <- pick [0..m `div` 2]
       return (proper [ symbol "let*"
                      , proper [proper [symbol f, body] | (f, body) <- zip fs bodies]
                      , proper ([symbol "apply", symbol f] ++
                                take k args ++
                                [proper (symbol "list" : drop k args)]) ], Just (vs !! j))

    where lam n =
            do m <- pick [0..n-1]
               xs <- pickSymbolsUnique n
               return (proper [symbol "lambda",
                               proper xs,
                               xs !! m], m)

-- #############################################################################
-- Project 3
-- #############################################################################

--------------------------------------------------------------------------------
-- The begin special form (doesn't do much on its own)

beginTests :: TestCases
beginTests =
    [ TC ("(begin 1 2 3)", Just "3")
    , TC ("(begin)", Nothing)
    ]

---------------------------------------------------------------------------------
-- The set! special form

setBangTests :: TestCases
setBangTests =
    [ TC ("(let* [(x 1)] (begin (set! x 5) x))", Just "5")
    , TC ("(let* [(x 1) (y 2)] (begin (set! x (+ y y)) x))", Just "4")
    , TC ("(let* [(x 1)] (begin (set! x (+ x x)) x))", Just "2")
    , TC ("(let* [(x 1)] (list (let* [(x 2)] (begin (set! x 5) x)) x))", Just "(5 1)")
    , TC ("(begin (set! x 2) x)", Nothing)
    , TC ("((lambda (x) (begin (set! x 5) x)) 1)", Just "5")
    , TC ("((lambda (x y) (begin (set! y 2) (list x y))) 1 1)", Just "(1 2)")
    , TC ("((lambda (x) (begin (set! x (+ x x)) x)) 2)", Just "4")
    , TC ("((lambda (x) (list ((lambda (x) (begin (set! x 5) x)) x) x)) 1)", Just ("(5 1)"))
    , TC ("((lambda (x) (begin (set! y x) y)) 1)", Nothing)
    ]

---------------------------------------------------------------------------------
-- The define special form

defineTests :: TestCases
defineTests =
    [ TC ("(begin (define x 5) x)", Just "5")
    , TC ("(begin (define x 1) (define y (+ x x)) y)", Just "2")
    , TC ("(let* [(x 1)] (begin (define y (+ x x)) (list x y)))", Just ("(1 2)"))
    , TC ("(let* [(y 1)] (list (let* [(x 1)] (begin (define y (+ x x)) y)) y))", Just "(2 1)")
    , TC ("((lambda (x) (begin (define y (+ x x)) y)) 1)", Just "2")
    , TC ("((lambda (x) (begin (define x 5) x)) 1)", Just "5")
    , TC ("((lambda (x) (begin (define x (+ x x)) x)) 1)", Just "2")
    ]

--------------------------------------------------------------------------------

buildTests :: [(Int, TC)] -> IO Test
buildTests testCases = test <$> assertions
    where assertions :: IO [Assertion]
          assertions =  do g <- getStdGen
                           return (evalState (sequence [check i tc | (i, tc) <- testCases]) g)

buildTestsSeeded :: Int -> [(Int, TC)] -> IO Test
buildTestsSeeded seed testCases = test <$> assertions
    where assertions :: IO [Assertion]
          assertions = return (evalState (sequence [check i tc | (i, tc) <- testCases]) (mkStdGen seed))

p1Tests = concat
          [ constTests, ifTests, condTests, andTests, orTests, eqTests, plusTests,
            minusTests, timesTests, equalsTests, comparisonTests, consTests,
            fstTests, sndTests, listFTests, numberTests, boolQTests, pairTests,
            nilTests, listQTests ]

p2Tests = concat
          [ letStarTests, primOpVariableTests, lambdaTests, letStarLambdaTests,
            applyTests ]

p2RTests = concat
           [ -- let* tests: 30/100
             letStarRTests 7, letStarRTestsShadowing 7, primOpVariableRTests 3 5, letStarNestedRTests 8
             -- lambda tests: 30/100
           , lambdaConstRTests 9, lambdaBadParamsRTests 3, lambdaNestedPermuteRTests 9, lambdaImproperRTests 9
             -- mixing let* and lambda: 20/100
           , letStarLambdaRTests 3 7, bindingFunctionsTests 10
             -- apply: 20/100
           , applyRTests 2 2, applyLambdaRTests 4, bindingFunctionsApplyTests 4 ]

p3Tests = concat [beginTests, setBangTests, defineTests]

p1Start, p1End, c1Start, c1End, p2Start, p2End :: Int
p1Start = 0
p1End   = length p1Tests
c1Start = p1End
c1End   = c1Start + length c1Tests
p2Start = c1End
p2End   = p2Start + length p2Tests
p2RStart = p2End
p2REnd   = p2RStart + length p2RTests
c2Start = p2REnd
c2End = c2Start + length c2Tests
p3Start = c2End
p3End = p3Start + length p3Tests

allTests = zip [0..] $ concat [p1Tests, c1Tests, p2Tests, p2RTests, c2Tests, p3Tests]

testSets =
    [ ("p1", [p1Start, p1End])
    , ("c1", [c1Start, c1End])
    , ("p2", [p2Start, p2End])
    , ("p2r", [p2RStart, p2REnd])
    , ("c2", [c2Start, c2End])
    , ("p3", [p3Start, p3End]) ]

main :: IO ()
main = do allArgs <- getArgs
          let (flags, args) = partition (('-' ==) . head) allArgs
              builder = case filter (("-s" ==) . take 2) flags of
                          ((_ : _ : ss) : _) -> buildTestsSeeded (read ss)
                          _                  -> buildTests
              (makeJson, testIdxs)  = partition ("json" ==) args
              tests | null testIdxs = allTests -- pickTests [p1Start, p1End, p2Start, p2End]
                    | otherwise     = pickTests (ranges testIdxs)
          if not (null makeJson)
          then putStrLn (testsJson tests)
          else do results <- runTestTT =<< builder tests
                  if errors results + failures results == 0
                  then exitWith ExitSuccess
                  else exitWith (ExitFailure 1)

  where ranges [] = []
        ranges (s : ss)
            | Just idxs <- lookup s testSets = go idxs ++ ranges ss
            | Just i <- readMaybe s          = i : ranges ss
            | otherwise                      = error "Unknown ranges"
            where go []           = []
                  go (i : j : ks) = i : j : go ks

        pickTests (i : j : ks) = between i j ++ pickTests ks
        pickTests [i]          = [allTests !! i]
        pickTests []           = []

        between i j = take (j - i) (drop i allTests)

        testsJson tests =
            unlines [ "{\"name\" : \"" ++ label n t ++ "\", \"setup\" : \"\", \"run\" : \"cabal run Test " ++ show n ++ "\", \"input\" : \"\", \"output\" : \"\", \"comparison\" : \"included\", \"timeout\" : 0.5, \"points\" : 1 },"
                    | (n, t) <- tests
                    ]
