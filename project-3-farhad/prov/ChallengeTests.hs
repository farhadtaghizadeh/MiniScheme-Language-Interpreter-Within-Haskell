module ChallengeTests (c1Tests, c2Tests) where

import System.Exit
import System.Environment ( getArgs )
import Test.HUnit

import Checker
import Parser

type TestCases = [TC]

--------------------------------------------------------------------------------
-- (1) Quote special form: Constants & basic constructions
--------------------------------------------------------------------------------

-- Can quote constants
bool'Tests :: TestCases
bool'Tests = [
  TC ("'#t", Just "#t"),
  TC ("'#f", Just "#f")
  ]

var'Tests :: TestCases
var'Tests = [
  TC ("'x", Just "x"),
  TC ("'abc", Just "abc"),
  TC ("'f1", Just "f1"),
  TC ("(quote f1)", Just "f1")
  ]

num'Tests :: TestCases
num'Tests = [
  TC ("'3", Just "3"),
  TC ("'123", Just "123"),
  TC ("'-1", Just "-1"),
  TC ("(quote -1)", Just "-1")
  ]

--------------------------------------------------------------------------------
--  Quote special form: Quoting primitive and special forms
--------------------------------------------------------------------------------

improperLists'Tests :: TestCases
improperLists'Tests = [
  TC ("'(d1 . (d2 . (d3 . d4)))", Just "(d1 . (d2 . (d3 . d4)))"),
  TC ("'(d1 d2 d3 . d4)", Just "(d1 d2 d3 . d4)"),
  TC ("(quote (x . y))", Just "(x . y)")
  ]

properLists'Tests :: TestCases
properLists'Tests = [
  TC ("'(1 2 3)", Just "(1 2 3)"),
  TC ("'(x y)", Just "(x y)"),
  TC ("'()", Just "()"),
  TC ("(quote (1 2 3))", Just "(1 2 3)")
  ]

-- The act of quoting primitives is (or should be) actually no different than
-- quoting a list, but it's worth checking these are *really* not executed.
prim'Tests :: TestCases
prim'Tests = [
  TC ("'(eq? 1 2)", Just "(eq? 1 2)"),
  TC ("'(+ 1 (+ 2 3))", Just "(+ 1 (+ 2 3))"),
  TC ("'(= 1 2 3)", Just "(= 1 2 3)"),
  TC ("(quote (number? (1 2 3)))", Just "(number? (1 2 3))")
  ]

specialForm'Tests :: TestCases
specialForm'Tests = [
  TC ("'(or 1 2)", Just "(or 1 2)"),
  TC ("(cond [(eq? 1 1) 1] '[else 2])", Just "1"),
  TC ("(cond ['(eq? 1 1) 3] '[else 2])", Just "3"),
  TC ("'(and 1 (and 2 3))", Just "(and 1 (and 2 3))"),
  TC ("(quote (cond [#f 1] [#t 2] [else 3]))", Just "(cond [#f 1] [#t 2] [else 3])")
  ]

--------------------------------------------------------------------------------
-- Quote special form: Nested quotes
--------------------------------------------------------------------------------

qqqqqqqqqquote n =
        concat $ replicate n "(quote " ++ ["x"] ++ replicate n ")"

nested'Tests :: TestCases
nested'Tests = [
  TC ("''x", Just "'x"),
  TC ("'''x", Just "''x"),
  TC ("'(1 '(2 3))", Just "(1 '(2 3))"),
  TC (qqqqqqqqqquote 10, Just (qqqqqqqqqquote 9))
  ]

--------------------------------------------------------------------------------
-- Eval primitive function:
--------------------------------------------------------------------------------

evalFailsTests :: TestCases
evalFailsTests = [
  TC ("(eval)", Nothing),
  TC ("(eval 1 2 3 4 5)", Nothing)
  ]

evalBoolTests = [
  TC ("(eval #t)", Just "#t"),
  TC ("(eval #f)", Just "#f")
  ]

evalNumTests = [
  TC ("(eval 0)", Just "0"),
  TC ("(eval 123)", Just "123"),
  TC ("(eval -1)", Just "-1")
  ]

-- We can't really express anything interesting without quotes.  MiniScheme will
-- eagerly evaluate its arguments, anyway -- so all we can test sans quotes is
-- that (eval t) works when t is a number of bool.
evalNoQuotingPrimTests =
  [
    TC ("(eval (fst (cons 1 2)))", Just "1"),
    TC ("(eval (snd (cons 1 2)))", Just "2"),
    TC ("(eval (snd (cons 1 2)))", Just "2")--,
    -- expected failures
    -- TC ("(eval (1 . 2))", Nothing),
    -- TC ("(eval (cons + (cons 1 (cons 2 ()))))", Nothing)
  ]

evalNoQuotingSpecialFormTests =
  [
    TC ("(eval (and #t #t #t #f))", Just "#f"),
    TC ("(eval (or #f #f #f 1))", Just "1"),
    TC ("(eval (cond [(eq? 1 1) 1] '[else 2]))", Just "1"),
    TC ("(eval (cons 1 (and #f #f #f x)))", Nothing)
  ]

-- The rest of these tests presume some form of quoting implemented.
-- Unfortunately, I do not believe we can test the correctness of eval
-- without evaluating quoted terms.
evalWithQuotingPrimTests =
  [
    TC ("(eval (cons '+ (cons 1 (cons 2 '()))))", Just "3"),
    TC ("(eval '(+ 1 2))", Just "3"),
    TC ("(eval '(fst (cons 1 2)))", Just "1"),
    TC ("(eval '(snd (cons 1 2)))", Just "2"),
    TC ("(eval '(snd (cons 1 2)))", Just "2"),
    TC ("(eval (cons 'list (cons 1 (cons 2 '()))))", Just "(1 2)"),
    TC ("(eval '(pair? '(1 . 2)))", Just "#t"),
    TC ("(list (eval '(+ 1 2)) 3)", Just "(3 3)")
  ]

evalWithQuotingSpecialFormTests =
  [
    TC ("(eval '(and #t #t #t #f))", Just "#f"),
    TC ("(eval '(or #f #f #f 1))", Just "1"),
    TC ("(eval '(cond [(eq? 1 1) 1] '[else 2]))", Just "1"),
    TC ("(eval '(cons 1 (and #f #f #f x)))", Just "(1 . #f)")
  ]

evalWithQuotingNestedTests =
  [
    TC ("(eval " ++ qqqqqqqqqquote 10 ++ ")", Just $ qqqqqqqqqquote 8),
    TC ("(eval (eval (eval 10)))", Just "10"),
    TC ("(eval (eval (eval '(and #t #f))))", Just "#f"),
    TC ("(+ (eval '(+ 1 2)) (eval '(+ 3 4)))", Just "10"),
    TC ("(eval (cons (cond [#t '+] [else '-]) (cons 1 (cons 2 '()))))", Just "3"),
    TC ("(eval (cons (cond [#f '+] [else '-]) (cons 1 (cons 2 '()))))", Just "-1")
  ]

--------------------------------------------------------------------------------
-- Splice special form:
--------------------------------------------------------------------------------


spliceFailsTests = [
  TC ("$()", Nothing),
  TC ("(splice e)", Nothing),
  TC ("$e", Nothing),
  TC ("$(+ 1 2)", Nothing)
  ]

spliceBoolTests = [
  TC ("'$#t", Just "#t"),
  TC ("'$#f", Just "#f")
  ]

spliceNumTests = [
  TC ("'$1", Just "1"),
  TC ("'$-1", Just "-1"),
  TC ("(quote (splice 123))", Just "123")
  ]

spliceImproperListsTests = [
  TC ("'$'(1 2 . rest)", Just "(1 2 . rest)"),
  TC ("(quote $(fst '(x . y)))", Just "x"),
  TC ("(quote $(snd '(x . y)))", Just "y")
  ]

spliceProperListsTests = [
  TC ("'$'(x y)", Just "(x y)"),
  TC ("'$'(eq 1 2)", Just "(eq 1 2)"),
  TC ("'$'(1 2 3)", Just "(1 2 3)"),
  TC ("'(splice (quote (x y)))", Just "(x y)"),
  TC ("'$()", Nothing)
  ]

splicePrimTests = [
  TC ("'(eq? $(eq? 1 2) #t)", Just "(eq? #f #t)"),
  TC ("(quote (splice (+ 1 (+ 2 3))))", Just "6"),
  TC ("'(+ $(fst '(x . y)) $(snd '(y . z)))", Just "(+ x z)"),
  TC ("'(* $(number? 1) $(number? 2))", Just "(* #t #t)")
  ]

spliceSpecialFormTests = [
  TC ("(or '$(and #t #f) '$(and #t #t))", Just "#t"),
  TC ("(and '$(or #t #f) '$(or #t #t))", Just "#t"),
  TC ("(and 'poo '$(+ 1 2))", Just "3"),
  TC ("'(and 'poo 'bar $(fst '(x . y)))", Just "(and 'poo 'bar x)")
  ]

spliceNestedTests = [
  TC ("'(x '(2 $(+ 2 3)))", Just "(x (quote (2 5)))"),
  TC ("'(x '(2 $(+ 2 3) $$(+ 2 3)))", Nothing),
  TC ("'(x '(2 $(+ 2 3) $(+ 2 6)))", Just "(x '(2 5 8))")
  ]

----------------------------------------------------------------------------------
-- Challenge 2 tests
--------------------------------------------------------------------------------

lambdaInLetTests =
  [ TC ("((let* [(x 1)] (lambda (y) (+ x y))) 2)", Just "3")
  , TC ("((let* [(x 1) (y 1)] \
        \    (lambda (y) (+ x y))) 2)", Just "3")
  , TC ("((let* [(x 1) (y 2)] \
        \    (lambda (x z) (+ x y z))) 1 3)", Just "6")
  , TC ("(let* [(x 1)                    \
        \       (f (lambda (y) (+ x y))) \
        \       (x 5)]                   \
        \   (f x))", Just "6")
  ]

lambdaInLambdaTests =
  [ TC ("(((lambda (x) (lambda (y) (+ x y))) 1) 2)", Just "3")
  , TC ("(let* [(o (lambda (f g) (lambda (x) (f (g x))))) \
       \        (inc (lambda (n) (+ n 1)))]               \
       \    ((o inc inc) 1))", Just "3")
  , TC ("(let* [(oo (lambda (f g) (lambda xs (f (apply g xs))))) \
        \       (inc (lambda (n) (+ n 1)))]                      \
        \   ((oo inc +) 1 2 3))", Just "7")
  , TC ("(((lambda (x)                   \
        \     (let* [(y (+ x x))]        \
        \         (lambda (z) (+ y z)))) \
        \  1) 2)", Just "4")
  ]




--------------------------------------------------------------------------------
-- export
--------------------------------------------------------------------------------

c1Tests :: TestCases
c1Tests = concat [
  -- quote tests
  bool'Tests, var'Tests, num'Tests,
  improperLists'Tests, properLists'Tests,
  prim'Tests, specialForm'Tests, nested'Tests,

  -- splice tests
  spliceFailsTests, spliceBoolTests, spliceNumTests,
  spliceImproperListsTests, spliceProperListsTests,
  splicePrimTests, spliceSpecialFormTests, spliceNestedTests,

  -- eval tests
  evalFailsTests, evalBoolTests, evalNumTests,
  evalNoQuotingPrimTests, evalNoQuotingSpecialFormTests,
  evalWithQuotingPrimTests, evalWithQuotingSpecialFormTests, evalWithQuotingNestedTests
  ]

c2Tests :: TestCases
c2Tests = concat
  [ lambdaInLetTests
  , lambdaInLambdaTests
  ]
