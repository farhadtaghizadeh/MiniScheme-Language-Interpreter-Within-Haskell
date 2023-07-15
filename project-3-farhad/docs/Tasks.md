# Project 3 Tasks

Project 3 includes four tasks, described here.  For detail on the language features in each task, you should also consult the description of [MiniScheme](MiniScheme.md).

The tasks in project 3 are more like those of project 2 than project 1: you will implement fewer special forms and primitive functions, but will require a deeper understanding of MiniScheme and your evaluator.

You should carefully read *all four tasks* before you begin modifying your code.

# Statefulness

Your most significant task is to extend your evaluator to support *modifying* user-defined variables.  Concretely, this means you will need to extend your evaluator to return a modified environment&mdash;mapping from names to data&mdash;as well as a result.

For example, if you previously had an evaluation function
```hs
eval :: Datum -> Environment -> Maybe Datum
```
then you might change this to:
```hs
eval :: Datum -> Environment -> Maybe (Datum, Environment)
```

You might observe that this looks suspiciously like the *State monad*, and indeed you could implement this part of the project monadically.  If so, you should probably consider using [the `mtl` library](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html).  You might end up with a type signature like the following (indeed, this is the type provided with the sample solution):

```hs
eval :: Datum -> StateT Env (Either Error) a
```

However, you do *not* need to use monads.  You should, however, consider writing a couple of helper functions to "thread" state through sequential evaluation.  At a minimum, I would recommend:
```hs
eval :: [Datum] -> Environment -> Maybe ([Datum], Environment)
```

## Environments and frames

There is another challenge to introducing statefulness.  Heretofore, you've been able to "restore" the environment after evaluating a `let*` or calling a `lambda` simply by "returning to" the original environment after you're done evaluating the `let*` or `lambda`.  To make this clearer, suppose you were evaluating an expression like:
```scheme
(let* [(x 1)]
   (list
      (let* [(x 2)] x)
      (let* [(y 2)] y)
      x))
```
This would evaluate to the list `(2 2 1)`.  But now think about the environment in which you evaluated each argument to `list`.  For each argument, we'll start with the *same* environment that we had when we started evaluating the `list`.  We may make *local* changes to that environment&mdash;in evaluating the second argument to `list`, for example, you extend that starting environment with a mapping from `x` to `2`.  But that extended environment doesn't persist into later calls.  In fact, the type of your `eval` function probably doesn't even make it possible for it to persist.

However, with mutation this is no longer the case.  Later in this file, you'll see more about `set!`, the special form that does mutation.  Here's an example of how it could be used:
```scheme
(let* [(x 1)]
   (list
      (let* [(x 2)] x)
      (let* [(y 2)]
         (begin
            (set! x y)
            y))
      x))
```
The result of evaluating this expression should be the list `(2 2 2)`.  The interesting thing is what happens in the second argument to  `list`.  First, the environment is extended with `y` mapped to `2`.  In the extended environment, we assign `x` to `2`.  Then, we leave the `let*`, and so need to discard the inner binding of `y`.  However, we should *not* simultaneously discard the change to the binding for `x`.

One approach here is to regard the environment as a list of *frames*, where each frame is a list of *bindings*.   That is, you might have a definition like:
```hs
type Environment = [[(String, Datum)]]
```
The good thing about this is that you can now keep all the bindings from a particular `let*` or `lambda` together.  When you "enter" a `let*` or `lambda` body, you'll start a new frame on the stack, and all the new bindings will go into that frame.  When you leave the `let*` or `lambda` body, you can pop the top frame off the environment, without having to know anything about how many bindings it contained.  There will also be a little more work in other places: you can no longer rely on `lookup` to find bindings for you, but will have to write a recursive function to traverse the environment yourself.


# The `begin` special form

Your first visible addition to project 3 is the `begin` special form.  The `begin` special form evaluates a number of MiniScheme expressions in a row; you can think of this as being equivalent to a `;` separated block of statements in your favorite imperative programming language.

So: `(begin e1 e2 e3)` first evaluates `e1`, then `e2`, finally `e3`.

The value resulting from evaluating a `begin` expression is the value resulting from its final expression.  For example, `(begin 1 2 3)` evaluates to `3`.

(At this point, it may not be obvious what purpose the `begin` special form serves&mdash;after all, why not just have the final expression?  However, `begin` will be essential to using the remaining features of this project.)

# The `set!` special form

Your next addition in project 3 is the `set!` special form.  `set!` is used to mutate variables that have already been introduced, either via `let*`, `lambda`, or (once you have implemented it) `define`.  `set!` is not used to define new variables.

The expression `(set! x e)`, where `x` is a symbol and `e` is any MiniScheme expression, updates the existing binding of `x` to contain the results of evaluating `e`.  The result value is the result of evaluating `e`.

For example, the expression
```scheme
(let* [(x 2)]
  (begin
    (set! x (+ x x))
    x))
```
evaluates to `4`.  To make the mutation of `x` more apparent, the example:
```scheme
(let* [(x 2)]
  (list
    x
    (begin
      (set! x (+ x x))
      x))))
```
evaluates to the list `(2 4)`.  The first `x` in the `list` appears before evaluating the `set!`, and so evaluates to the initial value of `x`.  The second `x` (inside the `begin`) occurs after evaluating the `set!`, and so evaluates to the final value of `x`.

# The `define` special form

Your final task in project 3 is the `define` special form.  `define` is used to add new bindings to the current environment&mdash;you can think of it as a version of `let*` that doesn't need a body, or simply as variable declaration in your favorite imperative programming language.

The expression `(define x e)` evaluates the body `e` and binds the result to the symbol `x`.  (This is true regardless of whether or not there's already a binding for `x` in the environment.)  The result of the `define` expression can be anything you want&mdash;we'll never test the value returned by `define`.

Here's an example that puts all the pieces together:
```scheme
(begin
   (define counter 0)
   (define count
      (lambda (n)
         (begin
            (set! counter (+ counter 1))
            (+ n counter))))
   (list (count 1) (count 2) (count 3)))
```
This expression evaluates to the list `(2 4 6)`, as `counter` is incremented with each call to `count`.



