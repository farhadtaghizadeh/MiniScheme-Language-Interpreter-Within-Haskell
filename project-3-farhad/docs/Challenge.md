# Project 1 Challenge Tasks

There are three challenge tasks for project 1, weighted equally.

## The `quote` special form

The first task is to implement a special form `quote`.  The `quote` special form simply returns its argument, unevaluated, as a Scheme datum.  For example, the expression `(quote (1 2 3))` returns the proper combination `(1 . (2 . (3 . ())))`.  Similarly, the proper expression `(quote (+ 1 2))` returns the proper combination `(+ . (1 . (2 . ())))`, where the first element in the combination is the *unevaluated* symbol `+`.

This provide a good opportunity to check that your `eq?` function is implemented properly: the expression

```scheme
(eq? (fst (quote (+ 1 2))) (quote +))
```

should evaluate to `#t`.

The provided parser supports two syntactic shortcuts for writing quoted expressions.  First, `'e` is equivalent to `(quote e)`.  So, the previous expression could have been written:

```scheme
(eq? (fst '(+ 1 2)) '+)
```

Second, a quoted list can be written in braces:

```scheme
(eq? (fst {+ 1 2}) '+)
```

You do not have to do anything extra to support this syntax.

## The `eval` primitive function

The second challenge task is to implement a primitive `eval`, which evaluates its argument as if it were a Scheme expression.  Note that evaluate is *not* a special form: in the expression `(eval e)`, first `e` is evaluated to find the argument to `eval`; second, the result of evaluating `e` is evaluated as if it were a Scheme expression.

Here is an example:

```scheme
(eval (cons '+ (cons 1 (cons 2 '()))))
```

The argument evaluates to the proper combination `(+ 1 2)`, (where `+` is so far an uninterpreted symbol).  Then, `eval` evaluates that resulting combination as a special form, producing 3.

## The `splice` special form

The final challenge task is to implement a special form `splice`.

This special form is unusual in that it is only meaningful *inside* a `quote` special form; `splice` outside of a `quote` fails to evaluate.

Inside a `quote`, the expression `(splice e)` is *not* included in the result of the `quote`; instead, the expression `e` is evaluated and its result is included in the `quote`.  For example, the term:

```scheme
{+ 1 (+ 1 2) 3}
```

evaluates to the proper combination `(+ 1 (+ 1 2) 3)`, where the `+`s are both uninterpreted symbols.  On the other hand, the term

```scheme
{+ 1 (splice (+ 1 2)) 3}
```

evaluates to the proper combination `(+ 1 3 3)`, where the spliced expression has been evaluated.

The provided parser supports a syntactic shortcut for writing splices: `$e` is equivalent to `(splice e)`.  So the above example could have been written `{+ 1 $(+ 1 2) 3}`.  You do not have to do anything extra to support this syntax.

# Project 2 Challenge Tasks

The challenge task for project 2 is to implement static scoping for `lambda`.

Static and dynamic scoping are different ways to interpret the variable references in a function definition.  Suppose that we have a function term that uses a variable that's *not* a parameter:
```scheme
(lambda (x) (+ x y))
```
Variable `x` (as the argument to `+`) is a parameter.  However, there are two ways to think about `y`:

* We could interpret it in the context in which the function is *called*&mdash;that is to say, each invocation of this function might see a different value of `y` (or indeed, not have a value of `y` at all).  This is called *dynamic scoping*, and is what is implemented in the non-challenge version of the project.

* Alternatively, we could interpret it in the context in which the function is *defined*&mdash;that is to say, invocations of the function all see the same value of `y`.  This is called *static scoping*.

Static scoping is more common in functional programming languages.  In fact, it is essential to how we have learned functional programming in Haskell.  Consider the "multiple argument" function
```hs
f x y = x + y
```
We know that this is actually interpreted as *nested* lambdas:
```hs
f = \x -> \y -> x + y
```
But note that in the inner lambda, the `x` is a variable from the context!  We can express the same thing in MiniScheme:
```scheme
(lambda (x) (lambda (y) (+ x y)))
```
But with dynamic scoping, this definition will not work: the `x` in the inner lambda will be interpreted at the *use* of the latter function.

The challenge problem, then, is to implement static scoping.  This modifies three parts of project 2: the representation of function objects, the `lambda` special form, and the implementation of function application.

## Closures

For the variables in a function to be interpreted in its defining environment, each function object needs to retain the environment in which it was defined.  This combination of a function object and its defining environment is known as a *closure*.

Your first task in the challenge problem is to extend your AST with closures (instead of function objects).  Your representation of closures is up to you; at a minimum, it consists of the parameters, the body of the function, and the environment in which the function was defined.  Your printer may represent closures however you want; we will only test the result of calling closures.

## The `lambda` special form

Your second task is to adapt the `lambda` special form to closures.  The behavior of `lambda` must be extended to capture not just the arguments and body, but also the environment at the point of definition.

## Application

Your third task is to adapt application to closures.  Application differs from its behavior in the base project in that the parameter-argument bindings extend the *saved* environment in the closure, not the environment at the point of the function call.  The body of the function is then evaluated in that extended environment, giving the result of the function call.

After the function call, the environment should be unchanged.

## Testing

The challenge problem for project 2 is assessed by the overall number of passing challenge tests.

# Project 3 Challenge Tasks

There are two independent challenge tasks for project 3.  In total, they can contribute up to **7%** to your semester grade.

Both challenge tasks *assume* that you have completed the challenge tasks for project 2.  If you have *not* completed the project 2 challenge tasks, then you will not be able to attempt the project 3 challenge tasks.

## Recursion (3%)

One of the surprising features of dynamic scoping is that recursion "just works".  For example, if you *haven't* implemented the project 2 challenge tasks, then the following program:
```scheme
(let* [(fact
          (lambda (n)
             (if (<= n 1) 1 (* n (fact (- n 1))))))]
   (fact 5))
```
ought to evaluate to `120`.  Why?  Well, when we evaluate `(fact 5)`, we're doing so in the environment *at the call site*, so the definition of `fact` is in scope.

Sadly, this is not the case if you implemented the project 2 challenge!  In your implementation, the call to `(fact 5)` will be evaluated in the environment in which `fact` was defined, which doesn't yet contain the definition of `fact`.

Unless it did, of course, which is the the first challenge task for project 3.  The task is to support recursive definition of functions in MiniScheme.

How should you accomplish this?  It depends on whether or not you're planning to do the second challenge task as well.

If you're planning to do the second challenge task, you should be able to get recursion almost "for free"&mdash;because your closures will have to store environment *references*, not environments, and you can freely mutate those environments.

If you're not doing the second challenge task, you can still accomplish this by noticing a little bit about how closures work.  Suppose that you have an expression like
```scheme
(let* [(f M)] N)
```
for arbitrary `M` and `N`, and suppose that `M` evaluates to some closure.  (It doesn't really matter how.)  Note that nothing *inside* the closure can depend on the stored environment yet, because it hasn't been executed.  So you can just update the captured environment to include a binding of `f` to the returned closure, and you'll be set.

## Mutation with Static Scope (4%)

The second challenge task is to implement mutation with static scope.  To see why this is challenging, consider the following example:
```scheme
(begin
   (define count
      (let* [(counter 0)]
         (lambda (n)
            (begin
               (set! counter (+ counter 1))
               (+ n counter)))))
    (list (count 1) (count 2) (count 3)))
```
As with the counter example in the regular tasks, this expression should evaluate to the list `(2 4 6)`.  However, the mechanism by which it does so is more complicated: there is no global `counter` variable.  Instead, there is a local `counter` variable which gets *saved in the closure of `count`*.

Keep in mind: we're not actually changing either the interpretation of scope from the project 2 challenge&mdash;we still save the variables where a function is defined as the function's closure&mdash;or the interpretation of `set!` from project 3&mdash;we still mutate the nearest binding of a variable.  The challenge arises *in their interaction*: the closest occurrence of a variable may well be in a closure.

So how can you realize this behavior in Haskell?

One thing you *can't* do is try to somehow track, with each function, where (or whether) that function is stored, so that you can update its closure that way---because multiple functions can capture the same local state.  Here's an example:
```scheme
(begin
   (define the-counter
      (let* [(counter 0)]
         (cons (lambda (n) (set! counter (+ counter n)))
               (lambda () counter))))
   (let* [(incr (fst the-counter))
          (get (snd the-counter))]
      (begin
         (incr 1)
         (incr 2)
         (get))))
```
Now `the-counter` is a pair of functions that *share* access to the `counter` variable.  (If you want to think about this as an *object*, and the two functions as its *methods*... well, you're basically right.)  In executing the `set!` in `incr`, you need to also update `get`'s view of `counter`.

There are two ways that occur to me for you to do this problem.

The first is to fall back on an underlying model of mutation provided by Haskell to implement your mutable variables.  Just the [`State` monad](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html) from [`mtl`](https://hackage.haskell.org/package/mtl) won't do it, because that gives you a *single* instance of state, and you want each variable to be *individually* mutable.  Instead, you could use the [`ST` monad](https://hackage.haskell.org/package/base-4.16.0.0/docs/Control-Monad-ST.html), and store individual variables in [`STRef`s](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-STRef.html).  The `ST` monad seems more complicated than it is: there's some trickery to guarantee that it's *safe*, but you don't actually *use* it any differently from any other monad.

I don't love that idea, though, because it doesn't actually model mutability in Haskell; instead, it relies on someone else's model of mutability in Haskell.

The second approach would be to model the mutation of closures (that is, of environment frames) directly.  To do this, you need to add a layer of indirection to your use of the environment.  On the one hand, you'll have an indexable collection of environment frames (you might call this the "heap"); you could represent them as:

* A list of frames (remember to count from the right-end of the list, so that your indexes don't change at the list grows);
* An association list `[(Int, Frame)]`; or even
* Use a map type from Haskell's libraries, like [`Data.Map`](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Map.html) or [`Data.IntMap`](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-IntMap.html).

Then, every place that you would have used an environment directly before, you'll use a reference to a frame instead.  Then, when `set!` updates a frame *in the heap*, every closure *with a reference to that frame* will automatically see the updated version.

This seems like a bigger change than it is.  You have already been thinking about "the environment" as a list of frames&mdash;now it still is, but they're connected by integers that you can see, instead of by pointers inside Haskell.  You've already thought about saving environments&mdash;you still are, but you're saving your own references to them instead of using Haskell's.  And so forth.