# Overview

This document contains a general overview of project 3.  Specific details are contained in the other Markdown files in this directory.

You should read all the documentation carefully before beginning to work on the project.

# Project 3 Tasks

Project 3 concludes the implementation of the "MiniScheme" language, describe in [MiniScheme.md](MiniScheme.md).  The language we built in project 2 was purely functional: while we could give names to local definitions and to function parameters, we could not *change* the meaning of a name once it was introduced.  The primary contribution of project 3 is a special form to mutate an existing variable definition.  You will also add special forms to make defining new functions and variables more convenient, and to sequence MiniScheme statements.

Each of these tasks is described in more detail in [Tasks.md](Tasks.md).

# Directory Organization

The project directory contains several subdirectories.

* `docs`: Contains the descriptions of the project and the MiniScheme language.
* `prov`: Contains Haskell code provided as part of the project.  **You should not change any code within the `prov` directory.**
* **[P3]** `prov/Soln`: Contains sample solution code for projects 1 and 2.  You may either refer to this code in writing your own solution, or import code from `Soln` directly in your solution.  In the latter case, you **must** update the `Project.cabal` file to list the modules from `Soln` that you are using.
* `src`: Contains source code that you write as part of the project.

Finally, there are the usual dot files in the root of the project folder.

You can put all your project code into `Main.hs`.  However, you may find it more useful to divide your project code among several modules&mdash;for example, an `AST` module for your syntax tree, a `Printer` module for your pretty printer, and so forth.

If you add additional modules to your project, you **must** update the `Project.cabal` file in the root of the project accordingly.  You should add each new module you create to the `other-modules` list in **both** the `minis` and `Test` targets.  Please see [the Cabal documentation](https://cabal.readthedocs.io/en/3.6/intro.html) for more details.

# Executables

`Project.cabal` defines two executable targets.  `minis` is an evaluator for MiniScheme programs: it takes a list of files, and interprets the contents of those files as MiniScheme expressions using your evaluation and printing functions.  For example, you could invoke:
```shell
$ cabal run minis examples/add.scm
0
3
6
6
10
$
```

**[P2,P3]** The `minis` executable now also functions as a simple REPL (read-evaluate-print loop), if given no file arguments.  For example:
```shell
$ cabal run minis
Up to date.
] (+ 1 2 3)
6
] ((lambda args (apply + args)) 1 2 3)
6
]
```
You can exit the REPL by entering a blank line at the `]` prompt.

These behaviors are defined in the `main` function in `src/Main.hs`.  We do *not* use this main function in testing, so you are free to add additional output as you find useful.  `Test` is the test runner target, as in the problem sets.

# Testing

Our tests for the project are essentially end-to-end tests: each test generates a source MiniScheme expression, sends it to your evaluator *and* printer functions (via the `run` function you write), and then compares your result against the expected result.

Note that you do *not* have to match the exact formatting of the expected result (for example: you don't have to match spaces, line breaks, or so forth).  You do, however, have to produce valid syntax; for example, if the expected result is the list "(1 2 3)", and your output omits the parentheses "1 2 3", the test will not be considered passing.

This means that you have to have *some* evaluation and *some* printing code to pass any of the tests.  It does not mean you have to have *all* of the evaluation and printing code to pass any of the tests.  I strongly encourage you to work iteratively: implement the minimal functionality needed to pass tests, rather than trying to implement the entire evaluator before testing.

Your evaluation function will be expected to return a `Nothing` value if evaluation fails (for example, if you try to subtract an empty list of numbers, or take the length of an integer).  We have included test cases where evaluation should fail; those test cases will not be considered passing if your evaluator crashes (for example, raises an incomplete pattern match exception) rather than returning `Nothing`.

**[P2, P3]** Our testing strategy has evolved to address overly-specific solutions submitted for the first project.  There are now two collections of tests included with the project.

* *Static* tests consist of *fixed* input strings and expected outputs.  All of the tests distributed with project 1 were static tests.
* *Randomized* tests have a common pattern, but components of that pattern will be different each time the test is run.  For a very simple example, a randomized test might always consist of a program `((lambda (V) V) M)`, but variable `V` and expression `M` might vary in each execution.   The actual randomized tests will be more variable than that: developing the same example, we might have a random number of variables, a random number of arguments, and a result chosen at random from among the parameters.

Each run of the randomized tests will include a number of runs of each individual test&mdash;for example, we might run the randomized `lambda` test 10 times, that is, with 10 different lists of parameters and arguments.

The static tests are present to guide your implementation, but are *not* used in assessing your project.  Your project will be assessed based on the randomized tests *alone*.  (Of course, you can still receive less credit than your randomized test score, in cases of academic misconduct or in case you have not followed the project directions.)  To assure fairness, we will grade all the projects with the same random seed (so, while the test cases will still be generated, each submission will be graded with the same test cases).

# Challenge

The challenge problem for project 3 consists of implementing mutation for lexical scope, and is described in [Challenge.md](Challenge.md).

**[P2, P3]** Challenge problems are *cumulative* through all three projects.  That means:

1. You can assume that future challenge problems may rely on past challenge problems.
2. You may complete the challenge for each project during *any subsequent project*, for full credit.
