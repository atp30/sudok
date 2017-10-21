# Sudoku Solver

## Overview

This is an excercise for personal instruction to build a sudoku solver in
Haskell. No guaranees can be made for efficiency or effectiveness.

The idea is to build a Monad Stack which sets up a series of rules and
constraints - each of which can maintain it's own state. Any level of the
stack can cause the game to fail (by evaluating to mzero/empty). They
communicate by filtering the total number of available options.

Currently winning is managed by a seperate function - but it would be nice
to manage this in the type system in future.

I also intend to use this to learn a bit more about testing frameworks and
documentation. Currently it is a bit of a mess.

## Instructions

I have been testing this on GHC8.2. An earlier version was failing to build due
to a bug in GHC8.02, so mileage on lesser versions may vary.

If you have the nix package manager, a `nix-build` or `nix-shell` command should
be adequate - the derivations read from the cabal file.

Currently, there are five test files. Run the solver on them by:

```
git clone https://github.com/atp30/sudok.git
cd sudok
cabal build
sudoku < tests/test1.txt
sudoku < tests/test2.txt
sudoku < tests/very_hard.txt
```

Currently, `test1.txt` and `test2.txt` are solved entirely deterministically.
`very_hard.txt` requires trial and error. I am uncertain about the others,
although it seems as though some trial and error may be used, based on times.

test1.txt and test2.txt are sourced from [sudopedia](http://sudopedia.enjoysudoku.com/Test_Cases.html)
and licensed under the GNU FDL.

the wiki* tests are sourced from wikipedia
