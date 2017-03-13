# Ur/Web Misc

Various code written at various times in Ur/Web. Not all of it is
useful in its current state.

## `relation.urs`

This is a toy relational algebra query evaluator, embedded in
Ur/Web. Please find the test case in `tests/rel.ur`.

Features:

* first-class relation variables (and they are typed precisely,
  meaning the underlying row type is tracked statically)
* relvar operators that allow manipulation of relvars:
  * `empty` (for an empty relvar)
  *`insert` (to insert a row into a relvar)  
* query operators (these will never modify their inputs):
  * `project` (to project out a column from a relvar)
  * `join` (to perform a natural equi-join of two relations)
  * `restrict` (to apply a predicate to a relvar)
  * `union` (set union of two relvars)
  * `difference` (set difference of two relvars)
  * `times` (cross-join of two relvars)

To run: please build it via `urweb rel`, run `./rel.exe` and navigate
to `http://localhost:8080/main`.

## Picklers

This is serialization/deserialization in one go based on Andrew
Kennedy's paper [Pickler Combinators](https://www.microsoft.com/en-us/research/publication/functional-pearl-pickler-combinators/).

I used something similar in an F# prototype at work, but it was way
too tedious to handle n-ary tuples. Metaprogramming based on row types
to the rescue!

## Drag-drop

This is just a demo to show that drag-n-drop is possible to write in
pure Ur/Web. To run, please build it via `urweb dragdrop`, run
`./dragdrop.exe`, and navigate to `http://localhost:8080/main`.

I also left some quirky notes for a hypothetical future extension to
Ur/Web to handle HTML5 drag and drop API.

## Pagination, Qstring, TreeFun

These are taken verbatim from another project. The modules are
unlikely to build, at this point.
