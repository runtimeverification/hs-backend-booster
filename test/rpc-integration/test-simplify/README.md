# Tests for simplification in `booster` (executed with `booster-dev` only)

This integration test runs a few simplification requests with `Int`
expressions. It is expected that known rules from `domains.md` apply,
which will, e.g., move symbolic terms in additions and subtractions to
the left.

Since `booster-dev` is used without an llvm backend library, no
integer evaluation is expected in the results.

Some additional functions and predicates were defined to test the
behaviour for recursive evaluation (when an evaluation or
simplification has a requirement which must be simplified/evaluated
before it is found to be true). See `simplify.k` for details.


1) _no-simplification_

      _Input:_
   - `123`

   _Expected:_
   - `123` (no rule applies)

1) _plus-null-removed_

   _Input:_
   - `0 + f(34)`

   _Expected:_
   - `f(34)` (`domains.md` rule to remove zero addition)

1) _symbolic-first_

   _Input:_
   - `12 + f(34)`

   _Expected:_
   - `f(34) + 12` (symbolic value moved to the left)

1) _symbolic-first-of-3_

   _Input:_
   - `12 + f(34) + 56`

   _Expected:_
   - `f(34) + 12 + 56` (symbolic value moved to the left)

1) _with-logging_

   Same as _symbolic-first-of-3_ but with simplification logging enabled

   _Input:_
   - `12 + f(34) + 56`

   _Expected:_
   - `f(34) + 12 + 56` (symbolic value moved to the left)

1) _evaluate-under-function_

   _Input:_
   - `f(12 + 0)`

   _Expected:_
   - `f(12)` (removing the zero addition)

1) _evaluate-two-stage-fail_

   _Input:_
   - `g(2)`

   _Expected:_
   - `g(2)` (stuck since no evaluation rule applies)

1) _evaluate-two-stage_

   _Input:_
   - `g(1)`

   _Expected:_
   - `1` (applying rule `eval-g` after evaluating `p3(1)` to `true`)
