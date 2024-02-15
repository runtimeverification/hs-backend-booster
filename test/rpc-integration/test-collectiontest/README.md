# Simple tests related to (internalised) Map and Set

The definition for these tests imports `MAP` and `SET`, and defines a custom
`Collection` sort as a supersort of `Set`, as well as a `State ( Collection, Int )`.

The only rule in the definition counts down from the `Int` in the `State`,
while adding the numbers it counts down from to the `Collection`.


1. Execute request, starting from `State ( .Set, 12 )`
   - Expected response: `State( {1..12}, 0}`

2. Simplify request for `{ 1 -> 111, 2 -> AVARIABLE }[1] orDefault 0`
   - Expected response: `111` (as a `KItem`)
