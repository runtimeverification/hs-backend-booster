module MONOMORPHIC-MAP
  imports INT

  syntax WrappedInt ::= wrap(Int)  [symbol, klabel(wrapInt)]
  syntax Int ::= unwrap(WrappedInt)  [function, total, injective, symbol, klabel(unwrapInt)]
  rule unwrap(wrap(A:Int)) => A

  syntax MapIntToInt [hook(MAP.Map)]
  syntax MapIntToInt ::= MapIntToInt MapIntToInt
         [ left, function, hook(MAP.concat), klabel(_MapIntToInt_),
           symbol, assoc, comm, unit(.MapIntToInt), element(_Int2Int|->_),
           index(0), format(%1%n%2)
         ]
  syntax MapIntToInt ::= ".MapIntToInt"
         [function, total, hook(MAP.unit),klabel(.MapIntToInt), symbol]
  syntax MapIntToInt ::= WrappedInt "Int2Int|->" WrappedInt
         [function, total, hook(MAP.element), klabel(_Int2Int|->_), symbol]

  syntax priorities _Int2Int|->_ > _MapIntToInt_ .MapIntToInt
  syntax non-assoc _Int2Int|->_

endmodule
