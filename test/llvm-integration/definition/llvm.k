module LLVM
  imports INT
  imports BYTES
  imports BOOL
  imports MAP
  imports SET
  imports K-EQUAL
  imports MAP-INT2INT

  syntax Num ::= Even | Odd
  syntax Even ::= Zero() | Two() | Four() | Six() | Eight() | Ten()
  syntax Odd ::= One() | Three() | Five() | Seven() | Nine()

  syntax Num ::= pred(Num) [function]

  rule pred(One()) => Zero()
  rule pred(Two()) => One()
  rule pred(Three()) => Two()
  rule pred(Four()) => Three()
  rule pred(Five()) => Four()
  rule pred(Six()) => Five()
  rule pred(Seven()) => Six()
  rule pred(Eight()) => Seven()
  rule pred(Nine()) => Eight()
  rule pred(Ten()) => Nine()

  syntax Int ::= eval(Num) [function, total]

  rule eval(Zero()) => 0
  rule eval(SomeNum) => 1 +Int eval(pred(SomeNum))
    requires notBool SomeNum ==K Zero()

  syntax Num ::= div2(Even) [function, total]

  rule div2(Zero()) => Zero()
  rule div2(Two()) => One()
  rule div2(Four()) => Two()
  rule div2(Six()) => Three()
  rule div2(Eight()) => Four()
  rule div2(Ten()) => Five()

endmodule

module MAP-INT2INT
  imports INT

  syntax WrappedInt

  syntax WrappedInt ::= wrap(Int)  [symbol, klabel(wrapInt)]
  syntax Int ::= unwrap(WrappedInt)  [function, total, injective, symbol, klabel(unwrapInt)]
  rule unwrap(wrap(A:Int)) => A

  syntax MapInt2Int [hook(MAP.Map)]
  syntax MapInt2Int ::= MapInt2Int MapInt2Int
         [ left, function, hook(MAP.concat), klabel(_MapInt2Int_),
           symbol, assoc, comm, unit(.MapInt2Int), element(_Int2Int|->_),
           index(0), format(%1%n%2)
         ]
  syntax MapInt2Int ::= ".MapInt2Int"
         [ function, total, hook(MAP.unit),
           klabel(.MapInt2Int), symbol, latex(\dotCt{MapInt2Int})
         ]
  syntax MapInt2Int ::= WrappedInt "Int2Int|->" WrappedInt
         [ function, total, hook(MAP.element),
           klabel(_Int2Int|->_), symbol,
           latex({#1}\mapsto{#2}), injective
         ]

endmodule
