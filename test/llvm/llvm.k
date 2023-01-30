module LLVM
  imports INT
  imports BYTES
  imports BOOL
  imports MAP
  imports SET
  imports K-EQUAL

  syntax Num ::= Even | Odd
  syntax Even ::= Zero() | Two() | Four() | Six() | Eight() | Ten()
  syntax Odd ::= One() | Three() | Five() | Seven() | Nine()

  syntax Int ::= eval(Num) [function, total]
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

  rule eval(Zero()) => 0
  rule eval(SomeNum) => 1 +Int eval(pred(SomeNum))
    requires notBool SomeNum ==K Zero()

endmodule
