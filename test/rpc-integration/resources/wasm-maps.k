module WASM-MAPS
  imports INT

  syntax WrappedInt ::= wrap(Int)  [symbol, klabel(wrapInt)]
  syntax Int ::= unwrap(WrappedInt)  [function, total, injective, symbol, klabel(unwrapInt)]
  rule unwrap(wrap(A:Int)) => A


  configuration
        <wasm>
          <instrs> .K </instrs>
          <locals> .MapIntToVal </locals>
        </wasm>

  syntax Instr ::=  "init_local"  Int Val
 // -------------------------------------
  rule <instrs> init_local INDEX VALUE => . ... </instrs>
       <locals> .MapIntToVal => wrap(INDEX) Int2Val|-> VALUE  </locals>

  //---------------------------------------------------------------------------
  syntax IValType ::= "i32" [klabel(i32), symbol] | "i64" [klabel(i64), symbol]
  syntax ValType  ::= IValType

  syntax IVal ::= "<" IValType ">" Int    [klabel(<_>_)]
  syntax Val ::= IVal
  syntax Val ::= "undefined"
 // ---------------------------

  syntax MapIntToVal [hook(MAP.Map)]
  syntax MapIntToVal ::= MapIntToVal MapIntToVal
         [ left, function, hook(MAP.concat), klabel(_MapIntToVal_),
           symbol, assoc, comm, unit(.MapIntToVal), element(_Int2Val|->_),
           index(0), format(%1%n%2)
         ]
  syntax MapIntToVal ::= ".MapIntToVal"
         [function, total, hook(MAP.unit),klabel(.MapIntToVal), symbol]
  syntax MapIntToVal ::= WrappedInt "Int2Val|->" Val
         [function, total, hook(MAP.element), klabel(_Int2Val|->_), symbol]

  syntax priorities _Int2Val|->_ > _MapIntToVal_ .MapIntToVal
  syntax non-assoc _Int2Val|->_

endmodule
