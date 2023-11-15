module WASM-MAPS
  imports MAP-VAL-TO-VAL

  configuration
        <wasm>
          <instrs> $PGM </instrs>
          <locals> .MapValToVal </locals>
        </wasm>

  syntax Instr ::= "init_local"  Int Int
  // -----------------------------------
  rule <instrs> init_local INDEX VALUE => . ... </instrs>
       <locals> LOCALS => LOCALS {{ wrap(INDEX) <- wrap(VALUE) }} </locals>

endmodule

module MAP-VAL-TO-VAL
  imports WRAPPED-INT

  syntax Val ::= WrappedInt
 // -----------------------

  syntax MapValToVal [hook(MAP.Map)]
  syntax MapValToVal ::= MapValToVal MapValToVal
         [ left, function, hook(MAP.concat), klabel(_MapValToVal_),
           symbol, assoc, comm, unit(.MapValToVal), element(_Val2Val|->_)
         ]
  syntax MapValToVal ::= ".MapValToVal"
         [function, total, hook(MAP.unit),klabel(.MapValToVal), symbol]
  syntax MapValToVal ::= Val "Val2Val|->" Val
         [function, total, hook(MAP.element), klabel(_Val2Val|->_), symbol]

  syntax MapValToVal ::= MapValToVal "[" key: Val "<-" value: Val "]" [function, total, klabel(MapVal2Val:update), symbol, hook(MAP.update), prefer]

  syntax priorities _Val2Val|->_ > _MapValToVal_ .MapValToVal
  syntax non-assoc _Val2Val|->_

  syntax MapValToVal ::= MapValToVal "{{" key: Val "<-" value: Val "}}"
                 [ function, total, klabel(MapValToVal:primitiveUpdate), symbol,
                   prefer
                 ]
  rule M:MapValToVal {{ Key:Val <- Value:Val }}
      => M[Key <- Value]

endmodule

module WRAPPED-INT
  imports INT

  syntax WrappedInt ::= wrap(Int)  [symbol, klabel(wrapInt)]
  // -------------------------------------------------------

  syntax Int ::= unwrap(WrappedInt)  [function, total, injective, symbol, klabel(unwrapInt)]
  // ---------------------------------------------------------------------------------------
  rule unwrap(wrap(A:Int)) => A
endmodule
