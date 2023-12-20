module QUESTIONMARK-VARS
  syntax State ::= "a"
                 | "b"
                 | ".State"

  syntax KItem ::= "#setSymbolicState" [klabel(setSymbolicState)]

  configuration <k> $PGM:KItem ~> .K </k>
                <state> .State       </state>

  rule [make-state-symbolic]:
       <k> #setSymbolicState => . ... </k>
       <state> _ => ?X                </state>

endmodule
