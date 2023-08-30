module TEST
  import INT
  import BOOL

  syntax State ::= "a" [token]
                 | "b" [token]
                 | "c" [token]
                 | "d" [token]

  configuration <k> $PGM:State ~> .K </k>
                <int> 0:Int          </int>

  rule [AB]: <k> a => b ... </k>
  rule [AC]: <k> a => c ... </k>
             <int> X        </int>
             requires X ==Int 42 andBool X =/=Int 42
  rule [BD]: <k> b => d ... </k>
  rule [CD]: <k> c => d ... </k>

endmodule
