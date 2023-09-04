module TEST
  import INT

  syntax State ::= "init" [token]
                 | "a"    [token]

  configuration <k> $PGM:State ~> .K </k>
                <int> 0:Int </int>
                <jnt> 0:Int </jnt>

  rule [init]: <k> init => a ... </k>
               <int> X        </int>
               <jnt> Y        </jnt>
               ensures X ==Int Y

endmodule
