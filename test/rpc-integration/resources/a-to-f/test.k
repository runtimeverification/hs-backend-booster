module TEST
  imports BOOL
  imports INT

  syntax State ::= "a" [token]
                 | "b" [token]
                 | "c" [token]
                 | "d" [token]
                 | "e" [token]
                 | "f" [token]
                 | "bot" [token]

  // we need a k cell with a kseq in it to have proper indexing
  configuration <T>
                  <k> $PGM:State ~> .K </k>
                  <val> 0 </val>
                </T>


  rule [AB]: <k> a => b ... </k> <val> V </val> requires V ==Int 0
  rule [AC]: <k> a => c ... </k> <val> V </val> requires notBool (V ==Int 0)
  rule [CD]: <k> c => d ... </k>
  rule [DE]: <k> d => e ... </k>
  rule [EF]: <k> e => f ... </k>
  rule [BotF]: <k> bot => a ... </k> ensures false

// to produce input state:
// krun --output kore --depth 1 -cPGM=c | kore-parser definition.kore --module TEST --pattern /dev/stdin --print-pattern-json > state.json

endmodule
