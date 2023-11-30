module TEST
  import BOOL
  import K-EQUAL

  syntax State ::= "init" [token]
                 | "a"    [token]
                 | "b"    [token]
                 | "c"    [token]
                 | "d"    [token]
                 // | "e"    [token]
                 // | "f"    [token]

  configuration <k> $PGM:State ~> .K </k>
//              <state> init:State   </state>

  rule [init]: <k> init => a ... </k>
//             <state> _ =>      </state>

  rule [AB]: <k> a => b ... </k>
    requires X ==K a andBool X ==K b
    // requires no-confusion in the booster SMT solver

  rule [AC]: <k> a => c ... </k>

  // BTW The :State is necessary. kore-exec crashes with this rule
  // [CD]: <k> (X => d) ~> _Rest </k>
  rule [CD]: <k> (X:State => d) ~> _Rest </k>
    requires notBool X ==K a
     andBool notBool X ==K b
     andBool notBool X ==K d
     // X == c considering no-junk and no-confusion, unclear without

// ------ recursive data types ----------------

  syntax Tree ::= Leaf ( KItem )
              | Inner ( TreeList )
  syntax TreeList ::= Nil()
                   | Cons ( Tree , TreeList )


  syntax NameCollision ::= Nil() | Leaf() | Inner ( KItem )

endmodule
