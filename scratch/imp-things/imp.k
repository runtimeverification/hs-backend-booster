module IMP-SYNTAX
    imports ID-SYNTAX
    imports INT-SYNTAX
    imports BOOL-SYNTAX

    syntax Exp ::= Int | Bool
               | "(" Exp ")" [bracket]

    // Integer arithmetic syntax
               > non-assoc:
                  "-" Exp     [neg, color(violet),seqstrict]
               > left:
                  Exp "*" Exp [mul, color(violet),seqstrict]
               |  Exp "/" Exp [mul, color(violet),seqstrict]
               > left:
                  Exp "+" Exp [add, color(violet),seqstrict]
               |  Exp "-" Exp [add, color(violet),seqstrict]
    // Integer comparison syntax
               > non-assoc:
                  Exp "<"  Exp [comp,color(teal),seqstrict]
               |  Exp "<=" Exp [comp,color(teal),seqstrict]
               |  Exp ">"  Exp [comp,color(teal),seqstrict]
               |  Exp ">=" Exp [comp,color(teal),seqstrict]
    // comparison for both Bool and Int
               |  Exp "==" Exp [comp,color(teal),seqstrict]
               |  Exp "!=" Exp [comp,color(teal),seqstrict]
    // Bool operators
               > left:
                  "!" Exp [color(blue),seqstrict]
               > left:
                  Exp "&&" Exp [color(blue),seqstrict]
                | Exp "^"  Exp [color(blue),seqstrict]
                | Exp "||" Exp [color(blue),seqstrict]
                | Exp "->" Exp [color(blue),seqstrict]

    // Identifiers can be expressions
    syntax Exp ::= Id

    // statements and IMP control flow
    syntax Stmts ::= List{Stmt, ";"}

    syntax Stmt ::= "return" Exp                    [strict]
                | Id "=" Exp                        [strict(2)]
                | "if" "(" Exp ")" Stmt "else" Stmt [strict(1)]
                | "while" "(" Exp ")" Stmt
                | "{" Stmts "}"

endmodule

module IMP
    imports IMP-SYNTAX
    imports INT
    imports BOOL
    imports ID
    imports MAP
    imports LIST
    imports K-EQUAL

    configuration
      <T>
        <k> $PGM:Stmts </k>   // execute statements in order
        <store> .Map </store> // stores bound variables
      </T>

    // technicality to make the `seqstrict` above work
    syntax Bool ::= isKResult(K) [function, symbol]
    rule isKResult(_:Int)  => true
    rule isKResult(_:Bool) => true
    rule isKResult(_)      => false [owise]

    // semantics of Int and Bool expressions
    rule <k> - I1:Int => 0 -Int I1 ... </k>
    rule <k> I1:Int + I2:Int => I1 +Int I2 ...</k>
    rule <k> I1:Int - I2:Int => I1 -Int I2 ...</k>
    rule <k> I1:Int * I2:Int => I1 *Int I2 ...</k>
    rule <k> I1:Int / I2:Int => I1 /Int I2 ...</k> requires I2 =/=Int 0

    rule <k> ! B1:Bool          => notBool B1 ...</k>
    rule <k> B1:Bool && B2:Bool => B1 andBool B2 ...</k>
    rule <k> B1:Bool ^ B2:Bool  => B1 xorBool B2 ...</k>
    rule <k> B1:Bool || B2:Bool => B1 orBool B2 ...</k>
    rule <k> B1:Bool -> B2:Bool => B1 impliesBool B2 ...</k>

    // comparison operators for `Int` (returning `Bool`)

    rule <k> I1:Int <  I2:Int => I1 <Int I2 ...</k>
    rule <k> I1:Int <= I2:Int => I1 <=Int I2 ...</k>
    rule <k> I1:Int >  I2:Int => I1 >Int I2 ...</k>
    rule <k> I1:Int >= I2:Int => I1 >=Int I2 ...</k>
    rule <k> I1:Int == I2:Int => I1 ==Int I2 ...</k>
    rule <k> I1:Int != I2:Int => I1 =/=Int I2 ...</k>

    // with `==` and `!=`, we _should_ be able to compare `Bool`.
    rule <k> B1:Bool == B2:Bool => B1 ==Bool B2 ...</k>
    rule <k> B1:Bool != B2:Bool => B1 =/=Bool B2 ...</k>

    // rule <k> X:Id => V ... </k>
    //      <store> ... X |-> V ... </store>
    // avoid AC matching on the map by defining a total lookup function
    syntax KItem ::= #lookup(Id, Map) [function, total]
    rule <k> X:Id => #lookup(X, M) ... </k>
         <store> M </store>

    rule #lookup(X, M) => M[X] requires X in_keys(M)
    rule #lookup(_, _) => false [owise]

    // statement block execution
    rule <k> { SS:Stmts }    => SS ~> . ... </k>
    rule <k> S:Stmt ; SS:Stmts => S ~> SS ...</k>
    rule <k> .Stmts ~> SS => SS </k>
    rule [stop]: <k> .Stmts => . </k> [priority(40)]

    // assignment
    rule <k> X = V => . ...  </k>
         <store> M => M [ X <- V ] </store>
         requires isKResult(V)

    // using a built-in #if statement
    rule <k> if ( B ) S1 else S2  => #if B #then S1 #else S2 #fi ...  </k>

    // unrolling `while` using if. Important that `while` is not strict!
    rule <k> while ( B ) S => if (B) { S ; while ( B ) S } else { .Stmts } ... </k>

endmodule
