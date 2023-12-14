module COLLECTIONTEST
  imports INT
  imports BOOL
  imports SET
  imports LIST
  imports MAP

  syntax State ::= State(Collection, Int) [klabel(State), symbol]

  syntax Collection ::= Set | Map // | List

  syntax Bool ::= f ( Int ) [function, total, no-evaluators, klabel(f), symbol]
  // to create an opaque key/element (unequal to any Int but that is
  // not recognised by the execution)

  configuration <k> $PGM:State ~> .K </k>

  rule [countdown-set]:
    <k> State(CURRENT:Set, N) => State(CURRENT SetItem(N) , N -Int 1) </k>
      requires N >Int 0 andBool notBool (N in CURRENT)
      [preserves-definedness]

  rule [build-map]:
    <k> State(CURRENT:Map, N) => State(CURRENT N |-> N , N -Int 1) </k>
      requires N >=Int 0 andBool notBool N in_keys(CURRENT)
      [preserves-definedness]

  rule [zero-map]:
    <k> State(CURRENT:Map, N) => State(CURRENT[ 0 -Int N <- 0 ], N -Int 1) </k>
      requires 0 -Int N in_keys(CURRENT)

endmodule
