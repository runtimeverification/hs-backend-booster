module MICRO-KEVM-SYNTAX
  imports INT
  imports SET
// Micro KEVM is a synthetic K definition that uses several K features important in KEVM:
// * Map-cells
// * Sets
// * Lists of Bytes
    syntax WordStack ::= ".WordStack"      [smtlib(_dotWS)]
                       | Int ":" WordStack [klabel(_:_WS), smtlib(_WS_)]
 // --------------------------------------------------------------------

  syntax EthereumSimulation ::= ".EthereumSimulation"

  configuration
    <k> $PGM:EthereumSimulation </k>
    <micro-kevm>
      <wordStack> .WordStack </wordStack>
      <touchedAccounts> .Set </touchedAccounts>
      <accounts>
        <account multiplicity="*" type="Map">
          <acctID>      0                  </acctID>
          <balance>     0                  </balance>
        </account>
      </accounts>
    </micro-kevm>
endmodule

module MICRO-KEVM
  imports MICRO-KEVM-SYNTAX
endmodule