#!/usr/bin/env bash
set -euxo pipefail

export KEVM_VERSION=${KEVM_VERSION:-'master'}

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"

FEATURE_BRANCH_NAME=${FEATURE_BRANCH_NAME:-"$(git rev-parse --abbrev-ref HEAD)"}
FEATURE_BRANCH_NAME="${FEATURE_BRANCH_NAME//\//-}"

if [[ $FEATURE_BRANCH_NAME == "master" ]]; then
  FEATURE_BRANCH_NAME="feature"
fi

export PYTEST_PARALLEL=8 

BUG_REPORT_DIR=$($SCRIPT_DIR/performance-tests-kevm.sh --bug-report)

nix_shell() {
  GC_DONT_GC=1 nix develop $SCRIPT_DIR/..#cabal --extra-experimental-features 'nix-command flakes' --command bash -c "$1"
}

nix_shell "cabal build kore-rpc-booster"

export SERVER=$(nix_shell "cabal exec which kore-rpc-booster")

nix_shell "cabal build kore-rpc-client"

export CLIENT=$(nix_shell "cabal exec which kore-rpc-client")

export LLVM_LIB="$SCRIPT_DIR/bug-reports/kevm-$KEVM_VERSION-$FEATURE_BRANCH_NAME/interpreter.dylib"

export LOG_DIR="$BUG_REPORT_DIR"

for tar in $(find $SCRIPT_DIR/bug-reports/kevm-$KEVM_VERSION-$FEATURE_BRANCH_NAME -name \*tar ); 
do 
  echo "######## $tar ########"; 
  nix_shell "$SCRIPT_DIR/run-with-tarball.sh $tar -l Aborts --print-stats 2>&1 | tee $SCRIPT_DIR/bug-reports/kevm-$KEVM_VERSION-$FEATURE_BRANCH_NAME/$(basename $tar).out"; 
done

cd $SCRIPT_DIR/bug-reports/kevm-$KEVM_VERSION-$FEATURE_BRANCH_NAME

# Counting abort reasons
grep -e "Uncertain about" *log | sed -e 's,/tmp/tmp.[^/]*/evm-semantics/kevm-pyk/src/kevm_pyk/kproj/evm-semantics/,,' -e "s/.*Uncertain about \(.*\)$/\1/" -e "s/jumpi.true :.*/jumpi.true/" | nix_shell "runghc $SCRIPT_DIR/Count.hs" > abort_reasons.count

# Counting uncertain conditions by rule and actual outcome in kore-rpc
grep -E -e "^\[Aborts.*Uncertain about a condition| Booster aborted, kore yields"  *log | sed -n -e '/Uncertain about/,/yields/{/yields/{!p};p}' | sed  -e 's/\(jumpi\.true\) :.*$/\1/' | sed -n -e 'N;s/.*Uncertain about a condition in \(.*\)\n.*kore yields \(.*\)/ \1 -- \2 /g;p' | less | nix_shell "runghc $SCRIPT_DIR/Count.hs" > uncertain_conditions.count