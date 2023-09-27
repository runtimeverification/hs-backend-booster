#!/usr/bin/env bash

set -exuo pipefail

# using variables from runDirectoryTest.sh

echo "client=$client"
echo "dir=$dir"
echo "arguments=$*"

if [ -x $(which jq) ] ; then
   JQ=$(which jq)
   echo "Using jq from PATH at $JQ"
else
   echo "Using Nix-build jq (hope it's there...) "
fi


diff="diff -s -"
# remove "--regenerate" and tweak $diff if it is present

client_args=""
for arg in $*; do
    case "$arg" in
        --regenerate)
            echo "Re-generating expectation files"
            diff="tee"
            ;;
        *)
            client_args+=" $arg"
            ;;
    esac
done

# execute with logging (to a stuck state), compare with time fields removed
echo "Running a request which gets stuck, with logTiming enabled"
${client} \
    execute $dir/state-c.execute ${client_args} -O log-timing=true | \
    $JQ 'del(.result.logs[].time)' | \
    ${diff} $dir/response-c.json
