#! /usr/bin/env bash

# Starts an rpc server and runs a tarball against it using rpc-client
# Environment variables:
#   DEFINITION: kore file to load (optional, otherwise from tarball)
#   LLVM_LIB: default llvm library to load (required)
#   BOOSTER: booster home directory (only relevant if not setting the following)
#   SERVER: kore-rpc server path (default: from build dir in BOOSTER)
#   CLIENT: rpc client path (default: from build dir in BOOSTER)
#   LOG_DIR: path to place logs, defaults to current directory

tarball=${1?"Tarball argument missing"}
shift

tarname=$(basename $tarball)

booster=${BOOSTER:-$(realpath $(dirname $0)/..)}
server=${SERVER:-$booster/.build/booster/bin/kore-rpc-booster}
client=${CLIENT:-$booster/.build/booster/bin/kore-rpc-client}
log_dir=${LOG_DIR:-.}

if [ -z "$DEFINITION" ]; then
    # unpack definition file from tarball, into tmp dir
    mkdir -p tmp
    tar xf $tarball -O definition.kore > tmp/definition.kore
    kore=tmp/definition.kore
else
    kore=$DEFINITION
fi

if [ ! -s $kore ]; then
    echo "Definition file $kore missing/empty"
    exit 1
fi


# find out module name from definition file
MODULE=$(grep -o -e "^module [A-Z0-9-]*" $kore | tail -1 | sed -e "s/module //")

# hard-wire llvm backend, use surplus arguments from the command line as server options
if [ -f ./${tarname}-interpreter.so ]; then
    lib=./${tarname}-interpreter.so
else
    lib=$(realpath ${LLVM_LIB})
fi
if [ ! -f "$lib" ]; then
    echo "A path to an LLVM backend library must be provided in LLVM_LIB"
    exit 2
fi
server_params="--llvm-backend-library $lib --server-port 0 $@"

echo "Starting server: $server $kore --module ${MODULE:-UNKNOWN} $server_params"
$server $kore --module ${MODULE?"Unable to find main module"} $server_params > ${log_dir}/${tarname}.log 2>&1 &
server_pid=$!

trap 'kill -2 ${server_pid}' ERR EXIT
echo "Server PID ${server_pid}"

i=15
while ! lsof -a -p${server_pid} -sTCP:LISTEN -iTCP && [[ $i -ge 0 ]]; do
    echo "Waiting for server ($i attempts left)"
    i=$((i-1))
    sleep 1
done

# find server port via lsof
server_port=$(lsof -a -p${server_pid} -sTCP:LISTEN -iTCP | grep ${server_pid} | sed -e 's/.* TCP \*:\([0-9]*\).*$/\1/')
echo "Server listening on port ${server_port}"


echo "Running requests from $tarball against the server: $client run-tarball $tarball --keep-going -p ${server_port} -h 127.0.0.1"
$client run-tarball $tarball --keep-going -p ${server_port} -h 127.0.0.1

echo "Done with $tarball"