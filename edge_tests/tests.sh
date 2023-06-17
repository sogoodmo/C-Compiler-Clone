#!/bin/bash
set -e
#export LLVM_INSTALL_PATH=/home/gihan/LLVM/install-10.0.1
#export LLVM_INSTALL_PATH=/modules/csXXX/llvm-8/
#export LLVM_INSTALL_PATH=/modules/csXXX/llvm-10.0.1
#export LLVM_INSTALL_PATH=/modules/csXXX/llvm-12.0.1
#export LLVM_INSTALL_PATH=/tmp/LLVM/llvm-14.0.6
export LLVM_INSTALL_PATH=/modules/csXXX/llvm-15.0.0

export PATH=$LLVM_INSTALL_PATH/bin:$PATH
export LD_LIBRARY_PATH=$LLVM_INSTALL_PATH/lib:$LD_LIBRARY_PATH
CLANG=$LLVM_INSTALL_PATH/bin/clang++

module load GCC/9.2.0

DIR="$(pwd)"

### Build mccomp compiler
echo "Cleanup *****"
rm -rf ./mccomp

echo "Compile *****"

make clean
make mccomp

COMP=$DIR/mccomp
echo $COMP

function validate {
  $1 > perf_out
  echo
  echo $1
  grep "Result" perf_out;grep "PASSED" perf_out
  rc=$?; if [[ $rc != 0 ]]; then echo "TEST FAILED *****";exit $rc; fi;rm perf_out
}

echo "Test *****"

cd edge_tests/test/
pwd
rm -rf output.ll add
"$COMP" ./test.c
$CLANG driver.cpp output.ll  -o add
validate "./add"