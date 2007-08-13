#!/bin/bash
# Run with as parameter either 'Sum', 'Rec', 'One', or 'Const'

# Compile Haskell to LLVM, compile the llvm file, compile the runtime, link them both, invoke the file
../bin/8/ehc -cllvm ${1}.hs && llvm-as -f ${1}.ll && llvm-as -f ../src/rts/runtime.ll && llvm-ld ../src/rts/runtime.bc ${1}.bc && lli -force-interpreter a.out.bc 
