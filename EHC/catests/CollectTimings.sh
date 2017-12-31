#!/bin/bash
TESTS=("
  BinTreeInternals
  BinTreeNoInternals
  BinTreeNoInternalsViaList
  BinTreeNoInternalsViaList2
  BinTreeInlinedMap
  BinTreeInlinedMap2
  BinTreeHigherOrderMap
  Fact
  Fibt
  Fibt2
  BinTreeInlinedMapNoValues
  ListInlinedMap
")
  # Map
  # Fold

for t in $TESTS
do
  echo "$t*"
  rm ./MainTime.* 2> /dev/null
  cp testshsfiles/$t.hs MainTime.hs
  echo "Optimized"
  rm ./MainTime 2> /dev/null
  uhcCRun.sh MainTime > /dev/null 2>&1
  for i in {0..9}
  do  
    time ./MainTime > /dev/null
  done
  echo "Master"
  uhcMRun.sh MainTime > /dev/null 2>&1
  for i in {0..9}
  do  
    time ./MainTime > /dev/null
  done
done