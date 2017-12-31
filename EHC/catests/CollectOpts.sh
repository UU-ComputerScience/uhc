#!/bin/bash
TESTS=("
  BinTree
  BinTreeInternals
  BinTreeNoInternals
  BinTreeNoInternalsViaList
  BinTreeNoInternalsViaList2
  BinTreeInlinedMap
  BinTreeInlinedMapNoValues
  BinTreeHigherOrderMap
  ListInlinedMap
  Fact
  Fibt
  Fibt2
  Lists
  Map
  Fold
  Strict
")

for t in $TESTS
do
  echo "$t:"
  rm ./MainOpts 2> /dev/null
  rm ./MainOpts.* 2> /dev/null
  cp testshsfiles/$t.hs MainOpts.hs
  uhcCRun.sh MainOpts > /dev/null
  echo ""
done