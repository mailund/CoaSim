#!/bin/sh

underscore="[4m"
bold="[1m"
green="[32m";
red="[31m";
reset="[0m"

tests='markersTest.py randomMarkersTest.py simulateTest.py intervalsTest.py treesTest.py nodesTest.py customMarkerTest.py callbackTest.py iteratorTest.py migrationTest.py'
modules=`ls modules/CoaSim/*.py`

for t in $tests; do
  echo "Running $bold$underscore$t$reset"
  if ./$t; then
    echo "$bold${green}Success$reset"
  else
    echo "$bold${red}Failed$reset"
  fi
done
  
for m in $modules; do
  echo "Running $bold$underscore$m$reset"
  if python $m; then
    echo "$bold${green}Success$reset"
  else
    echo "$bold${red}Failed$reset"
  fi
done
  
