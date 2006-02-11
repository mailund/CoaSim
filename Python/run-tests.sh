#!/bin/sh

underscore="[4m"
bold="[1m"
green="[32m";
red="[31m";
reset="[0m"

tests='markersTest.py'

for t in $tests; do
  echo "Running $bold$underscore$t$reset"
  if ./$t; then
    echo "$bold${green}Success$reset"
  else
    echo "$bold${red}Failed$reset"
  fi
done
  
