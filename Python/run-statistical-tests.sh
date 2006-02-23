#!/bin/sh

underscore="[4m"
bold="[1m"
green="[32m";
red="[31m";
reset="[0m"

tests=`ls stat-tests/*.py`

echo
echo "Running a set of statistical tests -- these are expected to fail"
echo "occasionally, as statistically unlikely events ${bold}${underscore}do${reset} happen,"
echo "but consistent failures should be reportet as a bug."
echo

for t in $tests; do
  echo "Running $bold$underscore$t$reset"
  if ./$t; then
    echo "$bold${green}Success$reset"
  else
    echo "$bold${red}Failed$reset"
  fi
done
