#!/bin/sh

./marker_test > marker-test.out
cmp marker-test.expected marker-test.out
