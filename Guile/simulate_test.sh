#!/bin/sh

./simulate_test > simulate-test.out
cmp simulate-test.expected simulate-test.out
