#!/bin/sh

./arg_parameters_test > arg-parameters-test.out
cmp arg-parameters-test.expected arg-parameters-test.out
