#!/bin/sh

./epochs_test > epochs-test.out
cmp epochs-test.expected epochs-test.out
