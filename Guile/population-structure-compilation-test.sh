#!/bin/sh

./coasim_guile population-structure-compilation-test.scm > population-structure-compilation-test.out
cmp population-structure-compilation-test.expected population-structure-compilation-test.out
