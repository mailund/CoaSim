#!/bin/sh

./coasim_guile migration-test.scm > migration-test.out
cmp migration-test.expected migration-test.out
