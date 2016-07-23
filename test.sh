#!/bin/sh
# switch working dir to install location
cd $(dirname $0)

swipl -f test.pl -t run_tests
