#!/bin/sh
# switch working dir to install location
cd $(dirname $0)

swipl -f ws.pl -g start
