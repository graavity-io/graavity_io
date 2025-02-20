#!/bin/bash

OS='centos-6.8'

rlwrap -A ./bin/$OS/graavityclient -c "conf/$(ls conf | grep -m 1 client)" +RTS -N2
