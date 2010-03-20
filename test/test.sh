#!/bin/bash

cd "$(dirname "$0")"
blackbox-test -i in -s .case
