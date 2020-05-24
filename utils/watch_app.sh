#!/bin/bash -eu
ls -d ../R/* | entr sh -c 'lsof -i:11616 | awk '"'"'NR!=1 {print $2}'"'"' | xargs kill ; Rscript ../dev/run_dev.R &'
