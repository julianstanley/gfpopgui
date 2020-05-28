#!/bin/bash -eu
find ../R/ | entr sh -c 'lsof -i:11616 | awk '"'"'NR!=1 {print $2}'"'"' | xargs kill ; (cd .. && pwd && Rscript ./dev/run_dev.R &)'
