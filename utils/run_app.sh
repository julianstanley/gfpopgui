#!/usr/bin/env bash
trap watch_app.sh kill $( lsof -i:11616 -t)
bash watch_app.sh
