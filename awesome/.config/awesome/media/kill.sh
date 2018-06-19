#!/bin/sh

PIDS=`pgrep mpris_wrapper`

for PID in $PIDS
do
    pkill -P $PID
done

