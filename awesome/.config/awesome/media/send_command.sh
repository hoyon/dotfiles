#!/bin/sh

echo $2 | nc -q 0 localhost $1
