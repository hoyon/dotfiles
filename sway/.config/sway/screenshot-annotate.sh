#!/bin/bash

file=$(mktemp --suffix .png)
grimshot save area "$file"
drawing "$file"
