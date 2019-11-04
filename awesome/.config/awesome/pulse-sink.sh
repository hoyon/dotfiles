#!/bin/sh

pulsemixer --list-sinks | perl -lne '/ID:\ (.*), Name: Audioengine/ && print $1'
