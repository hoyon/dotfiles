#!/bin/bash

nc -lk $1 | python3 -u /home/hoyon/.config/awesome/media/mpris.py
