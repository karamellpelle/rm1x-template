#!/bin/sh


ghc "$@" -i "source/Template.hs" --make source/Main.hs -O2 -o rm1x-template
