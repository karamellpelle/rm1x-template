#!/bin/sh


# the -i option was not working, see http://stackoverflow.com/a/5174368/753850
sed -e "s/RM1X/$(git rev-parse HEAD)/g" source/Git.hs > __G__ && mv __G__ source/Git.hs
sed -e "s/rm1x/$(git rev-parse --short HEAD)/g" source/Git.hs > __G__ && mv __G__ source/Git.hs


