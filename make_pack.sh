#!/bin/sh

#Usage ./make_pack.sh 1.0.4
echo "name(tabling_dra)."> pack.pl
echo "version('$1')." >> pack.pl
tail -n +3 pack.pl.in >> pack.pl
cat pack.pl
zip -r tabling_dra-$1.zip pack.pl README.md prolog/dr*.pl t/*.pl

