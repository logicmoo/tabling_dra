#! /bin/sh

# $1 is expected to have xsb executable + command line options
EMU=$1

$EMU << EOF
[make_graphs].

make_small_graphs.
EOF
