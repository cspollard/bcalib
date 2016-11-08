#!/bin/bash

cd ~/Programming/bcalib.git
TMP=`mktemp -d`
bcalibRun --outfile $TMP/tmp --infiles $1 > ${1/infiles/log} 
OUT=${1/infiles/hist.gz}`
mv $TMP/tmp $OUT
RTN=$!
rm -r $TMP
exit $RTN
