#!/bin/bash

cd ~/Programming/bcalib.git
TMP=`mktemp -d`
bcalibRun --outfile $TMP/tmp --infiles $1 > ${1/infiles/log} 
OUT=${1/infiles/hist.gz}`
RTN=`mv $TMP/tmp $OUT`
rm -r $TMP
exit $RTN
