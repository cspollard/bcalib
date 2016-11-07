#!/bin/bash

cd ~/Programming/bcalib.git
TMP=`mktemp -d`
bcalib --outfile $TMP/tmp --infiles $1 > ${1/infiles/log} 
mv $TMP/tmp ${1/infiles/hist.gz} 
rm -r $TMP
