#!/bin/bash

source /cvmfs/atlas.cern.ch/repo/ATLASLocalRootBase/user/atlasLocalSetup.sh
echo "lsetup ROOT"
lsetup ROOT

echo "cd ~/Programming/bcalib.git"
cd ~/Programming/bcalib.git

echo "TMP=`mktemp -d`"
TMP=`mktemp -d`

echo "bcalibRun --outfile $TMP/tmp --infiles $1 > ${1/infiles/log}"
bcalibRun --outfile $TMP/tmp --infiles $1 > ${1/infiles/log} 

echo "xrdcp $TMP/tmp root://eosatlas.cern.ch/$2"
xrdcp $TMP/tmp root://eosatlas.cern.ch/$2

RTN=$!
echo "rm -r $TMP"
rm -r $TMP

echo "exit $RTN"
exit $RTN
