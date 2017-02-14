#!/bin/bash

export X509_USER_PROXY=$HOME/vomsproxy

OUTFOLDER=${1/infiles/run}
echo $OUTFOLDER

cd $PBS_O_WORKDIR
source /cvmfs/atlas.cern.ch/repo/ATLASLocalRootBase/user/atlasLocalSetup.sh
localSetupROOT
mkdir $OUTFOLDER
bcalibRun --outfile $OUTFOLDER/hist.gz --infiles $1 > $OUTFOLDER/log 2>&1
