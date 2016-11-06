#!/bin/bash

cd ~/Programming/bcalib.git
bcalib --outfile ${1/infiles/hist.gz} --infiles $1 > ${1/infiles/log} 
