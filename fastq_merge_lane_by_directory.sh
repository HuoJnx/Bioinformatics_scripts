#!/bin/bash

## description
#### It receives a directory, then it will check if their are *.fq or *.fastq or *.fq.gz
#### It assumes the directory contains high throughtput sequencing results in paired end, 
#### It assumes the paired end results are in the following format: *_1.suffix and *_2.suffix,
#### It assumes their are multiple lanes for a single end, and each lane gives a single file,
#### then it will merge all the seuqnecing files for the same end into single file.
#### then it will use repair.sh in BBMap to confirm the merged file are paired again.
#### Finally it will use seqkit to give summary of all the fq files, and remove the merge.1/2.fq.gz
#### Package requirement: pigz, BBMap, seqkit


# Get the directory path from the command line argument
dir=$1
exec 1> ${dir}/log_merge_lane.log 2>&1

# Check the file type and perform the merging and repairing
echo "Merging."
if ls ${dir}/*_1.fq.gz 1> /dev/null 2>&1; then ## if find something, although it won't print, it still will return the status code 0, which will be interpret as true
    zcat ${dir}/*_1.fq.gz | pigz -c > ${dir}/merged.1.fq.gz
    zcat ${dir}/*_2.fq.gz | pigz -c > ${dir}/merged.2.fq.gz
elif ls ${dir}/*_1.fq 1> /dev/null 2>&1; then
    cat ${dir}/*_1.fq | pigz -c > ${dir}/merged.1.fq.gz
    cat ${dir}/*_2.fq | pigz -c > ${dir}/merged.2.fq.gz
elif ls ${dir}/*_1.fastq 1> /dev/null 2>&1; then
    cat ${dir}/*_1.fastq | pigz -c > ${dir}/merged.1.fq.gz
    cat ${dir}/*_2.fastq | pigz -c > ${dir}/merged.2.fq.gz
fi

## repair order
repair.sh overwrite=t in1=${dir}/merged.1.fq.gz in2=${dir}/merged.2.fq.gz out1=${dir}/repaired.1.fq.gz out2=${dir}/repaired.2.fq.gz

## give reports
echo "Give reports for merging lane."
seqkit stats -j100 ${dir}/*.fq.gz 1> ${dir}/merge_lane_summary.log 2>&1

## remove
echo "Remove merged files and leave the repaired files."
rm -rf ${dir}/merged.1.fq.gz ${dir}/merged.2.fq.gz

## echo
echo "All finished."