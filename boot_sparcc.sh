#!/bin/bash


set -e
export otu_path=$1
export temp_otu_path="temp_otu.tsv"
export niter=50
export n_exiter=20
export ncpu=64
export marker="sparcc"
export corr_file="corr_${marker}.tsv"
export cov_file="covar_${marker}.tsv"
export pval_file="pvalues_${marker}.tsv"
export dir_bootstrap_input="bootstrap_counts_${marker}"
export dir_bootstrap_output="bootstrap_output_${marker}"
export n_bootstrap=1000
export n_iter_bootstrap=5

## remove existed files
rm -rf $temp_otu_path $corr_file $cov_file $pval_file $dir_bootstrap_input $dir_bootstrap_output


## replace the cell in first row first column as "#OTU ID"
echo "Replace the cell in the first row first column as '#OTU ID'."
awk 'BEGIN{FS=OFS="\t"} NR==1{$1="#OTU ID"}1' $otu_path > $temp_otu_path

## calculate cor cov
echo "Calculating cor and cov matrix."
fastspar --otu_table $temp_otu_path \
        --correlation $corr_file \
        --covariance $cov_file \
        --iterations $niter \
        --exclude_iterations $n_exiter \
        --threads $ncpu \
        --yes \
        &>log.cor_cov

## generate bootstrap inputs
echo "Generating bootstrap inputs"
mkdir -p $dir_bootstrap_input

fastspar_bootstrap --otu_table $temp_otu_path \
                    --number $n_bootstrap \
                    --prefix "${dir_bootstrap_input}/" \
                    &>log.generate_bootstrap

## bootstrap
echo "Running bootstrap validation."
boot_sparcc(){
    set -e
    output1="${dir_bootstrap_output}/cor${2}"
    output2="${dir_bootstrap_output}/cov${2}"
    echo -e "\nHello! Processing $1, will output $output1 and $output2.\n"
    fastspar --otu_table $1 \
    --correlation $output1 \
    --covariance $output2 \
    --iterations $n_iter_bootstrap \
    --threads $ncpu \
    --yes
}
export -f boot_sparcc

mkdir -p $dir_bootstrap_output

find $dir_bootstrap_input -type f|parallel -j $ncpu "boot_sparcc {} {/}" &>log.bootstrap

## calculate p
echo "Calculating p-values."
fastspar_pvalues --otu_table $temp_otu_path \
                --correlation $corr_file \
                --prefix "${dir_bootstrap_output}/cor_" \
                --permutations=$n_bootstrap \
                --threads=$ncpu \
                --outfile $pval_file \
                &>log.calculate_p

## detect if the pval file exists
if [ -f "$pval_file" ];then
    rm -rf $dir_bootstrap_input $dir_bootstrap_output
    echo -e "\nall finish.\n"
else
    echo -e "Error, please check the log."
fi
