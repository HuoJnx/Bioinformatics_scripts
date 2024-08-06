#!/bin/bash
display_help() {
echo "Description:"
echo "This script calculates the SparCC (Sparse Correlations for Compositional data) correlation and p-values with bootstrap validation for a given OTU (Operational Taxonomic Unit) table."
echo
echo "Usage: $0 <otu_table_path> <output_directory>"
echo
echo "Arguments:"
echo "  <otu_table_path>    Path to the OTU table file"
echo "  <output_directory>  Directory where the output files will be saved"
echo
echo "Options:"
echo "  -h, --help          Display this help message and exit"
echo
echo "Steps:"
echo "1. The script takes the path to the OTU table as an input argument."
echo "2. It aleady sets up various variables and file paths for the analysis."
echo "3. It removes any existing files from previous runs to ensure a clean start."
echo "4. The script modifies the first cell in the first row of the OTU table to '#OTU ID' and saves it as a temporary file."
echo "5. It calculates the correlation and covariance matrices using the 'fastspar' tool with specified parameters."
echo "6. The script generates bootstrap inputs by creating a directory and running 'fastspar_bootstrap' on the temporary OTU table."
echo "7. It runs the bootstrap validation by executing 'fastspar' on each bootstrap input file in parallel using the 'parallel' command."
echo "8. The script calculates the p-values using 'fastspar_pvalues' based on the correlation matrix and bootstrap results."
echo "9. Finally, it checks if the p-value file exists. If it does, the script removes the bootstrap input and output directories and prints a completion message. If the p-value file is missing, it indicates an error and suggests checking the log files."
echo
echo "Output files:"
echo "- corr_sparcc.tsv: Correlation matrix."
echo "- covar_sparcc.tsv: Covariance matrix."
echo "- pvalues_sparcc.tsv: P-values for the correlations."
echo "- Bootstrap input and output directories for intermediate files."
echo
echo "For more details, please refer to the script comments and documentation."
}

if [[ $# -eq 0 || $1 == "-h" || $1 == "--help" ]]; then
    display_help
    exit 0
fi

if [[ $# -ne 2 ]]; then
    echo "Error: Invalid number of arguments."
    echo "Usage: $0 <otu_table_path> <output_directory>"
    exit 1
fi

set -e
export otu_path=$1
export output_dir=$2
export temp_otu_path="${output_dir}/temp_otu.tsv"
export niter=50
export n_exiter=20
export ncpu=64
export marker="sparcc"
export corr_file="${output_dir}/corr_${marker}.tsv"
export cov_file="${output_dir}/covar_${marker}.tsv"
export pval_file="${output_dir}/pvalues_${marker}.tsv"
export dir_bootstrap_input="${output_dir}/bootstrap_counts_${marker}"
export dir_bootstrap_output="${output_dir}/bootstrap_output_${marker}"
export n_bootstrap=1000
export niter_bootstrap=10

## make the output_dir
mkdir -p $output_dir
## remove existed files
rm -rf $temp_otu_path $corr_file $cov_file $pval_file $dir_bootstrap_input $dir_bootstrap_output

# Replace the cell in the first row, first column as "#OTU ID"
echo "Replace the cell in the first row, first column as '#OTU ID'."
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
    --iterations $niter_bootstrap \
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
    rm -rf $temp_otu_path $dir_bootstrap_input $dir_bootstrap_output
    echo -e "\nall finish.\n"
else
    echo -e "Error, please check the log."
fi
