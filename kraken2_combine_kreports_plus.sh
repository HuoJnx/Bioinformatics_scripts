#!/bin/bash

## Description
# This script is designed to merge multiple Kraken2 reports into a single report. 
# It first searches for all files matching a given pattern within a specified directory. 
# It then uses the `combine_kreports.py` script from Kraken2 to merge these reports into a single file. 
# Finally, it processes this merged report to remove all comment lines (lines starting with `#`) except for the last one.

## Usage
# ./script.sh <input_directory> <output_directory> <search_pattern>

## Arguments
# <input_directory>: The directory to search for Kraken2 report files. 
#                   The script will search this directory and all subdirectories for files matching the search pattern.
# <output_directory>: The directory where the merged and polished Kraken2 report will be saved.
# <search_pattern>: The pattern to match when searching for Kraken2 report files. 
#                   This should be a string that can be recognized by the `find` command's `-name` option. 
#                   For example, to match all files with the `.report` extension, you would use `"*.report"`.

## Output
# The script will produce 3 output files in the specified output directory:
# Merge_kraken2.tsv: This is the raw merged Kraken2 report, produced by `combine_kreports.py`. 
#                    It contains all lines from all input reports, including comment lines.
# Polished_Merge_kraken2.tsv: This is the polished merged Kraken2 report. 
#                             It is the same as `Merge_kraken2.tsv`, but all comment lines except for the last one have been removed.
# 
# Merge_kraken2_f_list: This is the file list for the merge files.

## Dependencies
# KrakenTools: Specifically, the `combine_kreports.py` script from.
# awk: A versatile programming language that is standard on Unix-like operating systems. It is used here for text processing.
# R script "~/BIN/junsheng/IMPORT/import_R.R" and related package.

## Example
# kraken2_combine_kreports_plus.sh $kraken2_dir "." "kraken2_summary.tsv"
set -e
in_dir=$1
out_dir=$2
search_pattern=$3

f_list=$(find $in_dir \( -type f -o -type l \) -name $search_pattern)
n_f=$(echo "$f_list"|wc -l)

printf "Get %s files, they are: \n%s\n" $n_f "$f_list" 

out_unpo="${out_dir}/Merge_kraken2.tsv"
out_po="${out_dir}/Polished_Merge_kraken2.tsv"
out_f_list="${out_dir}/Merge_kraken2_f_list.tsv"

## run combine
echo -e "Creating raw kraken2 merged report.\n"
combine_kreports.py -r $f_list --display-headers -o $out_unpo

## remove the comment lines except for the last
echo -e "Saving polished kraken2 merged reports.\n"
awk '/^#/ {line=$0; next} {if (line) print line; line=""; print}' $out_unpo > $out_po

## save the f_list in combination
echo -e "Saving file list\n"
echo "$f_list" > $out_f_list

## save the df_taxid & df_otu_taxid
echo -e "Sving df_taxid and df_otu_taxid in specific subdirs.\n"
Rscript -e '
options(warn=-1)
source("~/BIN/junsheng/IMPORT/import_R.R")

# Get the input file name from the command line argument
args = commandArgs(trailingOnly = TRUE)
input_file = args[1]

# Read in the data
df = read_tsv(input_file, show_col_types = FALSE)

# Filter and restructure the data
df_s = df %>% select(matches("[0-9]+_lvl"), lvl_type, taxid, name) %>%
                filter(lvl_type == "S" | lvl_type == "U") %>% #### select that in species and unknown
                select(-lvl_type)

# Create the taxa dataframe and write it out
df_taxid = df_s %>% select(taxid)

df_taxid %>% select(taxid) %>% write_df_wrap("all_taxid", df_dir="taxa_table",col_names=F)


# Create the otu dataframe and write it out
df_s_final = df_s %>% select(-name) %>%
                        column_to_rownames("taxid") %>%
                        arrange_by_row(ascending = F)
df_s_final %>% write_df_wrap("df_otu_taxid", df_dir="otu_table", rowname="taxid")
' $out_po

echo -e "All finished.\n"