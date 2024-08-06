#!/bin/bash
set -e

# Function to display help
display_help() {
    echo "Usage: $0 -i <input_dir> -o <output_dir> -p <search_pattern>"
    echo "Description: This script performs a series of operations on files matching a search pattern in the input directory."
    echo "Options:"
    echo "  -i <input_dir>    Input directory containing files to process."
    echo "  -o <output_dir>   Output directory where results will be saved."
    echo "  -p <search_pattern> Pattern to search for files in the input directory."
    echo "  -h, --help        Display this help message."
    exit 0
}

# Parse command-line options
while getopts ":i:o:p:h" opt; do
    case "$opt" in
        i) in_dir="$OPTARG";;
        o) out_dir="$OPTARG";;
        p) search_pattern="$OPTARG";;
        h) display_help;;
        \?) echo "Invalid option: -$OPTARG" >&2
            display_help
            ;;
    esac
done

# Check for required options
if [ -z "$in_dir" ] || [ -z "$out_dir" ] || [ -z "$search_pattern" ]; then
    echo "Error: Missing required options."
    display_help
fi

## get variable prepared

f_list=$(find "$in_dir" \( -type f -o -type l \) -name "$search_pattern")
n_f=$(echo "$f_list" | wc -l)

printf "》》Get %s files, they are: \n%s\n" $n_f "$f_list" 

out_unpo="${out_dir}/Merge_kraken2_raw.tsv"
out_po="${out_dir}/Merge_kraken2_polished.tsv"
out_f_list="${out_dir}/Merge_kraken2_f_list.tsv"
out_taxid="${out_dir}/raw_taxa_table/all_taxid.tsv"

## make directory prepared
dir_taxa_table=$(dirname "$out_taxid")
mkdir -p "$dir_taxa_table"

## run combine
echo -e "》》Creating raw kraken2 merged report.\n"
combine_kreports.py -r $f_list --display-headers -o $out_unpo



## get needed lines
echo -e "》》Parse the raw report for further polish.\n"
header=$(grep -E '^#' "$out_unpo")
table=$(grep -v -E '^#' "$out_unpo")

inputs_info=$(echo "$header" | grep -E '^#S[0-9]+') #inputs info are started with "#S[0-9]+"
colname=$(echo "$header" | tail -n 1 | sed 's/#//') #only select the tail 1 and remove the "#" character


## save the real table
echo -e "》》Saving the table\n"
realtable=$(echo "${colname}"; echo "${table}")
echo "$realtable" >  $out_po

## save the f_list in combination
echo -e "》》Saving file list\n"
echo "$inputs_info" > $out_f_list

## save the df_taxid
echo -e "》》Saving df_taxid in specific subdirs.\n"
taxid=$(echo "$realtable" | csvtk cut -t -f "taxid" | tail -n +2) # extract teh column with head "taxid", but remove the header
echo "$taxid" > $out_taxid

## prompt
echo -e "》》All finished.\n"