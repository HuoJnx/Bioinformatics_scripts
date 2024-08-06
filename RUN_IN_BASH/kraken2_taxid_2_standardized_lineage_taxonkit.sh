#!/bin/bash
set -e
# Initialize variables with default values
input_file=""
unclassified_marker="Unclassified"

# Function to display help
display_help() {
    echo "Usage: $0 [-i input_file] [-u unclassified_marker] [-h]"
    echo "Options:"
    echo "  -i input_file            Path to the input file (default: empty)"
    echo "  -u unclassified_marker  Unclassified marker (default: Unlcassified)"
    echo "  -h                       Display this help message"
    exit 1
}

# Parse command line options
while getopts "i:u:h" opt; do
    case $opt in
        i)
            input_file="$OPTARG"
            ;;
        u)
            unclassified_marker="$OPTARG"
            ;;
        h)
            display_help
            ;;
        \?)
            echo "Invalid option: -$OPTARG"
            display_help
            ;;
    esac
done

# Check if input_file is empty
if [ -z "$input_file" ]; then
    echo "！！！Error: You must specify an input file using the -i option."
    display_help
fi

# Determine the output directory and filename
output_dir=$(dirname "$input_file")
output_file_raw="$output_dir/df_taxa_standardized_raw.tsv"
output_file_polished="$output_dir/df_taxa_standardized_polished.tsv"

printf "》》Input taxid is %s, will output lineage to %s.\n" "$input_file" "$output_file_raw"

# Step 1: Perform taxonkit reformat and add header using sed
taxonkit reformat -f "{k}\t{p}\t{c}\t{o}\t{f}\t{g}\t{s}" -I 1 -i 2 -r "$unclassified_marker" "$input_file" > "$output_file_raw"

cat "$output_file_raw" \
    | sed '1i\taxid\tkindom\tphylum\tclass\torder\tfamily\tgenus\tspecies' > "$output_file_polished"

echo "》》All finished, file is saved to $output_file_polished."