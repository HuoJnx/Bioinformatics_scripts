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
output_file="$output_dir/df_taxa_standardized.tsv"

printf "》》Input taxid is %s, will output lineage to %s.\n" "$input_file" "$output_file"

# Step 1: Perform taxonkit reformat and add header using sed
taxonkit reformat -I 1 -r "$unclassified_marker" "$input_file" \
    | sed 's/;/\t/g' \
    | sed '1i\taxid\tkindom\tphylum\tclass\torder\tfamily\tgenus\tspecies' > "$output_file"

echo "》》All finished, file is saved to $output_file."