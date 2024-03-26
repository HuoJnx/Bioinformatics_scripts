#!/bin/bash
set -e
echo "》》Hello, here is parallel_metagenemark."
# Function to display help
show_help() {
    echo "Usage: $0 [OPTIONS]"
    echo "Run MetaGeneMark in parallel on a multi-FASTA file."
    echo ""
    echo "Options:"
    echo "  -i, --input INPUT_FILE   Input multi-FASTA file or directory containing them."
    echo "  -o, --output OUT_DIR     Output directory for final results."
    echo "  -m, --model MODEL_FILE   MetaGeneMark model file."
    echo "  -n, --n_sequence N       Number of sequences per split file (default: 2000)."
    echo "  -h, --help               Show this help message and exit."
    echo ""
}

# Default values
input_file=""
out_dir=""
model_file=""
n_sequence=2000

# Parse command-line options
while getopts "i:o:m:n:h" opt; do
    case $opt in
        i|--input)
            input_file="$OPTARG"
            ;;
        o|--output)
            out_dir="$OPTARG"
            ;;
        m|--model)
            model_file="$OPTARG"
            ;;
        n|--n_sequence)
            n_sequence="$OPTARG"
            ;;
        h|--help)
            show_help
            exit 0
            ;;
        \?)
            echo "Invalid option: -$OPTARG" >&2
            show_help
            exit 1
            ;;
    esac
done

# Check if required options are provided
if [[ -z "$input_file" || -z "$out_dir" || -z "$model_file" ]]; then
    echo "Error: Missing required options. Use -h or --help for usage."
    show_help
    exit 1
fi

# Rest of your script...
n_cores=$(nproc)

eval input_file="$input_file" #In case the directory has "~"

input_file=$(realpath "$input_file")
echo "》》The input is $input_file."
# Create a dedicated temporary output directory in /dev/shm
time_stamp=$(date +%Y%m%d_%H%M%S)
#time_stamp="111"
temp_out_dir="/dev/shm/MetaGeneMarkOut_$time_stamp"
mkdir -p "$temp_out_dir"
mkdir -p "$out_dir"

if [ -f "$input_file" ]; then
    echo "》》It is a file, need to check duplication and then split."
    
    duplicates=$(seqkit seq -n -i $input_file | sort | uniq -d)
    
    if [[ -n $duplicates ]]; then
        echo "》》Error: Duplicate IDs found:"
        echo "$duplicates"
        exit 1  # Exit the script with an error status
    else
        echo "》》No duplicate IDs found."
    fi
    temp_input_dir="/dev/shm/MetaGeneMark_$time_stamp"
    mkdir -p "$temp_input_dir"
    seqkit split2 --force --quiet -s $n_sequence -O "$temp_input_dir" "$input_file"
    input_dir="$temp_input_dir"
elif [ -d "$input_file" ]; then
    echo "》》It is a directory, don't need to split."
    input_dir="$input_file"
else
    echo "》》Error: Input path is neither a file nor a directory."
    return 1
fi

## remove the description in the split fasta files
find "$input_dir" -type f -name "*.fa"| grep -v "nodescription" | parallel "seqkit replace -p \"\s.+\" {} > {.}_nodescription.fa"

## run gmhmmp in parallel
run_gmhmmp() {
    segment_file=$1
    temp_out_dir=$2
    model_file=$3
    segment_out_subdir="${temp_out_dir}/$(basename ${segment_file})"
    mkdir -p "$segment_out_subdir"
    log_file="${segment_out_subdir}/gmhmmp.log"
    gmhmmp -f G -m "$model_file" -o "${segment_out_subdir}/segment.gff" \
           -A "${segment_out_subdir}/segment.faa" -D "${segment_out_subdir}/segment.fna" \
           "$segment_file" 2>"$log_file"
}
export -f run_gmhmmp

echo "》》Processing files in the input directory: $input_dir"
split_f_list=$(find "$input_dir" -type f -name "*nodescription.fa")
parallel -j $n_cores run_gmhmmp :::: <(echo "$split_f_list") ::: $temp_out_dir ::: $model_file

## removing comment lines
echo "》》Removing comment lines from GFF files."
find "$temp_out_dir" -type f -name "segment.gff" | parallel "grep -v '^# ' {} | grep -v '##'| awk 'NF' > {.}_nocomment.gff"

## polish the output gff
echo "》》Further polished the gff file and make the aux file."
polish_gff() {
    input_file="$1"
    output_file="${input_file%.gff}_modified.gff"
    aux_file="${input_file%.gff}_aux.tsv"

    awk -F"\t" -v OFS='\t' -v output_file="$output_file" -v aux_file="$aux_file" \
    '{
        # modified the $1 in file

        gene_id=$1

        if ($1 in seen) {
            ++seen[$1];
            $1 = $1 "_" seen[$1];
        } else {
            seen[$1] = 1;
            $1 = $1 "_1";
        }

        # Extract the so called gene_id value (actually its order)
        match($9, /gene_id=([0-9]+)/, arr);
        order_marker = arr[1];

        # Change the $9 in the file
        $9 = "gene_id=" gene_id ", " "gene_id_n_cds=" $1 ", " "order=gene_" order_marker;  # Replace $9 with modified value

        # Save
        print > output_file;  # Output modified GFF to file
        print gene_id,$1,"gene_" order_marker > aux_file;  # Output fields to another file
    }' \
    "$input_file"
}

export -f polish_gff
find "$temp_out_dir" -type f -name "segment_nocomment.gff" | parallel "polish_gff {}"

# check the id of faa and fna
echo "》》Get seqid from faa and fna."
## get the seqid file
get_faa_fna_id() {
    input_fa_file="$1"
    output_file="$1.seqid"

    seqkit seq -n "$input_fa_file" | \
    awk -F'\t>' -v OFS='\t' -v output_file="$output_file" \
    '{
        split($1, gene_id, "|");
        print gene_id[1], $2 > output_file;  # Output to specified output file
    }' 
}
export -f get_faa_fna_id
find "$temp_out_dir" -type f \( -name "segment.fna" -o -name "segment.faa" \) | parallel "get_faa_fna_id {}"


## check the seqid file, and check if the order of seqid is equal to gff
check_and_paste_files() {
    dir="$1"
    log_file="$dir/match.log"

    # Use paste to combine the three files column by column
    paste "$dir/segment_nocomment_aux.tsv" "$dir/segment.faa.seqid" "$dir/segment.fna.seqid" | \
    awk -F'\t' -v OFS='\t' -v log_file="$log_file" \
    '
    {
    if (!($1 == $5 && $5 == $7 && $3 == $4 && $4 == $6)) {
            print "Error: Conditions not met on line " NR > log_file
            print $0 >> log_file  # Append the line to the log_file
            exit 1  # Exit with an error code
    }
    }
    END {
        print "All matched." > log_file
    }'
}

export -f check_and_paste_files
sud_dir=$(find "$temp_out_dir" -mindepth 1 -type d)
echo "$sud_dir" | parallel "check_and_paste_files {}"


log_files=$(find "$temp_out_dir" -type f -name "match.log")
count_sub_dir=$(echo "$sud_dir"|wc -l)
count_log_files=$(echo "$log_files"|wc -l)
if [ $count_sub_dir -eq $count_log_files ]; then
    echo "》》The number of log files is equal to the number of subdirectories."
else
    echo "》》The number of log files does not match the number of subdirectories."
    exit 1
fi


# Check each log file for the presence of "Error" word
for log_file in $log_files; do
    if grep -q "Error" "$log_file"; then
        echo "Error: An 'Error' was found in $log_file."
        exit 1  # Exit with an error code
    fi
done

# If no errors were found in any log file, print a success message
echo "》》All match.log files are error-free."

# Call the function to perform the replacement
echo "》》Replace the gene_id with corrected one."
replace_seqids() {
    input_fa_file="$1"
    corrected_seqid_file="$(dirname $1)/segment_nocomment_aux.tsv"
    output_fa_file="${input_fa_file}.correctedID.fa"

    # Use awk to perform the replacement and handle output
    awk -F'\t' '
        NR == FNR {
            # Processing the first file (corrected SeqIDs)
            map[FNR] = $2 " " $3;
            next;
        }
        NR > FNR {
            # Processing the second file (input FASTA)
            if (/^>/) {
                count_lines++
                print ">" map[count_lines];
            } else {
                print;
            }
        }
    ' "$corrected_seqid_file" "$input_fa_file" > "$output_fa_file"
}

export -f replace_seqids
find "$temp_out_dir" -type f \( -name "segment.fna" -o -name "segment.faa" \)| parallel "replace_seqids {}"

# merge all the files from the temp_out_dir to the out_dir
gff_list=$(find "$temp_out_dir" -type f -name "segment_nocomment_modified.gff" | sort)
faa_list=$(echo "$gff_list"| parallel "echo \$(dirname {})/segment.faa.correctedID.fa") ## This way is for keeping the list the same order as gff_list
fna_list=$(echo "$gff_list"| parallel "echo \$(dirname {})/segment.fna.correctedID.fa") ## This way is for keeping the list the same order as gff_list
echo "$gff_list" | parallel --keep-order cat :::: - > "$out_dir/final.gff"
echo "$faa_list" | parallel --keep-order cat :::: - > "$out_dir/final.faa"
echo "$fna_list" | parallel --keep-order cat :::: - > "$out_dir/final.fna"

echo "》》All finished."
echo "》》Remember to remove the temporary files in /dev/shm."