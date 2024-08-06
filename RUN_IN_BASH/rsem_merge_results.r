#!/usr/bin/env Rscript

print_help = function(){
    cat("
    Usage: Rscript script.R <input_dir> <file_patterns> <extract_count> <output_file_path>

    Arguments:
        <input_dir>           Path to the input directory containing RSEM files.
        <file_patterns>       File pattern to search for (expected '*.isoforms.results').
        <extract_count>       Type of count to extract (expected one of 'expected_count', 'TPM', 'FPKM', 'IsoPct').
        <output_file_path>    Path to the output file where results will be written.


    Example:
        Rscript script.R /path/to/input /path/to/output '*.isoforms.results' 'TPM'
    ")
}

# Get the command line arguments
args = commandArgs(trailingOnly = TRUE)
# Call the help function if not enough arguments or -h/--help is present
if (length(args) < 4 || '--help' %in% args || '-h' %in% args) {
    print_help()
    quit(status = 0)
}

#!/usr/bin/env Rscript
library(tidyverse)
library(foreach)
library(doParallel)
registerDoParallel()

# Your script logic here
input_dir = args[1]
file_patterns = args[2]
extract_count = args[3]
output_file_path = args[4]


# List all '*.genes.results' files in the input directory
f_list_results = list.files(input_dir, pattern = file_patterns, full.names = T)
#f_list_results

# For each file, read the data, select the 'gene_id' and 'TPM' columns, add a 'sample_marker' column, and combine
str_remove_pattern = file_patterns %>% str_remove(coll("*"))
#str_remove_pattern

df_template= read_tsv(f_list_results[1])
first_colname = colnames(df_template)[1]
first_colname_sym = first_colname %>% as.name
extract_count_sym = extract_count %>% as.name

df_final = foreach(file_path = f_list_results, .combine = "bind_rows") %dopar% {
    df_temporary = df_temporary = read_tsv(file_path)
    file_marker = file_path %>% basename %>% str_remove(pattern = str_remove_pattern)
    df_temporary_select = df_temporary %>% select(!!first_colname_sym, !!extract_count_sym) %>%
                                mutate(sample_marker = file_marker)
    return(df_temporary_select)
}
#df_final

# Pivot the data to a wider format
df_final_wider = df_final %>% pivot_wider(id_cols = first_colname, names_from = "sample_marker", values_from = extract_count, values_fill = NA) %>%
                                column_to_rownames(first_colname) %>%# just for checking replication
                                arrange(desc(rowSums(across(everything())))) %>%
                                filter(rowSums(across(everything()))!=0)
#df_final_wider

# Write the data to a TSV file
df_final_wider = df_final_wider %>% rownames_to_column(var = first_colname)

## Check if dir existed
output_dir = dirname(output_file_path)

if (!dir.exists(output_dir)) {
    # Directory does not exist, so create it
    dir.create(output_dir, recursive = T)
    cat("Directory created at:", output_dir, "\n")
} else {
    cat("Directory already exists at:", output_dir, "\n")
}

## and write
write_tsv(df_final_wider, output_file_path)


