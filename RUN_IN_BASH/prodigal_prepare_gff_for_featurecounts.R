#!/usr/bin/env Rscript
library(argparse)


# Function to print help message
print_help = function() {
    cat("Usage: ./script_name.R -i <input_file>\n\n")
    cat("Description:\n")
    cat("This script processes the raw Prodigal gff to that ready for featurecounts.\n\n")
    cat("Options:\n")
    cat("  -i, --input <input_file>: Specifies the path to the input TSV file.\n\n")
    cat("Example:\n")
    cat("  ./script_name.R -i input_data.tsv\n")
}

# Parse command line arguments
parser = ArgumentParser()
parser$add_argument("-i", "--input", help = "Input data frame file path")
args = parser$parse_args()

input_file = args$input
output_file = file.path(dirname(args$input), "res_for_read_count.gff")

# Display help message if no arguments provided or if -h/--help used
if (length(args$input) == 0 || "-h" %in% args || "--help" %in% args) {
    print_help()
    quit(save = "no")
}







# Process command line options
suppressPackageStartupMessages(library(tidyverse))

# Read the input data frame
input_df = read_tsv(input_file, comment = "#", col_names = FALSE)

# Perform data processing
df_X1_X9 = input_df %>%
    select(X1, X9) %>%
    mutate(ID = str_extract(X9, "ID=[0-9]+_[0-9]+")) %>%
    mutate(ID = str_remove(ID, "ID=")) %>%
    separate(ID, into = c("seq_order", "CDS_order"), sep = "_") %>%
    group_by(X1) %>%
    mutate(my_CDS_order = row_number()) %>%
    ungroup() %>%
    mutate(match = (CDS_order == my_CDS_order))

# Validate data
if (df_X1_X9 %>% pull(match) %>% all) {
    print("Validation passed.")
} else {
    stop("Not all passed.")
}

# Create new X9 column
df_new_X9 = df_X1_X9 %>%
    select(X1, CDS_order) %>%
    mutate(X9 = paste(X1, CDS_order, sep = "_")) %>%
    select(X9)

# Create the final data frame
df_final = input_df %>% mutate(X9 = sprintf("ID=%s", df_new_X9$X9))

# Write the final data frame to a file

write_tsv(df_final, file = output_file, col_names = FALSE)

cat("All finished.\n")
