#!/usr/bin/env Rscript

args = commandArgs(trailingOnly = TRUE)

# Check for the correct number of arguments
if (length(args) < 3) {
    cat("Usage: bash_compare_2_column.R <path_to_dataframe1> <path_to_dataframe2> <ignore_order: TRUE|FALSE>\n")
    cat("This script compares the rows of two data frames for equality, with an option to ignore order.\n")
    quit(status = 1)
}

suppressPackageStartupMessages(library(tidyverse))

# Assign command line arguments to variables
df1_path = args[1]
df2_path = args[2]
ignore_order = args[3] %>% as.logical



# Read the first 5 rows of each dataframe without column names
cat("Reading 2 data.frames.\n")

df1 = read_tsv(df1_path, col_names = F)
df2 = read_tsv(df2_path, col_names = F)

# Check if the number of rows is equal
cat("Checking rownumber.\n")
if (nrow(df1) != nrow(df2)) {
    stop("The number of rows not equal!")
}

# Whether ignore order
if(ignore_order){
    df1 = df1 %>% arrange(X1)
    df2 = df2 %>% arrange(X1)
}

# Merge, rename columns, add rank, and check for equality
cat("Checking columns\n")
df_merge = bind_cols(df1, df2) %>%
    setNames(c("df1", "df2")) %>%
    mutate(rank = row_number()) %>%
    mutate(equal = (df1 == df2))

# Conditionally write non-equal rows to stdout
if (!all(df_merge$equal)) {
    df_merge %>% 
        filter(!equal) %>%
        write_tsv(file = stdout())
} else {
    cat("All equal.\n")
}
