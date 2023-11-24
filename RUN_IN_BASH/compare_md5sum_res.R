#!/usr/bin/env Rscript
options(warn = -1)
## description
#### 1st args: The md5sum file from others
#### 2nd args: The md5sum file from ours
#### 3rd args: The output of comparision

args = commandArgs(trailingOnly = TRUE)

old_md5_file = args[1]
new_md5_file = args[2]
output_file = args[3]

suppressPackageStartupMessages(library(tidyverse))


df_old_md5 = read_delim(old_md5_file, col_names = F,show_col_types = FALSE) %>%
                select(-X2) %>%
                mutate(X3 = basename(X3)) %>%
                rename_with(~paste("old", .x, sep = "_"))

df_new_md5 = read_delim(new_md5_file, col_names = F,show_col_types = FALSE) %>%
                select(-X2) %>%
                mutate(X3 = basename(X3)) %>%
                rename_with(~paste("new", .x, sep = "_"))

df_compare_res = df_old_md5 %>%
                    left_join(df_new_md5, by = c("old_X3" = "new_X3")) %>%
                    mutate(consistent = ifelse(old_X1 == new_X1, T, F))

df_compare_res %>% write_tsv(file = output_file)

print("Comparison finished.")