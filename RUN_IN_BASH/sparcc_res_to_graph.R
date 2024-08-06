#!/usr/bin/env Rscript

args = commandArgs(trailingOnly = TRUE)

if (length(args) != 3) {
  stop("Usage: script.R <directory> <p_value_threshold> <abs_weight_threshold>")
}

data_dir = args[1]
p_thresh = as.numeric(args[2])
weight_thresh = as.numeric(args[3])

# libraries
library(igraph)
suppressPackageStartupMessages(library(tidyverse))

## read corr & p
corr_file = file.path(data_dir, "corr_sparcc.tsv")
p_file = file.path(data_dir, "pvalues_sparcc.tsv")

df_weight = read_tsv(corr_file) %>% column_to_rownames("#OTU ID")
df_p = read_tsv(p_file) %>% column_to_rownames("#OTU ID")

## filter by p-value and absolute weight thresholds
filtered_df_weight = map2_df(.x = df_weight, .y = df_p, .f = function(x, y) { ifelse(y <= p_thresh & abs(x) >= weight_thresh, x, 0)}) %>% as.data.frame

rownames(filtered_df_weight) = rownames(df_weight)

## create igraph object
g = graph_from_adjacency_matrix(filtered_df_weight %>% as.matrix,
                                mode = 'undirected',
                                weighted = TRUE,
                                diag = FALSE)

num_nodes = vcount(g)
num_edges = ecount(g)

print(sprintf("Using p-value threshold: %s and absolute weight threshold: %s", p_thresh, weight_thresh))
print(sprintf("Number of nodes and edges in the filtered graph: %s, %s", num_nodes, num_edges))

output_file = file.path(data_dir, sprintf("graph_p%s_weight%s.rd", p_thresh, weight_thresh))
saveRDS(g, file = output_file)