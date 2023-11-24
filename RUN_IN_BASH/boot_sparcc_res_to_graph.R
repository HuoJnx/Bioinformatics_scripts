#!/usr/bin/env Rscript

args = commandArgs(trailingOnly = TRUE)

thresh_list=sapply(args, as.numeric)


# get imports
import_dir=Sys.getenv("MY_IMPORT",unset="~/BIN/junsheng/IMPORT")
import_path=file.path(import_dir,"import_R.R")
source(import_path)

# librarys
library(igraph)

## read corr & p
df_weight = read_tsv("corr_sparcc.tsv") %>% column_to_rownames("#OTU ID")
df_p = read_tsv("pvalues_sparcc.tsv") %>% column_to_rownames("#OTU ID")

## mask p > thresh


## to igraph

for(thresh in thresh_list){
    masked_df_weight = map2_df(.x = df_weight, .y = df_p, .f = ~if_else(.y > thresh, 0, .x)) %>% as.data.frame
    rownames(masked_df_weight) = rownames(df_weight)
    g = graph_from_adjacency_matrix(masked_df_weight %>% as.matrix,
                               mode = 'undirected',
                               weighted = TRUE,
                               diag = FALSE)
    num_nodes = vcount(g)
    num_edges = ecount(g)
    print(sprintf("Using %s as thresh, Number of nodes and edges in graph: %s, %s", thresh, num_nodes, num_edges))
    write_graph(g, file = sprintf("graph_%s.gml",thresh), format = "gml")
}
