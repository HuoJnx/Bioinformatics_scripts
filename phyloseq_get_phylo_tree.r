#!/usr/bin/env Rscript


# Description:
# This script processes reference data and user-provided taxa data to generate a customized phylogenetic tree.
# It reads the reference data, filters and renames columns, and then matches taxa from the user data.
# The final output is a phylogenetic tree with taxa names replaced based on user input.

# Input:
# - Reference GTDB data
# - Reference GTDB tree
# - User-provided taxa data
# - Column names for taxid and taxa_name from user data

# Output:
# - A customized phylogenetic tree saved as 'tree_obj.rd' in the 'phylo_tree' directory
# - A table of taxa called "taxa_have_tree" that have tree information, saved in the 'taxa_table' directory

## suppress warning
options(warn=-1)

# get imports
import_dir=Sys.getenv("MY_IMPORT",unset="~/BIN/junsheng/IMPORT")
import_path=file.path(import_dir,"import_R.R")
source(import_path)

# get argv
args = commandArgs(trailingOnly = TRUE)
if (length(args) != 5) {
    stop("You must supply the paths for ref_gtdb, ref_gtdb_tree, and df_taxa, and the column names for taxid and taxa_name")
}

path_ref_gtdb = args[1]
path_ref_gtdb_tree = args[2]

ref_col_taxid="ncbi_species_taxid"
ref_col_genome_representative="gtdb_genome_representative"
path_df_taxa = args[3]
col_taxid = args[4]
col_taxa_name = args[5]


# librarys
suppressPackageStartupMessages(library(ape))
suppressPackageStartupMessages(library(phyloseq))
suppressPackageStartupMessages(library(microbiome))
suppressPackageStartupMessages(library(microViz))


# read reference
pretty_print("Read reference.")
df_gtdb=read_tsv(path_ref_gtdb,show_col_types = F)#"/nasb1_d3/junsheng/REF/GTDB/bac120_metadata_r214.tsv"


gtdb_tree=read.tree(path_ref_gtdb_tree)#"/nasb1_d3/junsheng/REF/GTDB/bac120_r214.tree"


df_gtdb_lite=df_gtdb%>%select(all_of(c(ref_col_taxid,ref_col_genome_representative)))%>%
                        mutate(!!ref_col_taxid:=.data[[ref_col_taxid]]%>%as.character)%>%
                        rename(!!col_taxid:=ref_col_taxid)


# read the user df_taxa for getting customized tree
pretty_print("Read user df_taxa.")
df_taxa_has_taxid=read_tsv(path_df_taxa,show_col_types = F)

df_taxa_has_taxid_unique=df_taxa_has_taxid%>%
                            select(all_of(c(col_taxid,col_taxa_name)))%>%
                            distinct(.data[[col_taxid]],.keep_all=T)%>%## only allow unique taxid
                            distinct(.data[[col_taxa_name]],.keep_all=T)%>%## only allow unique col_taxa_name
                            mutate(!!col_taxid:=.data[[col_taxid]]%>%as.character)


pretty_print("Filter reference.")
df_needed_representative=df_taxa_has_taxid_unique%>%left_join(df_gtdb_lite)%>%
                                            distinct(.data[["gtdb_genome_representative"]],.keep_all = T)%>%## only allow unique genome representation
                                            distinct(.data[[col_taxid]],.keep_all = T)%>%## only allow unique taxid
                                            drop_na





# tree operation
pretty_print("Tree operation.")
## only keep the tips in df_needed_representative
needed_tree=gtdb_tree%>%keep.tip(df_needed_representative[[ref_col_genome_representative]])
## the order of the tree don't support reorder, so I have to reorder the df_needed_representative, to get the right order of the taxa name
df_needed_representative_rearrange=df_needed_representative%>%arrange_by_order_vector(ref_col_genome_representative,order_vector = needed_tree[["tip.label"]])
needed_tree_rename=needed_tree
## replace the taxa name
needed_tree_rename[["tip.label"]]=df_needed_representative_rearrange[[col_taxa_name]]


# save things
pretty_print("Saving results.")
## save the taxa have tree info
df_needed_representative%>%write_df_wrap("taxa_have_tree",df_dir = "taxa_table")

## save trees
phylo_tree_dir="phylo_tree"
dir.create(phylo_tree_dir,showWarnings = F)
file_name=file.path(phylo_tree_dir,"tree_obj.rd")
needed_tree_rename%>%saveRDS(file_name)
pretty_print(sprintf("Saving %s",file_name))

ref_n_tips=length(gtdb_tree[["tip.label"]])
n_otu_df_taxa=nrow(df_taxa_has_taxid)
n_otu_df_taxa_unique=nrow(df_taxa_has_taxid_unique)
filtered_n_tips=length(needed_tree[["tip.label"]])
pretty_print(sprintf("The df_taxa has %s OTUs, %s of them are unique.",n_otu_df_taxa,n_otu_df_taxa_unique))
pretty_print(sprintf("Reference has %s tips,  and only %s left after filtering.",ref_n_tips,filtered_n_tips))


