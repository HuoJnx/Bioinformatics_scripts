#!/usr/bin/env Rscript

# Description:
# This script processes OTU data, aggregates it at a specified taxonomic level, 
# calculates the proportion of each OTU, and saves the results.


## suppress warning
options(warn=-1)

# get imports
import_dir=Sys.getenv("MY_IMPORT",unset="~/BIN/junsheng/IMPORT")
import_path=file.path(import_dir,"import_R.R")
source(import_path)

## other library
suppressPackageStartupMessages(library(phyloseq))
suppressPackageStartupMessages(library(microbiome))
suppressPackageStartupMessages(library(microViz))

args = commandArgs(trailingOnly = TRUE)
df_otu_path = args[1]
df_taxa_path = args[2]
level = args[3]


## read df_otu
pretty_print("Read df_otu.")

df_otu=read_tsv(df_otu_path,show_col_types = F)
idx_col=colnames(df_otu)[1]
df_otu=df_otu%>%column_to_rownames(idx_col)

## read df_taxa
pretty_print("Read df_taxa.")

df_taxa=read_tsv(df_taxa_path,show_col_types = F)
idx_col=colnames(df_taxa)[1]
df_taxa=df_taxa%>%column_to_rownames(idx_col)

## get phyloseq obj
pretty_print("Get phyloseq object.")
phy_obj=phyloseq(otu_table(df_otu,taxa_are_rows = T),tax_table(df_taxa%>%as.matrix))

## aggregate to specific level 
pretty_print(sprintf("Aggregate to %s level.",level))
ps_level=aggregate_taxa(phy_obj,level = level)
df_level=ps_level%>%otu_table%>%as.data.frame

## df proportion
pretty_print("Calculate proportion.")
df_level_prop=df_level%>%mutate_all(~.x/sum(.x)*100)%>%
                            mutate_all(~round(.x,3))

## save
pretty_print("Saving.")
df_level_prop%>%write_df_wrap(sprintf("Level_proportion (%s)",level),rowname = level)
