#!/usr/bin/env Rscript



## Description:
## This R script reads a GFF (General Feature Format) file and performs the following operations:
##
## 1. Reads the input GFF file specified by the input path.
## 2. Ranks the "seqid" values within the GFF file and creates a new column called "marker". 
##    The ranking is based on the occurrence of each "seqid" in the file. For example, if a 
##    "seqid" value "k111" appears multiple times with different CDS entries, the "marker" column
##     will contain unique values like "k111_1", "k111_2", "k111_3" to differentiate those entries.
## 3. Generates the "attributes" column using the "marker" values. Each "attributes" entry is in the
##    format "ID=marker_value;", providing a unique identifier for each feature.
## 4. Saves the modified GFF data with the added "marker" and "attributes" columns to a new file 
##    specified by the output path.
##

## Usage:
## 1st argv: Provide the input GFF file path 
## 2nd argv: Save GFF path
## 3rd argv: the marker list for filtering, it should not include the header.



## args
args = commandArgs(trailingOnly = T)

input_path=args[1]
output_path=args[2]
if(length(args)==3){
    print("Detect included_marker_path, will filtered based on it.")
    included_marker_path=args[3]
}

# get imports
import_dir=Sys.getenv("MY_IMPORT",unset="~/BIN/junsheng/IMPORT")
import_path=file.path(import_dir,"import_R.R")
source(import_path)

## library
library(ape)

## run

#### read file
print("Reading file.")
df_gff=read.gff(file=input_path)




#### give rank
print("Giving rank for seqkid, and make a new column 'marker'.")
df_gff_mutate=df_gff%>%group_by(seqid)%>%
                        mutate(marker=paste(seqid,row_number(),sep="_"))


#### filter by df_included_marker
if(!is.null(included_marker_path)){
    print("Get included_marker_path, filtering gff based on it.")
    ###### read included_marker_path
    df_included_marker=read_tsv(included_marker_path,col_names=F)
    colnames(df_included_marker)=c("marker")

    ###### filtering

    filtered_df_gff=df_gff_mutate%>%right_join(df_included_marker)
    df_gff_mutate=filtered_df_gff
}



#### make attributes column, and remove marker column
print("Making the attributes using 'marker'")
df_gff_mutate2=df_gff_mutate%>%mutate(attributes=paste0("ID=",marker,";"))%>%
                                    select(-marker)




## save
print("Saving.")
output_dir=dirname(output_path)
dir.create(output_dir,showWarnings = F,recursive = T)
df_gff_mutate2%>%write_tsv(file=output_path,col_names = F,num_threads=200)