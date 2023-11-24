#!/usr/bin/env Rscript

## description
#### It can read output from featureCounts, then output a polished featureCounts matrix and TPM matrix
#### 1st argv: input_path of featureCounts results
#### 2st argv: output_dir, directory is ok, not file name, because it will give 2 files in the dir
#### 3st argv: Whether to polish the names of the matrix. Because the names are filenames
####           it will contains directory name and suffix. You can choose to remove them.
#### 4th argv: The rowname to save when finally saving the 2 matrix

# get imports
import_dir=Sys.getenv("MY_IMPORT",unset="~/BIN/junsheng/IMPORT")
import_path=file.path(import_dir,"import_R.R")
source(import_path)

## args
args = commandArgs(trailingOnly = T)

input_path=args[1]
output_dir=args[2]
if((args[3]!="T")&(args[3]!="F")){
    stop("The 3rd args should be 'T' or 'F'.")
}else{
    polish_name=as.logical(args[3])
}
save_rowname=args[4]




## script for TPM
apply_vector_2_dataframe_col_by_col=function(df,vec,operator="/",strict=T){
    if(strict){
        if(nrow(df)!=length(vec)){
            stop("Please ensure the nrow of dataframe is equal to length of vector.")
        }
    }
    exp=paste("df",operator,"vec")
    new_df=eval(rlang::parse_expr(exp))
    return(new_df)
}
apply_vector_2_dataframe_row_by_row=function(df,vec,operator="/",strict=T){
    if(strict){
        if(ncol(df)!=length(vec)){
            stop("Please ensure the ncol of dataframe is equal to length of vector.")
        }
    }
    df_t=df%>%t
    exp=paste("df_t",operator,"vec")
    new_df=eval(rlang::parse_expr(exp))
    new_df_t=new_df%>%t%>%as.data.frame
    return(new_df_t)
}
calculate_TPM=function(df,length_vector){
    df_temp=df%>%apply_vector_2_dataframe_col_by_col(length_vector,operator = "/")
    colsum_df_tpm=colSums(df_temp)
    df_tpm=df_temp%>%apply_vector_2_dataframe_row_by_row(colsum_df_tpm,operator = "/")
    df_tpm=df_tpm*10^6
    return(df_tpm)
}



## run

df_readcount=read_tsv(input_path,comment = "#")

length_vec=df_readcount$Length

df_readcount_pure=df_readcount%>%column_to_rownames("Geneid")%>%
                                    select(-c(Chr,Start,End,Strand,Length))

#### rename
if(polish_name){
    df_readcount_pure=df_readcount_pure%>%rename_with(basename_without_suffix)
}

#### to TPM
df_tpm=calculate_TPM(df_readcount_pure,length_vec)


## save
df_readcount_pure%>%write_df_wrap(df_name="polished_readcounts",df_dir=output_dir,rowname=save_rowname)
df_tpm%>%write_df_wrap(df_name="polished_TPM",df_dir=output_dir,rowname=save_rowname)
