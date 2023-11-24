#!/bin/bash

taxaranks_parallel(){

    ## stop after error
    set -e
    
    ##parse the input path, then get the input_dir info
    input=$1
    input_dir=$(dirname $input|xargs realpath)
    base=$(basename $input)
    real_input="${input_dir}/${base}"

    
    ## define others variables
    time_mark=$(date +%Y%m%d_%H%M%S)
    
    
    temp_dir="${input_dir}/split_${time_mark}"
    
    merge_file="${input_dir}/${base}.lineage"
    merge_file_with_head="${input_dir}/${base}.lineage.with_head"
    df_taxa="${input_dir}/df_taxa_standardized.tsv"
    
    total_queries=$(wc -l $real_input|awk '{print $1}')
    n_threads=$(nproc)
    echo "Total queries = $total_queries, threads = $n_threads"
    if [ $total_queries -gt $n_threads ]
    then
        n_pieses=$n_threads
        echo "Total queries is greater than threads, n_pieses = $n_threads"
    else
        n_pieses=$total_queries
        echo "Total queries is not greater than threads, n_pieses = $total_queries"
    fi
    
    digi_width=4

    echo -e "Now is $time_mark.\n"
    echo -e "Input is $input,\n Temporary directory is $temp_dir .\n"
    echo -e "Output is $merge_file_with_head.\n"
    
    ## check all temporary files
    
    if [ -f "$merge_file" ]; then
        echo "$merge_file existed, please delete it before running."
        exit 1
    fi
    
    if [ -f "$merge_file_with_head" ]; then
        echo "$merge_file_with_head existed, please delete it before running."
        exit 1
    fi

    ## make temp_dir and split
    echo "Create temporary directory $temp_dir."
    mkdir -p $temp_dir

    
    ## split

    split -a $digi_width -d -n "l/${n_pieses}" $real_input "${temp_dir}/prefix_"

    
    ## run taxaranks in parallel
    echo -e "Annotating...\n"
    f_list=$(find ${temp_dir} -type f)
    echo "$f_list"|parallel "taxaranks -i {} -o {.}.lineage -t"
    
    ## merge

    echo -e "Merging...\n"
    #### drop the first row for each file, then merge
    output_list=$(find ${temp_dir} -type f -name "*.lineage")

    echo "$output_list" |parallel "awk 'NR>1 {print}' {} &>> $merge_file"
    
    #### add the first row for the merge file
    echo -e "Add the first line from the first lineage file.\n"
    
    first_file=$(echo "$output_list"|head -n1)
    head_line=$(head -n1 $first_file)
    
    awk -v a="$head_line" 'BEGIN{print a} {print $0}' $merge_file &>$merge_file_with_head
    
    #### only select the columns user_taxa, superkingdom, phylum, order, family, genus,
    #### species, then rename columns user_taxa to taxid
    echo -e "Creating df_taxa.\n"

    Rscript -e '
    options(warn=-1)
    suppressPackageStartupMessages(library(tidyverse))
    # Get the input file name from the command line argument
    args = commandArgs(trailingOnly = TRUE)
    real_input=args[1]
    merge_file_with_head = args[2]
    output_file = args[3]

    df_ref = read_tsv(real_input,col_names=F,show_col_types = F,col_types=c("c"))

    df_merge = read_tsv(merge_file_with_head,show_col_types = F)
    df_merge = df_merge %>% select(user_taxa, superkingdom, phylum, class, order, family, genus, species) %>%
                            rename("taxid"="user_taxa") %>%
                            mutate_all(~ifelse(is.na(.x),"Unclassified",.x))%>%
                            mutate(taxid=taxid%>%as.character)
    df_merge = df_merge %>% right_join(df_ref, by = c("taxid"="X1"))
    write_tsv(df_merge, file = output_file)
    ' $real_input $merge_file_with_head $df_taxa


    ## remove temporary files
    mv -f $merge_file_with_head $merge_file
    rm -rf $merge_file_with_head
    rm -rf $temp_dir
    echo "Clear temporary files and directory."
    
    
    ## prompt
    echo "All finished."
}
export -f taxaranks_parallel


## description
# It gets the taxid, then output the full lineage for df_taxa in phyloseq object.

## dependency
# taxaranks in taxonomy_ranks packages
# R tidyverse
# GNU awk
# GNU parallel
# GNU split


## example
# kraken2_taxid_2_standardized_lineage.sh taxa_table/all_taxid.tsv

taxaranks_parallel $1 $2

