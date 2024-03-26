#!/usr/bin/env python
# coding: utf-8

import pandas as pd
from pandas import DataFrame as DF

from ete3 import NCBITaxa
from dask import delayed, compute
import dask
import sys
import importlib
import argparse

def main(sf_path, input_file, unclassified_marker="Unclassified"):
    sys.path.append(sf_path)
    sf = importlib.import_module("import_python")
    sf = importlib.reload(sf)

    df = pd.read_csv(input_file, sep="\t", header=None)
    unique_taxid = sf.unique(df)

    @delayed
    def get_lineage_data(taxid):
        ## get ncbi module
        ncbi = NCBITaxa()

        ## get lineage
        try:
            lineage = ncbi.get_lineage(taxid)
        except ValueError: # some times if can't find, will raise error
            lineage = None

        if lineage is not None:
            ranks = ncbi.get_rank(lineage)
            inverted_ranks = {v: k for k, v in ranks.items()}
            inverted_ranks = {"taxid": taxid, **inverted_ranks}
            return inverted_ranks

    # Parallel execution
    tasks = [get_lineage_data(taxid) for taxid in unique_taxid]

    merge_res_tuple = compute(*tasks)
    merge_res_tuple = [result for result in merge_res_tuple if result is not None]

    df_taxa = DF(merge_res_tuple)
    df_taxa = df_taxa.fillna(-1)
    df_taxa = df_taxa.applymap(lambda x: int(x))
    df_taxa = sf.column_to_rownames(df_taxa, "taxid")

    ncbi = NCBITaxa()
    unique_values = sf.unique(df_taxa)
    dict_name = ncbi.get_taxid_translator(unique_values)
    dict_name = {-1: unclassified_marker, **dict_name}

    df_taxa_name = df_taxa.applymap(lambda x: dict_name.get(x, unclassified_marker))

    common_list = ["superkingdom", "phylum", "class", "order", "family", "genus", "species"]
    df_taxa_name_common = sf.select(df_taxa_name, *common_list)
    df_taxa_name_common.loc[0] = [unclassified_marker] * len(df_taxa_name_common.columns)# Index is taxid, and 0 always is "Unclassified"
    df_taxa_name_common = df_taxa_name_common.sort_index()

    # Save df_taxa_name_common in the same directory as input_file
    save_dir = sf.dirname(input_file)
    sf.write_df_wrap(df_taxa_name_common, file_name="df_taxa_standardized", save_dir=save_dir)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Process all_taxid file and generate df_taxa_standardized.tsv in the same directory of the input file.")
    parser.add_argument("-s", "--sf_path", type=str, help="Path to self function directory")
    parser.add_argument("-i", "--input_file", type=str, help="Path to the file for all taxid you want to get lineage")
    parser.add_argument("-u", "--unclassified_marker", type=str, default="Unclassified", help="Marker for 'Unclassified' value (default is 'Unclassified')")
    args = parser.parse_args()

    main(args.sf_path, args.input_file, args.unclassified_marker)
