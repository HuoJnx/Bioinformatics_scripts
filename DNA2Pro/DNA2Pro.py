#!/usr/bin/python3
# import
import os, sys
import pandas as pd
from pandas import DataFrame as DF, Series




# input
## it's sometimes a symbol, so need to read realpath first
script_seq = os.path.dirname(os.path.realpath(sys.argv[0]))
in_seq = sys.argv[1]




# check
e_list = ["A", "T", "C", "G"]
if not any([True if e in in_seq else False for e in e_list]):
    raise Exception("Not neucletide.")


    
    
# get codon df with different frame shift
seq_list = [e for e in in_seq]
res_dict = {}

## frame shift
for start in range(3):
    sub_list = seq_list[start:]
    o = 0

    ### get codon
    for e in sub_list:
        res = sub_list[o : o + 3]
        res = "".join(res)
        if res != "":
            res_dict.setdefault("codon_{}".format(start), []).append(res)
        o += 3
        
## match the length for different frame
max_length=max([len(e) for e in res_dict.values()])
for k,v in res_dict.items():
    if len(v)<max_length:
        v.extend([None for _ in range(max_length -len(v))])
        res_dict[k]=v

df_in = DF(res_dict)


# get reference
dfr = pd.read_csv(os.path.join(script_seq, "codon_amino.tsv"), sep="\t")




# output
out_d = "DNA2Pro_output"
#os.makedirs(out_d, exist_ok=True)

for num in range(3):
    col_name = "codon_{}".format(num)
    df_sub = df_in[col_name]
    df_res = DF(df_sub).merge(dfr, how="left", left_on=col_name, right_on="codon")
    df_res = df_res.drop(labels=["codon"],axis=1)
    #df_res.to_csv("{}/{}.csv".format(out_d, col_name), sep="\t", index=False)
    if num == 0:
        print(df_res)