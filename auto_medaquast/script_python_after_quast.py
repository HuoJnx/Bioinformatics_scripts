##system
import sys, os, argparse
import pandas as pd

## statistics
import tableone
from tableone import TableOne
import pingouin as pg

## plot
import seaborn as sns
import matplotlib
from matplotlib import pyplot as plt



# warnings
import warnings
warnings.filterwarnings("ignore")


# user function
def printt(e):
    print("\n{}\n".format(e))



#parse argument

parser = argparse.ArgumentParser(description='Basically, this script is run by the auto_metaquast.sh, but you can call it manually.')
parser.add_argument("-i", type=str,help="Input directory.")
parser.add_argument("-d", type=str,help="Depth file. Not necessary.")
parser.add_argument("-s", type=str,help="Standard genome. Not necessary.")
parser.add_argument("-m", type=str,help="Metadata file, includs groupby information. Necessary, or the script will stop.")
parser.add_argument("-o", type=str,help="Output directory name. Then the path of the output directory will be that name, and inside the output directory of metaquast.")

args = parser.parse_args()

out_quast=args.i
out_python=args.o
meta_data=args.m
depth_file=args.d
golden_std=args.s









    
    
    
    
# get meta
try:
    df_meta=pd.read_table(meta_data,header=None)
    df_meta.columns=["file_name","group"]
    df_meta["file_name"]=df_meta["file_name"].map(lambda x:x.split(".")[0])
    df_meta["group"]=df_meta["group"].astype(str)
    ##指定 groupby, 之后要用
    groupby="group"
    printt("Successfully get the metadata file.")
except:
    printt("Can't read the metadata file, python process stop.")
    sys.exit(1)
    
    
    
    
# get golden standard genome mark
# 在这里，这个大写的GOLD_STD在排序的时候会始终保持在最前面
df_meta.loc[df_meta["file_name"]==golden_std,"group"]="GOLD_STD"

#### 检查这个 golden standard 的名字是不是有问题
num_std=len(df_meta.loc[df_meta["file_name"]==golden_std,"group"])
if ((num_std==0)&(golden_std!="None")):
    printt("Although golden standard genome is provided, it's invalid. Please double check if the provided name ‘{}’ is same as in quast.".format(golden_std))
elif num_std!=0:
    printt("Successfully found the standard genome")
    
        
            


    
    
    
    
    
# get depth information
try:
    df_depth=pd.read_table(depth_file,header=None)
    df_depth.columns=["strain","depth"]
    df_depth=df_depth.sort_values("depth",ascending=False)
    df_depth["depth_str"]=df_depth.depth.map(lambda x:"{:.2f}".format(x))
    df_depth["name"]=df_depth["strain"]+" ("+df_depth["depth_str"]+")"
    df_depth=df_depth.reindex(columns=["strain","name"])
    df_depth.columns=["Assemblies","name"]
    printt("Successfully found the depth file.")
except:
    printt("Can't read the depth file. But still can process.")
    
    
    
    
    
    
    
    
# get data
tsv_dir="{}/summary/TSV/".format(out_quast)
m_list = [e.split(".tsv")[0] for e in os.listdir(tsv_dir)]
for o, e in enumerate(m_list):
    ##read csv
    metric = e
    df = pd.read_csv(os.path.join(tsv_dir,metric)+".tsv", sep="\t")
    
    ##align name, if df_depth existed, then align, if not existed, just use the Assemblies as name
    try:
        df = df_depth.merge(df, how="outer")
        df.drop(columns=["Assemblies"], inplace=True)
        ##make index col to index
        df.set_index("name", inplace=True)
    except:
        df.rename(columns={"Assemblies":"name"},inplace=True)
        df.set_index("name", inplace=True)
    
    ##force to float
    df = df.apply(pd.to_numeric, errors="coerce")
    
    ## the NGA50 and genome fraction is special
    if e == "NGA50":
        df_nga50 = df
    if e == "Genome_fraction":
        df_gf = df
    ##get melt_matrix
    
    df_melt = df.reset_index()
    df_melt = df_melt.melt(id_vars=["name"], var_name="file_name", value_name=metric)
    df_melt

    ##get merge
    if o == 0:
        df_final = df_melt
    else:
        id_col = ["name", "file_name"]
        df_final = df_final.merge(df_melt, left_on=id_col, right_on=id_col)
printt("Successfully get the summary of the quast's results")
        
        
        
        
        
        
# merge with metadata
df_stat = df_final.merge(df_meta)






# some metrics should be divided by 10000
divide_1000=["Total_length_(ge_1000_bp)","Total_length_(ge_10000_bp)","Total_length_(ge_50000_bp)",
             "Largest_contig","Total_length","Largest_alignment","Total_aligned_length","NGA50","Misassembled_contigs_length"]
for col in divide_1000:
    #name
    o_name=col
    n_name="{}_(Kbp)".format(o_name)
    #calculation
    df_stat[n_name]=df_stat[o_name]/1000
    
df_stat=df_stat.drop(columns=divide_1000)







# define columns for statistics
stat_col4_tableone=["num_contigs","Total_length_(ge_1000_bp)_(Kbp)","Total_length_(ge_10000_bp)_(Kbp)","Total_length_(ge_50000_bp)_(Kbp)",
                   "Largest_contig_(Kbp)","Total_length_(Kbp)","Genome_fraction","NGA50_(Kbp)","Largest_alignment_(Kbp)","Total_aligned_length_(Kbp)",
                   "num_misassemblies","Misassembled_contigs_length_(Kbp)","num_mismatches_per_100_kbp","num_indels_per_100_kbp","Duplication_ratio"]
stat_col4_fiure=stat_col4_tableone
stat_col4_fiure.append("num_Ns_per_100_kbp")






# output tableone
print("\nTableone processing.\n")
mytable = TableOne(
    df_stat,
    columns=stat_col4_tableone,
    groupby=[groupby],
    overall=False,
    pval=True,
    decimals=2,
)
mytable.tabulate(tablefmt="html")
os.makedirs(out_python,exist_ok=True)
out_file="{}/out_tableone.csv".format(out_python)
mytable.to_csv(out_file)








# output pair-wise p-adjust (FDR_BH)
printt("Pair-wise t-test processing.")
out_pair_wise=os.path.join(out_python,"pair_wise")
os.makedirs(out_pair_wise,exist_ok=True)
for col in stat_col4_tableone:
    df_pair=pg.pairwise_ttests(data=df_stat,between="group",dv="Misassembled_contigs_length_(Kbp)",
                                   padjust="fdr_bh")
    df_pair=df_pair.sort_values("p-unc")
    df_pair.index.name="Original_order"
    df_pair.to_csv("{}/{}.csv".format(out_pair_wise,col))










# output barplot
x="file_name"

##这里，是想用 group 来进行排序，主要目的是让同一个 group 的 x 都凑在一起，不要分散开
order=df_stat[[x,groupby]].sort_values(groupby)[x].value_counts(sort=False).index.to_list()

## plot
printt("Barplots plotting.")
for o,c in enumerate(stat_col4_fiure):
    fig, ax = plt.subplots(1, 1, figsize=(6, 4))
    sns.boxplot(x="file_name",y=c, data=df_stat, hue=groupby, order=order, ax=ax, width=0.5, dodge=False)
    ax.set_xticklabels(ax.get_xticklabels(),fontdict={"size": "15","rotation":"90"})
    ax.set_yticklabels(ax.get_yticks(),fontdict={"size": "15"})
    #ax.set_xlabel("Pipeline", fontdict={"size": "25"})
    ax.set_ylabel(ax.get_ylabel(), fontdict={"size": "25"})
    if c=="num_contigs":
        ax.legend(bbox_to_anchor=(1, 1))
    else:
        ax.legend([],frameon=False)
    fig.savefig(
        "{}/{}.png".format(out_python,c),
        dpi=600,
        bbox_inches="tight",
        transparent=False,
    )
    plt.close()
    


    
    
    
    
    
    
# heatmap
printt("Heatmap plotting.")
## nga50
df_nga50_Kbp=df_nga50/1000
fig,ax=plt.subplots(1,1,figsize=(12,8))
sns.heatmap(df_nga50_Kbp,cmap="coolwarm",annot=True)
ax.set_xticklabels(ax.get_xticklabels(), fontdict={"size": "15"})
ax.set_ylabel("NGA50 (Kbp)",fontdict={"size":"25"})
fig.savefig(
        "{}/{}.png".format(out_python,"heatmap_NGA50"),
        dpi=600,
        bbox_inches="tight",
        transparent=False,
    )
plt.close()


## genome fraction
fig,ax=plt.subplots(1,1,figsize=(12,8))
sns.heatmap(df_gf,cmap="coolwarm",annot=True)
ax.set_xticklabels(ax.get_xticklabels(), fontdict={"size": "15"})
ax.set_ylabel("Genome fraction (%)",fontdict={"size":"25"})
fig.savefig(
        "{}/{}.png".format(out_python,"heatmap_Genome_fraction"),
        dpi=600,
        bbox_inches="tight",
        transparent=False,
    )
plt.close()