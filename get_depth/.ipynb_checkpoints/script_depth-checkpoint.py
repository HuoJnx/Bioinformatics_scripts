import os, sys, datetime

import pandas as pd
from pandas import DataFrame as DF, Series

#get files
in_dir=sys.argv[1]
f_list=[f for f in os.listdir(in_dir) if "depth" in f]
f_list=[os.path.join(in_dir,f) for f in f_list]


#get the summary dataframe for files
for o,f in enumerate(f_list):
    temp_df=pd.read_csv(f,sep="\t",header=None)
    temp_df.columns=["chrom","pos","depth"]
    temp_df["file"]=f
    if o==0:
        final_df=temp_df
    else:
        final_df=pd.concat([final_df,temp_df])

## strip the postfix of file
final_df["file"]=final_df["file"].str.lstrip(in_dir)
final_df["file"]=final_df["file"].map(lambda x:x.split(".")[0])

        
#get the average depth for each file
depth_mean=final_df.groupby("file").depth.mean()
depth_mean=DF(depth_mean).reset_index()

#print result
now_time = datetime.datetime.now().strftime('%Y_%m_%d_%H_%M_%S')

pwd=os.getcwd()
file_name="depth_{}.csv".format(now_time)
save_path=os.path.join(pwd,file_name)

depth_mean.to_csv(save_path,index=False,header=None)