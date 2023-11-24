## ----------------------------------------------------- mulitple prints for a cell -----------------------------------------------------
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'

## ----------------------------------------------------- parallel -----------------------------------------------------
import asyncio

def background(f):
    def wrapped(*args, **kwargs):
        return asyncio.get_event_loop().run_in_executor(None, f, *args, **kwargs)

    return wrapped

## ----------------------------------------------------- progress bar -----------------------------------------------------
from tqdm import tqdm, trange

## ----------------------------------------------------- mine -----------------------------------------------------
def pretty_print(t):
    print(t + "\n")
    
## ----------------------------------------------------- system -----------------------------------------------------
import os,sys,re,subprocess,shutil,pathlib,glob
import traceback, warnings
import itertools, more_itertools

def list_files(path,pattern,full_names=False,recursive=True):
    if(recursive):
        files=pathlib.Path(path).rglob(pattern)
    else:
        files=pathlib.Path(path).glob(pattern)

    if full_names:
        files=[str(f) for f in files]
    else:
        files=[f.name for f in files]
    return(files)

##numpy
import numpy as np
from numpy import random as random

## ----------------------------------------------------- normal pandas -----------------------------------------------------
import pandas as pd
from pandas import DataFrame as DF, Series

def write_df_wrap(df,df_name,df_dir=".",df_fmt="tsv",rowname="rowname",col_names=True,prompt=True):
    ## create folder
    os.makedirs(df_dir,exist_ok=True)
    ## filepath operation
    file_name=f"{df_name}.{df_fmt}"
    fig_path=os.path.join(df_dir,file_name)
    ## add rowname
    df.index.name=rowname
    ## format
    if(df_fmt=="tsv"):
        df.to_csv(fig_path,sep="\t",header=col_names)
    elif(df_fmt=="csv"):
        df.to_csv(fig_path,header=col_names)
    elif(df_fmt=="xlsx"):
        df.to_excel(fig_path,header=col_names)
    else:
        raise Exception("Only support tsv, csv and xlsx.")
    ## prompt
    if(prompt):
        tt=f"successfully save data.frame to {fig_path}"
        pretty_print(tt)

## ----------------------------------------------------- plot -----------------------------------------------------
from plotnine import *

def ggsave_wrap(fig,fig_name,fig_dir=".",fig_fmt="png",size=[5,5],prompt=True):
    ## build dir and fig_path
    os.makedirs(fig_dir,exist_ok=True)
    file_name=f"{fig_name}.{fig_fmt}"
    fig_path=os.path.join(fig_dir,file_name)
    fig.save(filename=fig_path,width=size[0],height=size[1])
    if(prompt):
        tt=f"successfully save figure to {fig_path}."
        pretty_print(tt)

























## deep learning
import torch
import torch.nn as nn
import torch.optim as optim
import torch.nn.functional as F
from torch.utils.data import Dataset, DataLoader

from transformers import BertPreTrainedModel, BertModel
from transformers import AutoConfig, AutoTokenizer

from sklearn import metrics
from sklearn.model_selection import train_test_split
from tqdm import tqdm, trange

torch.cuda.is_available()