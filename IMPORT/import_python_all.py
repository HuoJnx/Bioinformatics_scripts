#Commonly used
## mulitple prints for a cell
from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = 'all'

## progress bar
from tqdm import tqdm, trange

## mine
def pretty_print(t):
    print(t + "\n")
    
## system
import os,sys,re,subprocess,shutil
import traceback, warnings
import itertools, more_itertools

##numpy
import numpy as np
from numpy import random as random

##normal pandas
import pandas as pd
from pandas import DataFrame as DF, Series

##plot
from plotnine import *






## bash
from subprocess import PIPE, Popen


def bash(cmd, prompt=False):
    p = Popen(cmd, shell=True, stdout=PIPE, stdin=PIPE, stderr=PIPE)
    if prompt:
        out, err = p.communicate()
        return (out, err)
    else:
        return ()
    
def bash_p(cmd,get_err=False,prompt=False,list_out=False):
    #get results
    out,err=bash(cmd,prompt=True)
    out=out.decode()
    err=err.decode()
    
    #print
    if len(err)!=0:
        print("ERROR:\n{}\n".format(err),"-"*50,sep="\n")
    else:
        if prompt==True:
            print("OUT:\n{}\n".format(out),"--"*50)
    
    # output result
    if get_err==True:
        return(out,err)
    if list_out==True:
        out_list=out.split("\n")
        if out_list[-1]=="":
            out_list=out_list[:-1]
        return(out_list)
    else:
        return(out)




##advanced pandas
import modin.pandas as mpd
from modin.pandas import DataFrame as mDF, Series as mSeries











#Chinese characters
from xpinyin import Pinyin
from zhon import hanzi







#Machine learning
import scipy
from scipy.spatial.distance import pdist
from scipy.cluster.hierarchy import dendrogram,linkage
from scipy.cluster.hierarchy import fcluster




#sklearn basis (before train)
import sklearn
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import train_test_split # Separate the data for train and test




# imbalanced data
from imblearn.over_sampling import SMOTE




#Estimator
from sklearn.metrics import confusion_matrix
from sklearn.metrics import r2_score
from sklearn.metrics import roc_curve, auc # 计算roc和auc
from sklearn.metrics import precision_recall_curve
from sklearn.model_selection import cross_val_score #交叉验证

#Clustering
from sklearn.cluster import KMeans


#Dimension reduction
from sklearn.decomposition import PCA





#Classification
from sklearn.naive_bayes import GaussianNB#Gaussian naive bayes
from sklearn import svm
from sklearn.linear_model import LogisticRegression as LR




#Regression
from sklearn.linear_model import LinearRegression#Linear regression
from sklearn.preprocessing import PolynomialFeatures#Polynomial approximation





#Statistics

from scipy import stats #Traditional statistics without
import scikit_posthocs #Post hoc tests and adjusted p




# save model
import joblib





#statsmodel calling
import statsmodels.api as sm
import statsmodels.formula.api as smf











