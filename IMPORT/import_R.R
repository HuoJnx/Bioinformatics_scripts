## ---------------------------------- options ----------------------------------
options(repr.matrix.max.rows=20)
size_obj=c(8,8)
options(repr.plot.width=size_obj[1], repr.plot.height=size_obj[2])

set_fig_size=function(width,height,size_obj=NULL){
    if(!is.null(size_obj)){
        options(repr.plot.width=size_obj[1], repr.plot.height=size_obj[2])
    }else{
        options(repr.plot.width=width, repr.plot.height=height)
    }

}

pretty_print=function(tt){
    cat(tt,"\n\n")
}

print_rich=function(obj){
    IRdisplay::display(obj)
}
print_all_row=function(df){
    original_show_rows=getOption("repr.matrix.max.rows")
    options(repr.matrix.max.rows = Inf)
    print_rich(df)
    options(repr.matrix.max.rows = original_show_rows)
}

v=function(text){
    return(packageVersion(text))
}
## ---------------------------------- file operation -----------------------------
basename_without_suffix=function(x,last=T){
    x=basename(x)
    if(last){#will only remove the last ".+$"
        x=str_remove(x,regex("\\.[^.]+$"))##match a . then [^.] match not period, 
        
    }else{
        x=str_remove(x,regex("\\..+$"))
    }
    return(x)
}

get_var_name = function(var) {
  deparse(substitute(var))
}

cat_file_path = function(file_name,file_dir,file_fmt){
    # Construct the file name and sanitize it
    file_name_with_prefix = paste0(file_name, ".", file_fmt)
    file_name_with_prefix = fs::path_sanitize(file_name_with_prefix)
  
    # Construct the full file path
    file_path = file.path(file_dir, file_name_with_prefix)

    # return
    return(file_path)
}

remove_folder = function(folder_path, force = F) {
  # Check if the folder exists
  if (!dir.exists(folder_path)) {
    message("The specified folder does not exist.")
    return(FALSE)
  }
  
  # Check if the folder is empty or force_delete is TRUE
  if (length(list.files(folder_path)) == 0 || force) {
    unlink(folder_path, recursive = TRUE)
    message("Folder has been deleted.")
    return(TRUE)
  } else {
    message("Folder is not empty. Use force = TRUE to delete anyway.")
    return(FALSE)
  }
}
## ---------------------------------- parallel ----------------------------------
library_parallel=function(n_cl=NULL){
    library(doParallel)
    library(foreach)
    if(is.null(n_cl)){
        doParallel::registerDoParallel()
    }else{
        doParallel::registerDoParallel(cores=n_cl)
    }
    
}

## ---------------------------------- write thing ----------------------------------
write_df_wrap=function(df,file_name,save_dir=".",file_fmt="tsv",rowname="rowname",col_names=T,prompt=T){
    ## convert
    if(all(class(df)!=class(data.frame()))){
        df=data.frame(df,check.names = F)
    }
    ## create folder
    dir.create(save_dir,showWarnings=F)
    ## filepath operation
    file_name=paste0(file_name,".",file_fmt)
    file_name = fs::path_sanitize(file_name)
    fig_path=file.path(save_dir,file_name)
    ## rownames
    if(has_rownames(df)){
        df=df%>%rownames_to_column(rowname)
        tt=sprintf("Rowname detected, coverted it to a column call '%s.'",rowname)
        pretty_print(tt)
    }
    ## format
    if(file_fmt=="tsv"){
        write_tsv(df,fig_path,col_names=col_names)
    }else if(file_fmt=="csv"){
        write_csv(df,fig_path,col_names=col_names)
    }else if(file_fmt=="xlsx"){
        library(writexl)
        write_xlsx(df,fig_path,col_names=col_names,format_headers=F)
    }else{
        stop("Only support tsv, csv and xlsx.")
    }
    ## prompt
    if(prompt){
        tt=sprintf("successfully save data.frame to %s",fig_path)
        pretty_print(tt)
    }
}

write_plain_text=function(text,text_name,text_dir=".",text_fmt="txt",prompt=T){
    ## create folder
    dir.create(text_dir,showWarnings=F)
    ## filepath operation
    file_name=paste0(text_name,".",text_fmt)
    file_name = fs::path_sanitize(file_name)
    fig_path=file.path(text_dir,file_name)
    ## save
    sink(fig_path)

    print(text)
    
    sink()
    ## prompt
    if(prompt){
        tt=sprintf("successfully save data.frame to %s",fig_path)
        pretty_print(tt)
    }
}
write_RDS_plus = function(object, rds_name, rds_dir = ".", rds_fmt = "rd", prompt = T) {
    # Create the directory if it doesn't exist
    dir.create(rds_dir, showWarnings = F)
  
    # Construct the file name and sanitize it
    file_name = paste0(rds_name, ".", rds_fmt)
    file_name = fs::path_sanitize(file_name)
  
    # Construct the full file path
    rds_path = file.path(rds_dir, file_name)
  
    # Save the object as an RDS file
    saveRDS(object, rds_path)
  
  # Print a message to the console if prompt is TRUE
    if (prompt) {
        tt = sprintf("Successfully saved object to %s", rds_path)
        pretty_print(tt)  # Assuming you have a function called pretty_print to format the text
  }
}

write_RDS_if_non_existed_else_read = function(rds_name, rds_dir = ".", rds_fmt = "rd", command_func) {
    ## create folder
    dir.create(rds_dir, showWarnings = F)

    file_name = paste0(rds_name, ".", rds_fmt)
    file_name = fs::path_sanitize(file_name)
    rds_path = file.path(rds_dir, file_name)

    if (!file.exists(rds_path)) {
        object = command_func()
        saveRDS(object, rds_path)
    } else {
        object = readRDS(rds_path)
    }
    return(object)
}

## ---------------------------------- vector operation ----------------------------------
vector_remove = function(vec, elements_to_remove) {
  return(vec[!vec %in% elements_to_remove])
}
str_filter = function(str_vec,pattern,filter_out=T) {
    if(filter_out){
        str_vec=str_vec[!str_vec%>%str_detect(pattern)]
    }else{
        str_vec=str_vec[str_vec%>%str_detect(pattern)]
    }
    return(str_vec)
}

## ---------------------------------- infinity ----------------------------------
log2_no_infinity = function(x,c){
    return(log2(x+c))
}

is_infinite_plus = function(x) {
    # Check if the input is a data.frame
    if (is.data.frame(x)) {
    # If it's a data.frame, apply is.infinite to each element
        inf_matrix = x %>% mutate_all(is.infinite) %>% as.matrix
        return(inf_matrix)
    } else if (is.vector(x)) {
    # If it's a vector, just apply is.infinite directly
        inf_vector = is.infinite(x)
        return(inf_vector)
    } else {
    # If it's neither a data.frame nor a vector, produce an error
        stop("Input must be a data.frame or a vector")
    }
}
## ---------------------------------- tidyverse ----------------------------------
Sys.setenv(TZ = "Asia/Hong_Kong")# without this line, tidyverse will stuck for a long time
suppressPackageStartupMessages(library(tidyverse))
library(openxlsx)
read_xlsx = read.xlsx

as.data.frame_plus=function(df,rownames_to_column=F,var="rowname"){
    df=df%>%data.frame(check.names=F)
    if(rownames_to_column){
        df=df%>%rownames_to_column(var)
    }
    return(df)
}
fix_order = function(array,rev=F) {
    if (is.factor(array)) {
        return(array)
    } else {
        hope_lvl = if (rev) rev(unique(array)) else unique(array)
        array = factor(array, levels = hope_lvl)
        return(array)
    }
}


shuffle_df=function(df){
    ndf=df[sample(1:nrow(df)),]
    if(!has_rownames(df)){
        rownames(ndf)=NULL
    }
    return(ndf)
}

str_split_str_merge_dataframe=function(array,sep,keep_range){
    m_array=str_split(array,sep,simplify = T)[,keep_range]
    df_array=m_array%>%data.frame
    vec_string=df_array%>%unite(keep_range,sep = sep)%>%pull
    return(vec_string)
}

fa_add_colname=function(df,detect_factor=T){
    if(detect_factor){
        mutate_col=df%>%select_if(is.factor)%>%colnames
    }else{
        mutate_col=df%>%colnames
    }
    df=df%>%mutate_if(is.factor,as.character)
    for(c in colnames(df)){
        df[[c]] = paste(c, df[[c]], sep = "_")
    }
    return(df)
}

match_row=function(df_to_modify,reference,NA_fill=NA,first_col_is_rowname=F){
    ## if havn't rownames, make it
    if(first_col_is_rowname){
        first_col_need=(df_to_modify%>%colnames)[1]
        df_to_modify=df_to_modify%>%column_to_rownames(first_col_need)
        first_col_ref=(reference%>%colnames)[1]
        reference=reference%>%column_to_rownames(first_col_ref)
    }
    ## rownames function always return character
    if(is.data.frame(reference)){
        ref_row=reference%>%rownames
        print("Receive data.frame as reference.")
    }else if(is.vector(reference)){
        ref_row=reference
        print("Receive vecotr as reference.")
    }else {
        stop("Only data.frame or vecotr are supported.")
    }
    change_row=df_to_modify%>%rownames
    ## add the row non-existed in target
    need_add=setdiff(ref_row,change_row)
    df_to_modify[need_add,]=NA_fill
    ## remove the row non-existed in source
    df_to_modify=df_to_modify[ref_row,,drop=FALSE]
    
    ## return to original
    if(first_col_is_rowname){
        df_to_modify=df_to_modify%>%rownames_to_column(first_col_need)
    }
    
    return(df_to_modify)
}
match_col=function(df_to_modify,reference,NA_fill=NA){

    if(is.data.frame(reference)){
        print("Receive data.frame as reference.")
        ref_col=reference%>%colnames
    }else if(is.vector(reference)){
        ref_col=reference
        print("Receive vecotr as reference.")
    }else {
        stop("Only data.frame or vecotr are supported.")
    }
    change_col=df_to_modify%>%colnames
    ## add the row non-existed in target
    need_add=setdiff(ref_col,change_col)
    df_to_modify[,need_add]=NA_fill
    ## remove the row non-existed in source
    df_to_modify=df_to_modify[,ref_col]
    return(df_to_modify)
}
map_name = function(.data, name_vector, old_name_first = T) {
    
    if (class(name_vector) == "list") {
        name_vector = unlist(name_vector)
    }
    
    if (old_name_first) {
        x = names(name_vector)
        y = as.character(unname(name_vector))
    } else {
        y = names(name_vector)
        x = as.character(unname(name_vector))
    }
    
    func = function(e) {
        loc = which(e == x)
        res = y[loc]
        if (length(res) == 0) {
            res = e
        }
        return(res)
    }
    .data = .data %>% map_chr(func)
    return(.data)
}

#name_vector=c("NO NAFLD"=0,"NAFL"=1,"NASH"=2,"NASH&Fibrosis"=3)
#df$y_Diagnosis=map_name(df$y_Diagnosis,name_vector)
map_name_df=function(df_original,colname_need_to_change,df_rename,colname_old_name,colname_new_name,replace=T){

    ## check data type
    df_original[[colname_need_to_change]]=as.character(df_original[[colname_need_to_change]])
    df_rename=df_rename%>%mutate_all(as.character)
    
    ## get new name

    if(!replace){
        new_or_replace_col=colname_new_name
        print(sprintf("Create new col call %s.",colname_new_name))
    }else{
        new_or_replace_col=colname_need_to_change
        print("Replace the old col.")
    }
    df_original[[new_or_replace_col]]=factor(df_original[[colname_need_to_change]], levels = df_rename[[colname_old_name]],labels = df_rename[[colname_new_name]])
    df_original[[new_or_replace_col]]=as.character(df_original[[new_or_replace_col]])
    return(df_original)

}
#df_test=map_name_df(df_class,"Class",df_rename,"Original_class","Adjusted_class")

pull_all=function(df,unique=T){
    df=df%>%as.data.frame
    vec=df%>%unlist
    if(unique){
        vec=vec%>%unique
    }
    return(vec)
}
pull_with_name = function(.data, col_name) {
    # make to symbol
    col_name=ensym(col_name)
    
    # Extract the column as a vector using pull()
    vec = .data %>% pull(!!col_name)

    # Assign row names to the vector's names
    names(vec) = rownames(.data)

    return(vec)
}
auto_mapper=function(data,fix_order_vector=NULL,sort=T,prompt=F,type="df"){
    ## if vector, change to data.frame
    if(type=="vector"){
        df=data.frame(data=data)
    }else if(type=="df"){
        df=data
    }
    ## get all the unique value in df
    all_unique=df%>%pull_all
    if(sort){
        df_unique=data.frame(unique=all_unique,str_length=str_count(all_unique))%>%arrange(str_length)
        all_unique=df_unique%>%pull(unique)
    }
    ## reorder if provide fix_order_vector
    if(!is.null(fix_order_vector)){
        first=fix_order_vector
        then=setdiff(all_unique,fix_order_vector)
        all_unique=c(first,then)
    }
    ## build names and values for name_vector
    name_vector_value=seq(from=0,length.out=length(all_unique))
    name_vector=name_vector_value
    names(name_vector)=all_unique
    if(prompt){
        print(data.frame(name_vector))
    }
    

    ## rename the whole data.frame
    df=df%>%mutate_all(~map_name(.x,name_vector))
    if(type=="df"){
        return(df)
    }else if(type=="vector"){
        return(df%>%pull)
    }
    
}
#df_ngs_wider1=df_ngs_wider1%>%auto_mapper(fix_order_vector=c("NA_mark","sub","dup","del","ins","indel"))
auto_mapper_digi=function(df,NA_mark="NA_mark"){
    ## get all the unique value in df
    all_unique=df%>%pull_all
    num_unique=all_unique%>%as.numeric%>%na.omit%>%as.character
    char_unique=setdiff(all_unique,num_unique)
    
    ## move the NA_mark to the top
    char_unique=c(NA_mark,char_unique)%>%unique
    
    ## calculate the max of the num_unique
    num_min=num_unique%>%as.numeric%>%min
    num_max=num_unique%>%as.numeric%>%max
    if(num_max<0){
        start_ind=0
    }else{
        start_ind=num_max%>%as.integer+1
    }
    
    ## build names and values for name_vector
    name_vector_value=seq(from=start_ind,length.out=length(char_unique))
    name_vector=name_vector_value
    names(name_vector)=char_unique
    print(data.frame(name_vector))
    
    ## rename the whole data.frame
    df=df%>%mutate_all(~map_name(.x,name_vector))
    return(df)
}
#df_ngs_wider2=df_ngs_wider2%>%auto_mapper_digi(NA_mark="NA_mark")


t_df=function(df,include_rownames=T){
    if(all(class(df)!=class(data.frame()))){
        df=data.frame(df,check.names = F)
    }
    if(include_rownames){
        ## 
        need_rowname_repair=!(df%>%has_rownames)
        if(need_rowname_repair){
            var_name=colnames(df)[1]
            df=df%>%column_to_rownames(var=var_name)%>%t%>%data.frame(check.names = F)
        }else{
            df=df%>%t%>%data.frame(check.names = F)
        }
    }else{
        df=df%>%t%>%data.frame(check.names = F)
    }
    return(df)
}



rowMedians=function(df){
    if(!all(class(df)==class(matrix()))){
        m=as.matrix(df)
        
    }else{
        m=df
    }
    median_m=matrixStats::rowMedians(m,useNames=T)
    return(median_m)
}

pivot_longer_wrapper=function(df,id_col_index,id_col_name=NULL,names_to=NULL,values_to=NULL){
    ## if id_col_index==0, means the rowname should be export and index should +1
    if(id_col_index==0){
        df=df%>%rownames_to_column(id_col_name)
        id_col_index=id_col_index+1
    }
    need_col=(df%>%colnames)[-id_col_index]
    df=df%>%pivot_longer(cols=all_of(need_col),names_to=names_to,values_to=values_to)
    return(df)
}

pivot_longer_more_than_one_ID_wrapper=function(df,id_col_index,names_to=NULL,values_to=NULL){
    all_colnames=df %>% colnames
    need_col = setdiff(all_colnames,id_col_index)
    df = df %>% pivot_longer(cols = all_of(need_col), names_to = names_to, 
            values_to = values_to)
    return(df)
}

heatmap_sig_generator=function(df_sig){
    cols=colnames(df_sig)
    ind_cols=seq_along(cols)
    for(e in ind_cols){
        df_sig[,e]=ifelse(df_sig[,e]==T,e,NA)
    }
    return(df_sig)
}

drop_na_thresh=function(df,axis=0,thresh=0.3,prompt=T){
    if(axis==0){
        NA_percent=rowMeans(is.na(df))
        drop_ind=which(NA_percent>thresh)
        drop_name=rownames(df)[drop_ind]
        drop_percent=NA_percent[drop_ind]
        df=df[-drop_ind,]
    }else if(axis==1){
        NA_percent=colMeans(is.na(df))
        drop_ind=which(NA_percent>thresh)
        drop_name=colnames(df)[drop_ind]
        drop_percent=NA_percent[drop_ind]
        df=df[,-drop_ind]
    }
    if(prompt){
        print(NA_percent)
        print(sprintf("Drop %s, %s",drop_name, drop_percent))
    }
    return(df)
}

drop_na_rows = function(df) {
  # Identify rows where all values are NA
  na_rows = apply(df, 1, function(x) all(is.na(x)))
  
  # Remove rows where all values are NA
  df_no_na = df[!na_rows, ]
  
  return(df_no_na)
}

#### some operation like divide a data.frame by a vector in colwise behavior
apply_vector_2_dataframe_row_by_row=function(df,vec,operator="/",strict=T){
    if(strict){
        if(nrow(df)!=length(vec)){
            stop("Please ensure the nrow of dataframe is equal to length of vector.")
        }
    }
    exp=paste("df",operator,"vec")
    new_df=eval(rlang::parse_expr(exp))
    return(new_df)
}
apply_vector_2_dataframe_col_by_col=function(df,vec,operator="/",strict=T){
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

calculate_fold_change = function(df, split_col, arrange_col, numerator_group, denominator_group, mode = "FC") {
    # Make some character into name
    split_col_sym = as.name(split_col)
    arrange_col_sym = as.name(arrange_col)

    # Split the data frame into a list of data frames based on the 'split_col'
    list_df = split(df, f = df[[split_col]])

    # Extract the numerator and denominator data frames
    numerator_df = list_df[[numerator_group]]
    denominator_df = list_df[[denominator_group]]

    # Ensure the samples are in the same order in both data frames
    numerator_df = numerator_df %>% arrange(!!arrange_col_sym)
    denominator_df = denominator_df %>% arrange(!!arrange_col_sym)

    # Identify columns for fold change calculation (excluding the split and arrange columns)
    value_cols = setdiff(colnames(df), c(split_col, arrange_col))

    # Calculate fold change
    fold_change_df = denominator_df %>% select(-!!split_col_sym)

    if (mode == "FC") {
        fold_change_df[value_cols] = numerator_df[value_cols] / denominator_df[value_cols]
    } else if (mode == "log2FC") {
        fold_change_df[value_cols] = log2(numerator_df[value_cols] / denominator_df[value_cols])
    } else {
        stop("Invalid mode. Choose either 'FC' or 'log2FC'.")
    }

    # Add a new column to indicate that these are fold change values
    fold_change_df[[split_col]] = paste0(numerator_group, "_", denominator_group, "_", mode)
    fold_change_df = fold_change_df %>% select(!!split_col_sym,everything())


    return(fold_change_df)
}

keep_first = function(array,replace_value=NA) {
  replace(array, duplicated(array), replace_value)
}

keep_first_df = function(df,col_list,replace_value=NA,check_duplicated_by_all_cols=T){
    ## if check_duplicated_by_all_cols, it will use all the cols for checking duplicates
    ## for example, if T, then c("A","C") and c("B","C") won't be seen as duplicated, then c("A","C") and c("B","C") will keep original.
    if(check_duplicated_by_all_cols){
        df = df%>%group_by(across(all_of(col_list)))%>%
                    mutate_at(all_of(col_list),keep_first,replace_value)%>%
                    ungroup
    }else{
        df = df%>%mutate_at(all_of(col_list),keep_first,replace_value)
    }

    return(df)
}



join_index_helper = function(df1, df2, join_type, index_col = "index_default_12345") {
    index_col_sym = as.name(index_col)

    # Convert row names to a column in both data frames
    df1 = df1 %>% rownames_to_column(var = index_col)
    df2 = df2 %>% rownames_to_column(var = index_col)

    # Perform the join based on join_type
    if (join_type == "left") {
        merge_df = df1 %>% left_join(df2, by = index_col)
    } else if (join_type == "right") {
        merge_df = df1 %>% right_join(df2, by = index_col)
    } else if (join_type == "inner") {
        merge_df = df1 %>% inner_join(df2, by = index_col)
    } else if (join_type == "full") {
        merge_df = df1 %>% full_join(df2, by = index_col)
    }

    # Convert the "index" column back to row names
    rownames(merge_df) = merge_df[[index_col]]
    merge_df = merge_df %>% select(-!!index_col_sym)

    return(merge_df)
}

left_join_index = function(df1, df2, index_col = "index_default_12345") {
  join_index_helper(df1, df2, "left", index_col)
}

right_join_index = function(df1, df2, index_col = "index_default_12345") {
  join_index_helper(df1, df2, "right", index_col)
}

inner_join_index = function(df1, df2, index_col = "index_default_12345") {
  join_index_helper(df1, df2, "inner", index_col)
}

full_join_index = function(df1, df2, index_col = "index_default_12345") {
  join_index_helper(df1, df2, "full", index_col)
}

## ---------------------------------- text clean ------------------------------------
#library(stringr)
#library(pinyin)
#library(textclean)

#### can standardize the header
header4R=function(e){
    e=gsub(e,pattern='[^0-9a-zA-Z]+',replacement = "_")
    return(e)
}

#### rename all the col as "$prefix + number order" style.
replace_col=function(col,prefix="X"){
    len_col=1:length(col)
    new_col=map_chr(len_col,~sprintf("%s%s",prefix,.x))
    return(new_col)
}


#### replace a single element in a dataframe, the old_name can be index
rename_plus=function(df,old_name,new_name){
    if(class(df)!=class(data.frame)){
        df=data.frame(df,check.names=F)
    }
    if(is.numeric(old_name)){
        old_name=colnames(df)[old_name]
    }
    rename_vector=setNames(old_name,new_name)
    df=df%>%rename(rename_vector)
    return(df)
}

#### arrange the df by the order_vector. Notice, will only keep rows with value in $order_vector
arrange_by_order_vector=function(df,col4arrange,order_vector,return_factor=F){
    col4arrange_text = col4arrange
    col4arrange_symbol = col4arrange%>%as.name
    
    df = df %>% filter(!!col4arrange_symbol %in% order_vector)
    df = df %>% mutate(!!col4arrange_symbol := factor(!!col4arrange_symbol,levels=order_vector))
    df = df %>% arrange(!!col4arrange_symbol)
    if(!return_factor){
        df = df %>% mutate(!!col4arrange_symbol:=as.character(!!col4arrange_symbol))
    }
    return(df)
}

#### arrange row by a metrics like rowmean or rowmedian
arrange_by_row=function(df,metrics="mean",ascending=T,save_metrics_col=F){
    df=df%>%data.frame
    if(metrics=="mean"){
        df[["metrics"]]=rowMeans(df)
    }else{
        df[["metrics"]]=rowMedians(df)
    }
    if(ascending){
        df=df%>%arrange(metrics)
    }else{
        df=df%>%arrange(desc(metrics))
    }
    if(!save_metrics_col){
        df=df%>%select(-metrics)
    }
    return(df)
}
#### arrange row by clustering method
arrange_by_hclust=function(df,dist_method="euclidean",hclust_method="average"){
    dist_mat=stats::dist(df,method=dist_method)
    hclust_avg=stats::hclust(dist_mat,method=hclust_method)
    df2=df[hclust_avg$order,]
    row_order=df2%>%rownames
    return(list(ordered_df=df2,row_order=row_order))
}
## ---------------------------------- ggplot2 ----------------------------------

library(paletteer)

theme_set(theme_gray(base_size = 18))
#theme_set(theme_bw(base_size = 18))

ggsave_wrap=function(fig=NULL,file_name,save_dir=".",file_fmt="svg",size=c(5,5),prompt=T){
    ## if no fig input, use last plot
    if(is.null(fig)){
        fig=last_plot()
    }

    ## build dir and fig_path
    dir.create(save_dir,recursive=T,showWarnings=F)
    file_name=paste0(file_name,".",file_fmt)
    file_name = fs::path_sanitize(file_name)
    fig_path=file.path(save_dir,file_name)
    ggsave(fig_path,fig,width=size[1],height=size[2])
    if(prompt){
        tt=sprintf("successfully save figure to %s",fig_path)
        pretty_print(tt)
    }
}


get_hypothesis = function(data, group_col, value_col,
    general_m = "kruskal", pair_m = "wilcox", adj_m = "fdr", add_xy_function="max",detail_text=T) {
    library(rstatix)
    # ------------------------------------ check  ------------------------------------

    if(class(data)!=class(data.frame)){
        data=data%>%data.frame
    }
    # ------------------------------------ conduct hypothesis test ------------------------------------
    
    ## general test
    form = formula(paste(value_col, "~", group_col))
    if (general_m == "kruskal") {
        df_general_res = data %>% kruskal_test(form)
    }else if (general_m == "anova"){
        df_general_res = data %>% anova_test(form)
    }
    
    ## pair test
    if (pair_m == "wilcox") {
        pair_res = data %>% wilcox_test(form, p.adjust.method = "none")
    } else if (pair_m == 't.test') {
        pair_res = data %>% t_test(form,p.adjust.method = "none")
    }
    pair_res=pair_res%>%adjust_pvalue(method="fdr")%>%add_significance("p.adj")

    # ------------------------------------ get xy to plot ------------------------------------
    pair_res = pair_res %>%add_xy_position(x = group_col,fun=add_xy_function)%>%data.frame


    # ------------------------------------ return ------------------------------------
    general_text = get_test_label(df_general_res, detailed = detail_text)

    return(list(df_general_res=df_general_res,pair_res=pair_res,general_text=general_text))
}

close_all_device = function() {
  while (dev.cur() > 1) dev.off()
}


format_p=function(p,round=4,thresh=0.001){
    p_round=round(p,round)
    if(p_round<thresh){
        p_char=sprintf("P < %s",thresh)
    }else{
        p_char=sprintf("P = %s",p)
    }
    return(p_char)
}

plot_factor_proportion = function(df, col_you_want_as_x, col_you_want_as_legend,mode=c("stack","mode")) {
    # set mode
    if (length(mode)==2){
        mode=mode[1]
        print(sprintf("Use %s mode.",mode))
    }
    # Ensure the condition column is a factor
    df[[col_you_want_as_legend]] = as.factor(df[[col_you_want_as_legend]])
    
    
    # Calculate the proportion for each species
    group_by_vec=c(col_you_want_as_x, col_you_want_as_legend)
    data_summary = df %>% group_by(across(all_of(group_by_vec))) %>%
                    summarise(count = n()) %>%
                    mutate(proportion = count / sum(count))
    # Plot the bar plot
    plot = ggplot(data_summary, aes_string(x = col_you_want_as_x, y = "proportion", fill = col_you_want_as_legend)) +
            geom_bar(stat = "identity", position = mode)

    return(plot)
}

## ---------------------------------- rstatix -------------------------------------
add_xy_position_plus=function(subseted_rstatix_test_obj,x="taxa",dodge=0.8){
    library(rstatix)
    ## get y position
    rstatix_test_obj=subseted_rstatix_test_obj%>%add_y_position
    ## get data.frame
    df_rstatix_test_obj=rstatix_test_obj%>%as.data.frame
    # add xmin xmax
    dodge_each_side=dodge/4
    df_rstatix_test_obj=df_rstatix_test_obj%>%mutate(x=1:nrow(df_rstatix_test_obj))%>%
                                                mutate(xmin=x-dodge_each_side,xmax=x+dodge_each_side)
    return(df_rstatix_test_obj)
}


## ---------------------------------- corr ------------------------------------
#library(ggcorrplot2)#ggcorrplot(cor_matrix, method = "ellipse")
#library(GGally)#ggpairs(original_df)
#library(psych)#corr.test
#library(pheatmap)

# Define a function to select the upper triangle of a matrix, convert it to a data frame, and pivot to longer format
upper_tri_to_long = function(mat, value_name) {
    value_name_sym = value_name %>% as.name
    
    # Check if the matrix is itself to itself
    if (identical(rownames(mat), colnames(mat))) {
        print("Itself to itself correlation, will keep the upper triangle, but remove the diagonal.")
        mat = pull_upper_triangle(mat, diagonal = F) %>% as.data.frame_plus
    } else {
        print("Not itself to itself, will keep the whole matrix instead of pulling the upper triangle.")
        mat = mat %>% as.data.frame_plus %>%
                        rownames_to_column("rowname")
        # mat remains unchanged
    }

    # Convert to long format and rename columns
    df_long = mat %>% data.frame %>% 
                        pivot_longer_wrapper(id_col_index = 1, values_to = value_name, names_to = "to") %>%
                        filter(!!value_name_sym!="") %>%
                        rename("from"="rowname") %>%
                        mutate(!!value_name_sym:=as.numeric(!!value_name_sym))

    return(df_long)
}





# Define the parsing function
parse_corr_res = function(cor_res, corr_obj = "r", p_obj = "p", padj_method = "fdr", arrange_p = T, round = 4) {
  
    # Preprocess the 'r' and 'p' matrices
    r_long = upper_tri_to_long(cor_res[[corr_obj]], corr_obj)
    p_long = upper_tri_to_long(cor_res[[p_obj]], p_obj)
    
    # Merge the processed 'r' and 'p' data frames
    merged_df = full_join(r_long, p_long, by = c("from", "to"))

    # Adjust the p-values (The default p.adj object in cor_res is not corrected)
    p_adj_col_name = sprintf("p.adj (%s)",padj_method)
    merged_df[[p_adj_col_name]] = p.adjust(merged_df$p, method = padj_method)

    # Optionally arrange by p-value
    if (arrange_p) {
        merged_df = merged_df %>% arrange(p)
    }

    # Round the r, p, and p.adj values
    merged_df = merged_df %>% mutate_if(is.numeric, ~round(.x,digits = round-1))

    return(merged_df)
}


## ---------------------------------- linear ------------------------------------
#library(tidymodels)
#library(dotwhisker)
#library(vip)

## ---------------------------------- machine learning ------------------------------------
split_train_validation_test=function(x,p_train=0.7,p_validate=0.1,p_test=0.2){
    p_sum=round(p_train+p_validate+p_test,digits = 10)
    if(p_sum!=1){
        stop("The proportion should add to 1.")
    }
    
    spec = c(train = p_train, test = p_test, validate = p_validate)
    
    if(is.vector(x)){
        grouping_vector = sample(cut(seq(length(x)), length(x)*cumsum(c(0,spec)),labels = names(spec)))
        
    }else{
        grouping_vector = sample(cut(seq(nrow(x)), nrow(x)*cumsum(c(0,spec)),labels = names(spec)))
    }
    
    res = split(x, grouping_vector)
    return(res)
}

## ---------------------------------- bioinfo ------------------------------------
library_metagenomic=function(){
    library(phyloseq)
    library(microbiome)
    library(microViz)
}

id2name=function(df_otu,df_taxa,rowname_is_taxid=F,taxname_col="species"){
    ## check
    if(!rowname_is_taxid){
        stop("Custom ERROR!!!!! Rownames should be taxid in both df_otu and df_taxa!")
    }
    
    ## reorder df_taxa
    df_taxa=df_taxa[df_otu%>%rownames,,drop=F]
    name_col=df_taxa[,taxname_col]
    
    ##rename
    rownames(df_otu)=name_col
    rownames(df_taxa)=name_col
    return(list(df_otu=df_otu,df_taxa=df_taxa))
}

get_kegg_pathway_desc=function(path="/home/junsheng/BIN/junsheng/IMPORT/KEGG_pathway_desc.tsv"){
    if(file.exists(path)){
        print("Existed, read.")
        df_kegg=read_tsv(path)
    }else{
        print("Non existed, need network to build a new one. Also need the KEGGREST package.")
        df_kegg=data.frame(description=KEGGREST::keggList("pathway"))%>%
            rownames_to_column("map")%>%
            mutate(number=str_extract(map,"[0-9]+"))%>%
            mutate(ko=paste0("ko",number))%>%
            select(number,ko,map,description)
        }
    return(df_kegg)
    }

## ---------------------------------- metagenomics ----------------------------------
#library(phyloseq)
#library(microbiome)
#library(microViz)
transform_abs_to_rel=function(phy_obj){
    phy_obj_rel=phy_obj%>%transform_sample_counts(function(x){x/sum(x)})
    return(phy_obj_rel)
}


aggregate_unclassified=function(phy_obj, level, unclassified_marker="unclassified"){
    
    phy_obj = aggregate_taxa(phy_obj, level)
    n_taxa_before_merge=phy_obj%>%tax_table%>%nrow
    
    ## tackle unclassified OTU, merge them into 1 OTU
    #### Why we need to mutate all the columns to unclassified_marker? It's because although if a taxa's Species level are unclassified, their Order or Phylum or even Superkingdom may different
    #### Then they will be treated as different in aggregate_taxa.
    df_taxa = tax_table(phy_obj)
    
    #### replace all the columns in that index as unclassified_marker
    inds = which(df_taxa[,level]==unclassified_marker)
    df_taxa[inds, ] = unclassified_marker
    tax_table(phy_obj)=df_taxa
    
    #### aggregate all OTUs with unclassified_marker
    phy_obj=aggregate_taxa(phy_obj, level)
    n_taxa_after_merge=phy_obj%>%tax_table%>%nrow
    
    print(sprintf("Merge all the OTUs in %s level to a specific OTU called '%s', %s taxa before merge and %s taxa after that",level,unclassified_marker,n_taxa_before_merge,n_taxa_after_merge))
    return(phy_obj)
}

aggregate_rare_plus = function (phy_obj, level, unclassified_marker="unclassified", detection, prevalence, abundance, include_lowest=F,...){

    ## tackle unclassified OTU
    phy_obj=phy_obj%>%aggregate_unclassified(level,unclassified_marker)
    
    
    ## aggregate others, merge them into 1 OTU
    if(include_lowest){
    not_rare = tax_filter(phy_obj, min_prevalence = prevalence,
                                         prev_detection_threshold = detection,
                                         min_total_abundance = abundance,
                                         names_only = T)
    }else{
    not_rare = tax_filter(phy_obj, min_prevalence = prevalence,
                                         undetected = detection,
                                         min_total_abundance = abundance,
                                         names_only = T)
    }
    #### Sometimes the unclassified reads are not dominated in abundance, then it will be merged into "Others". We should prevent this.
    not_rare=c(not_rare,unclassified_marker)%>%unique
    
    df_tax = tax_table(phy_obj)

    inds = which(!rownames(df_tax) %in% not_rare)
    
    #### Why we need to mutate all the columns to "Other"? It's because although if a taxa's Species level are "Other", their Order or Phylum or even Superkingdom may different
    #### Then they will be treated as different in aggregate_taxa.
    df_tax[inds,] = "Other"


    tax_table(phy_obj) = df_tax
    
    phy_filtered=aggregate_taxa(phy_obj, level)
    return(phy_filtered)

}
aggregate_taxa_plus=function(phy_obj,level,unclassified_marker="unclassified",verbose=F){
    ## compare to aggregate_taxa, aggregate_taxa_plus will first merge all the OTUs with the specific level==unclassified_marker into 1
    #### Why we need to mutate all the columns to unclassified_marker? It's because although if a taxa's Species level are unclassified, their Order or Phylum or even Superkingdom may different
    #### Then they will be treated as different in aggregate_taxa.
    phy_obj=phy_obj%>%aggregate_unclassified(level,unclassified_marker)
    phy_obj=phy_obj%>%aggregate_taxa(level,verbose)
}

aggregate_top_taxa_plus = function(phy_obj, top, level, unclassified_marker="unclassified",other_marker="Other",only_keep_level_aggregate=F) {
    ## tackle unclassified OTU
    phy_obj=phy_obj%>%aggregate_unclassified(level,unclassified_marker)

    ## tackle others
    phy_obj = aggregate_taxa(phy_obj, level)

    top_taxas = top_taxa(phy_obj, top+1)
    df_taxa = tax_table(phy_obj)

    inds = which(!rownames(df_taxa) %in% top_taxas)

    df_taxa[inds, ] = other_marker

    if(only_keep_level_aggregate){
        df_taxa_new=df_taxa[, level]
    }else{
        df_taxa_new=df_taxa
    }
    
    tax_table(phy_obj) = tax_table(df_taxa_new)

    phy_obj=aggregate_taxa(phy_obj, level)

    return(phy_obj)
}

check_abs_rel_clr=function(df_otu, round_precision=10){

    df_otu=df_otu%>%data.frame

    if( all(df_otu >= 0) & any(df_otu > 1)){
        marker="ABS"
    }else if( all(df_otu <= 1) & all(df_otu >= 0)){
        marker="REL"
    }else if( any(df_otu < 0) & any(df_otu > 1)){
        colmeans_sum=df_otu %>% colMeans %>% sum %>% round(round_precision)
        if(colmeans_sum != 0){
            print_t="Maybe trim df_otu in CLR, or even not a CLR. Please check cautiously."
            warning(print_t)
            pretty_print(print_t)
        }
        marker="CLR"
    }else{
        marker="Unknown"
    }
    pretty_print(sprintf("Detect %s",marker))
    return(marker)
}

## ---------------------------------- publish version of test function ----------------------------------
publish_t_test = function(.data, group_col, value_col, strata_col = NULL, paired_test = F, var_equal=F, adj_method = "fdr", digit_round = 3) {
    # library
    library(rstatix)
    # Rename columns for the final table
    rename_adj_method = paste0("p.adj", " (", adj_method, ")")
    rename_t = "t-value"
    rename_p = "P-value"
    rename_ci = "95% CI"

    # Convert column names to symbols
    group_col_sym = as.name(group_col)
    value_col_sym = as.name(value_col)
    strata_col_sym = if (!is.null(strata_col)) as.name(strata_col) else NULL

    form = paste(value_col, "~", group_col) %>% as.formula
    # Run the t-test with error handling

    if(is.null(strata_col)){
        df_t_test_res = .data %>% t_test(formula = form, paired = paired_test,var.equal=var_equal, p.adjust.method="none",detailed = T) 

    }else{

        df_t_test_res = foreach(data_chunk = .data %>% group_split(!!strata_col_sym), .combine = 'bind_rows') %dopar% {
            strata_marker=unique(data_chunk[[strata_col]])
            

            tryCatch(
            {
                test_result = data_chunk %>%
                              t_test(formula = form, paired = paired_test, var.equal = var_equal, p.adjust.method = "none", detailed = T) %>%
                              mutate(!!strata_col_sym:=strata_marker) %>%
                              select(!!strata_col_sym,everything())

                return(test_result)
            }, error = function(e) {
                error_df = data.frame("strata" = strata_marker, "error_message" = e %>% conditionMessage)
                error_df = error_df %>% rename(!!strata_col_sym := "strata") %>%
                                        mutate("rename_t" = NA, "rename_p" =NA, "p.adj" =NA, "95% CI" =NA)
                return(error_df)
            }
            )
        }
    }

    if(all(is.na(df_t_test_res[["p"]]))){
        warning("All failed.")
        return(df_t_test_res)
    }

    df_t_test_res = df_t_test_res %>% adjust_pvalue(p.col="p",method=adj_method,output.col="p.adj") %>%
                                        add_significance("p.adj") %>%
                                        rename(!!rename_adj_method :="p.adj",!!rename_t := "statistic", !!rename_p := "p") %>%
                                        mutate_if(is.numeric, ~round(.x, digit_round)) %>%
                                        mutate(!!rename_ci:=sprintf("[%s, %s]",conf.low,conf.high))%>%
                                        select(-c(estimate,method,alternative,conf.low,conf.high))

    # Get summary statistics (Mean and SD) for each group
    stat_col = "Mean ± SD"
    df_summary_stats = .data %>% group_by(!!strata_col_sym, !!group_col_sym) %>%
                                get_summary_stats(!!value_col_sym,type = "mean_sd", show = c("mean", "sd")) %>%
                                mutate_if(is.numeric, ~round(.x, digit_round)) %>%
                                mutate(!!stat_col := sprintf("%s ± %s", mean, sd), !!group_col_sym := sprintf("%s (%s)", !!group_col_sym, stat_col)) %>%
                                pivot_wider(id_cols = strata_col, names_from = group_col, values_from = stat_col)

    # Perform a full join on strata_col and then relocate specific columns to the end
    if (is.null(strata_col)){
        df_final = df_t_test_res %>% cbind(df_summary_stats)
    }else{
        df_final = df_t_test_res %>% full_join(df_summary_stats, by = strata_col)    
    }
    df_final=df_final%>%relocate(all_of(c(rename_t, rename_p, rename_adj_method, rename_ci)), .after = last_col())

               
    return(list("test"=df_t_test_res,"desc"=df_summary_stats,"merge"=df_final))
}

publish_wilcox_test = function(.data, group_col, value_col, strata_col = NULL,paired_test = F, adj_method = "fdr", digit_round = 3) {
    # library
    library(rstatix)
    library_parallel()
    # rename info
    rename_adj_method = paste0("p.adj"," ","(",adj_method,")")
    rename_p = "P-value"
    rename_w = "W-statistics" 

    # Convert column names to symbols
    group_col_sym = as.name(group_col)
    value_col_sym = as.name(value_col)
    strata_col_sym = if (!is.null(strata_col)) as.name(strata_col) else NULL


    # Run the Wilcoxon test with error handling and automatic row binding
    form = sprintf("%s~%s",value_col,group_col) %>% as.formula


    if (is.null(strata_col)) {
        # If strata_col is NULL, run the Wilcoxon test directly on the entire data
        df_wilcox_res = .data %>%wilcox_test(form, paired = paired_test, p.adjust.method = "none", detailed = F)

    } else {
        
        df_wilcox_res = foreach(data_chunk = .data %>% group_split(!!strata_col_sym), .combine = 'bind_rows') %dopar% {
            strata_marker=unique(data_chunk[[strata_col]])

            tryCatch(
            {
                test_result = data_chunk %>%
                                  wilcox_test(formula = form, paired = paired_test, p.adjust.method = "none", detailed = F) %>%
                                  mutate(!!strata_col_sym:=strata_marker)%>%
                                  select(!!strata_col_sym,everything())
                return(test_result)

            }, error = function(e) {

                error_df = data.frame("strata" = strata_marker, "error_message" = e%>%conditionMessage)
                error_df = error_df %>% rename(!!strata_col_sym := "strata") %>%
                                        mutate("statistic" = NA, "p" =NA, "p.adj" :=NA)
                return(error_df)

            }
            )
        }
    }

    if(all(is.na(df_wilcox_res[["p"]]))){
        warning("All failed.")
        return(df_wilcox_res)
    }

    df_wilcox_res = df_wilcox_res %>% adjust_pvalue(p.col="p",method=adj_method,output.col="p.adj") %>%
                                        add_significance("p.adj") %>%
                                        rename(!!rename_adj_method :="p.adj",!!rename_w := "statistic", !!rename_p := "p") %>%
                                        mutate_if(is.numeric, ~round(.x, digit_round))




    # Get summary statistics (Median and IQR) for each group
    stat_col = "Median (IQR)"
    df_summary_stats = .data %>% group_by(!!strata_col_sym,!!group_col_sym) %>%
                                get_summary_stats(!!value_col_sym,type = "five_number", show = c("median", "q1", "q3")) %>%
                                mutate_if(is.numeric, ~round(.x, digit_round)) %>%
                                mutate(!!stat_col := sprintf("%s (%s, %s)", median, q1, q3), !!group_col_sym := sprintf("%s (%s)", !!group_col_sym, stat_col)) %>%
                                pivot_wider(id_cols = strata_col, names_from = group_col, values_from = stat_col)



    # Perform a full join on strata_col and then relocate specific columns to the end
    if (is.null(strata_col)){
        df_final = df_wilcox_res %>% cbind(df_summary_stats)
    }else{
        df_final = df_wilcox_res %>% full_join(df_summary_stats, by = strata_col)    
    }
    df_final=df_final%>%relocate(all_of(c(rename_w, rename_p, rename_adj_method)), .after = last_col())



    return(list("test"=df_wilcox_res,"desc"=df_summary_stats,"merge"=df_final))
}

publish_adonis = function(response_var, input_matrix, distance_method = "bray", digit_round = 3) {
  # Load required libraries
  library(vegan)
  library(broom)
  
  # Check if the input_matrix is a distance matrix
  if (class(input_matrix) != "dist") {
    # Calculate the distance matrix if it's not
    dist_matrix = vegdist(input_matrix, method = distance_method, na.rm = T)
  } else {
    dist_matrix = input_matrix
  }
  
  # Prepare the data frame for adonis2
  df_data = data.frame("response_var"=response_var)
  
  # Run the Adonis test
  res = adonis2(dist_matrix ~ response_var, data = df_data, method = distance_method, na.action = na.exclude)
  # Extract and rename the results
  save_res = (res %>% tidy)[1,]
  save_res = save_res %>% rename(SS = SumOfSqs, `F-model` = statistic, `P-value` = p.value) %>% 
                          mutate(MS = SS / df) %>% 
                          select(term, df, SS, MS, `F-model`, R2, `P-value`) %>%
                          mutate_if(is.numeric, ~round(.x, digit_round))
  
  return(list("dist_matrix"=dist_matrix,"response_var"=df_data,"adonis_res"=res, "tidy_res"=save_res))
}

# Function to perform Mantel Test
publish_mantel_test = function(df_matrix1, df_matrix2, 
                                distance_method1 = "bray", distance_method2 = "bray", 
                                correlation_method = "pearson", digit_round = 3
                              ) {

    # Load the required library
    library(vegan)

    # Calculate the distance matrices
    dist_matrix1 = vegdist(df_matrix1, method = distance_method1,na.rm=T)
    dist_matrix2 = vegdist(df_matrix2, method = distance_method2,na.rm=T)

    # Perform the Mantel test
    mantel_result = mantel(dist_matrix1, dist_matrix2, 
                          method = correlation_method, permutations = 999)
    df_mantel = data.frame(
        Corr_method = correlation_method,
        r = mantel_result$statistic,
        R2 = (mantel_result$statistic)^2,
        P_Value = mantel_result$signif,
        Permutations = mantel_result$permutations
    )
    
    df_mantel = df_mantel %>% mutate_if(is.numeric,~round(.x,digit_round))
    # Prepare the output

    return(list("dis_obj1"=dist_matrix1,"dis_obj2"=dist_matrix2,"mantel_res"=mantel_result,"tidy_res"=df_mantel))
}