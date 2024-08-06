## ---------------------------------- echo -------------------------------------
print("Importing R basic script.")
## ---------------------------------- options ----------------------------------

options(repr.matrix.max.rows=20)
size_obj=c(8,8)
options(repr.plot.width=size_obj[1], repr.plot.height=size_obj[2])

quiet_library = function(package) {
    package_name = deparse(substitute(package))
    suppressPackageStartupMessages(library(package_name, character.only = TRUE))
}


function_say = function(message, type="message") {
    caller_calls = sys.calls()
    if (length(caller_calls) > 1) {
        caller_call = caller_calls[[length(caller_calls) - 1]]
        function_name = as.character(caller_call[[1]])
    } else {
        function_name = "Global"
    }

    formatted_message = sprintf("》》%s : %s", function_name, message)
    if(type=="warning"){
        warning(formatted_message)
    }else if(type=="error"){
        stop(formatted_message)
    }else if(type=="message"){
        cat(formatted_message, "\n")
    }

}

check_global = function(f) {
    all_globals_in_func = codetools::findGlobals(f)
    
    # All variables in the global environment
    all_globals_in_env = ls(envir = .GlobalEnv)
    
    # Intersect with globals to find those that are also global variables (Which means R will fetch the value from global variables)
    used_globals = intersect(all_globals_in_func, all_globals_in_env)
    
    # Convert the vectors to comma-separated strings
    used_globals_str = paste(used_globals, collapse = ", ")
    
    # Get the name of the function passed as an argument
    func_name = deparse(substitute(f))
    
    # Prepare the return string with both sets of information

    return(sprintf("Check %s, global vars 》》 %s.", func_name, used_globals_str))

}

set_fig_size = function(width,height,size_obj = NULL){
    if(!is.null(size_obj)){
        options(repr.plot.width = size_obj[1], repr.plot.height = size_obj[2])
    }else{
        options(repr.plot.width = width, repr.plot.height = height)
    }

}

print_rich = function(obj){
    IRdisplay::display(obj)
}
print_all_row = function(df){
    original_show_rows=getOption("repr.matrix.max.rows")
    options(repr.matrix.max.rows = Inf)
    print_rich(df)
    options(repr.matrix.max.rows = original_show_rows)
}

v = function(text){
    return(packageVersion(text))
}
## ---------------------------------- file operation -----------------------------
basename_without_suffix = function(x, only_remove_the_last_suffix = T){
    DESCRIPTION = "If only_remove_the_last_suffix == T, will will only remove the last '.+$'"
    x = basename(x)
    if(only_remove_the_last_suffix){#will only remove the last ".+$"
        x = str_remove(x,regex("\\.[^.]+$"))##match a . then [^.] match not period, 
        
    }else{# will remove all the ".+$"
        x = str_remove(x,regex("\\..+$"))
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
library_parallel = function(n_cl = NULL){
    quiet_library(doParallel)
    quiet_library(foreach)
    if(is.null(n_cl)){
        doParallel::registerDoParallel()
    }else{
        doParallel::registerDoParallel(cores = n_cl)
    }
    
}

## ---------------------------------- write thing ----------------------------------
write_df_wrap=function(data, file_name,save_dir = ".",file_fmt = "tsv",rowname = "rowname",col_names = T,prompt = T){

    function_say("Saving dataframe.")
    ## check if data is null
    if(is.null(data)){
        function_say(sprintf("The dataframe with expected name '%s' is acntually NULL, skip saving.", file_name), type = "warning")
        return()
    }
    ## convert
    data = data %>% as.data.frame_plus()
    ## create folder
    dir.create(save_dir,showWarnings=F)
    ## filepath operation
    file_name = paste0(file_name,".",file_fmt)
    file_name = fs::path_sanitize(file_name)
    fig_path = file.path(save_dir,file_name)
    ## rownames
    if(has_rownames(data)){
        data = data %>% rownames_to_column(rowname)
        tt = sprintf("》》Rowname detected, coverted it to a column call '%s.'",rowname)
        function_say(tt)
    }
    ## format
    if(file_fmt == "tsv"){
        write_tsv(data,fig_path,col_names = col_names)
    }else if(file_fmt == "csv"){
        write_csv(data,fig_path,col_names = col_names)
    }else if(file_fmt == "xlsx"){
        quiet_library(writexl)
        write_xlsx(data,fig_path,col_names = col_names,format_headers = F)
    }else{
        function_say("Only support tsv, csv and xlsx.",type = "error")
    }
    ## prompt
    if(prompt){
        tt = sprintf("Successfully save data.frame to %s",fig_path)
        function_say(tt)
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
        print(tt)
    }
}
write_RDS_plus = function(object, file_name, save_dir = ".", file_fmt = "rd", prompt = T) {
    # Create the directory if it doesn't exist
    dir.create(save_dir, showWarnings = F)
  
    # Construct the file name and sanitize it
    file_name = paste0(file_name, ".", file_fmt)
    file_name = fs::path_sanitize(file_name)
  
    # Construct the full file path
    file_path = file.path(save_dir, file_name)
  
    # Save the object as an RDS file
    saveRDS(object, file_path)
  
  # Print a message to the console if prompt is TRUE
    if (prompt) {
        tt = sprintf("Successfully saved object to %s", file_path)
        print(tt)  # Assuming you have a function called print to format the text
  }
}

write_RDS_if_non_existed_else_read = function(rds_name, rds_dir = ".", rds_fmt = "rd", command_func) {
    DESCRIPTION = "It's not a real function!!!!!! It's just for copy and paste!!!!!!!!"
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

## str_split_all is a function made by myself, and then I found stringr::str_split_1 has provide the same function
## str_split_1 is a function that can defaultly split the vector to pieces by pattern.
str_split_all = stringr::str_split

## str_split_get_the_N_col is a function made by myself, and then I found stringr::str_split_i has provide the same function
str_split_get_the_N_col = stringr::str_split_i

str_split_into_chunks <- function(str_vector, chunk_length, sep = "\n") {
    DESCRIPTION = function(){
        cat("This function split the string into chunks by length, then which concatinated again by seq. (Support vector calculation)")
    }
    # mutate to character to prevent it's factor
    str_vector = as.character(str_vector)

    # Function to split a single string into chunks
    split_single_text <- function(text) {
        # Split the text into individual characters
        sst <- strsplit(text, "")[[1]]
        
        # Calculate the total length of the text
        length_text <- nchar(text)
        
        # Calculate the number of chunks needed
        n_chunk <- ceiling(length_text / chunk_length)
        
        # Create chunks
        chunks <- lapply(1:n_chunk, function(i) {
            start_index <- (i - 1) * chunk_length + 1
            end_index <- min(i * chunk_length, length_text)
            paste0(sst[start_index:end_index], collapse = "")
        })
        
        # Combine chunks with newline separator
        paste0(chunks, collapse = sep)
    }
    
    # Apply split_single_text to each element in str_vector
    result <- sapply(str_vector, split_single_text, USE.NAMES = FALSE)
    return(result)
}

str_sub_plus = function(string,start,end=NA){
    if(is.na(NA)){
        end = start
    }
    sub_res = str_sub(string = string, start = start, end = end)
    return(sub_res)
}

## ---------------------------------- format conversion -------------------------
as.percentage = function(numbers, digits = 2, return_type = "float", add_percent_sign = FALSE) {
    # Convert the numbers to percentages
    percentages = numbers * 100
    
    # Round the percentages to the specified number of digits
    rounded_percentages = round(percentages, digits)
    
    # Check the return type
    if (return_type == "str") {
        # Convert to string and add "%" if required
        if (add_percent_sign) {
            results = paste0(format(rounded_percentages, nsmall = digits), "%")
        } else {
            results = format(rounded_percentages, nsmall = digits)
        }
    } else if (return_type == "float") {
        results = rounded_percentages
    } else {
        stop("Invalid return_type. Choose either 'str' or 'float'.")
    }
    
    return(results)
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
quiet_library(tidyverse)

read_xlsx = openxlsx::read.xlsx
read_xlsx2 = readxl::read_excel

set_colnames = magrittr::set_colnames
set_rownames = magrittr::set_rownames

table_df = function(.data) {
    count_table = .data %>% pivot_longer(cols = everything(), names_to = "Variable", values_to = "Class") %>%
                    group_by(Variable, Class) %>%
                    summarise(Count = n(), .groups = 'drop') %>%
                    pivot_wider(names_from = Variable, values_from = Count, values_fill = 0)
    return(count_table)
}

format_p = function(p_values, round_digits = 4, thresh = 0.001, need_p_equal_prefix = F) {
    DESCRIPTION = "The Function formats p-values for reporting. It takes a vector of p-values and returns a character vector.
    P-values below the threshold are formatted as 'P < threshold', and other values are optionally prefixed with 'P ='."
    sapply(p_values, function(p) {
        p_round = round(p, round_digits)
        if (p_round < thresh) {
            p_char = sprintf("P < %s", thresh)
        } else {
            if (need_p_equal_prefix) {
                p_char = sprintf("P = %s", p_round)
            } else {
                p_char = as.character(p_round)
            }
        }
        return(p_char)
    })
}

robust_check_type = function(data, what_you_want_to_check = "data.frame", prompt = T) {
    data_type = class(data)
    standard_data = switch(what_you_want_to_check,
                            "data.frame" = data.frame(),
                            "matrix" = matrix(),
                            "list" = list(),
                            "numeric" = numeric(),
                            "character" = character(),
                            "factor" = factor(),
                            "integer" = integer(),
                            "logical" = logical(),
                            "dist" = dist(matrix(numeric(0), nrow = 0)),
                            stop("Invalid 'what_you_want_to_check' value"))
    standard_type = class(standard_data)

    if (length(data_type) != 1) {
        if(prompt){
            function_say(sprintf("Not a pure %s, it has %d classes: %s.", what_you_want_to_check, length(data_type), paste(data_type, collapse = ", ")))
        }
        return(FALSE)
    } else {
        if (data_type != standard_type) {
            if(prompt){
                function_say(sprintf("Not a pure %s, it is %s.", what_you_want_to_check, data_type))
            }
            return(FALSE)
        } else {
            return(TRUE)
        }
    }
}

as.data.frame_plus = function(.data, make_rownames_to_column = F, var = "rowname") {
    DESCRIPTION = function(){
        cat("
            This internal function enhance the based as.data.frame from 2 perspective:
            1. Defaultly switch off the check.name parameter which is on in based as.data.frame
            2. When your data.frame has rownames, you can choose to move it to as a column with your prefered name.
            Parameters:
            .data: The input data to be converted into a dataframe.
            make_rownames_to_column: If set to TRUE, the row names of the dataframe are converted into a separate column.
            var: A string specifying the name of the new column that will contain the row names if make_rownames_to_column is set to TRUE. 

            ")
    }
    if(robust_check_type(.data, what_you_want_to_check="dist", prompt = F)){
        .data = .data %>% as.matrix %>% data.frame(check.names = F)
    }

    if(!robust_check_type(.data, what_you_want_to_check="data.frame", prompt = F)){
        .data = .data %>% data.frame(check.names = F)
    }

    if (make_rownames_to_column) {
        .data = .data %>% rownames_to_column(var)
    }

    return(.data)
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

filter_na_rows = function(df, filter_out = TRUE) {
    if (filter_out) {
        df %>%
            filter(if_any(everything(), ~ !is.na(.)))# select rows with at least 1 non-NA
    } else {
        df %>%
            filter(if_all(everything(), ~ !is.na(.)))
    }
}

filter_na_cols = function(df, filter_out = TRUE) {
    if (filter_out) {
        df %>%
            select(where(~ !all(is.na(.))))
    } else {
        df %>%
            select(where(~ all(!is.na(.))))
    }
}

filter_na_rows_and_cols = function(df, filter_out = TRUE) {
    df %>%
        filter_na_rows(filter_out = filter_out) %>%
        filter_na_cols(filter_out = filter_out)
}

str_split_str_merge = function(.data,sep,keep_range){
    .data = str_split(.data,sep,simplify = T)[,keep_range]
    .data = .data %>% data.frame
    .data = .data %>% unite(keep_range,sep = sep) %>% pull
    return(.data)
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
map_name_df = function(df, df_rename, colname_need_to_change, colname_old_name, colname_new_name, replace = T, Keep_original_if_unmap = T) {
  
  # only keep the needed col of df_rename
  df_rename = df_rename %>% select(all_of(c(colname_old_name, colname_new_name)))
  # Convert the column names to symbols for non-standard evaluation
  colname_new_name_sym = colname_new_name %>% as.name
  colname_need_to_change_sym = colname_need_to_change %>% as.name
  
  # Create a named vector for the left join
  left_join_by = c(colname_old_name) %>% setNames(colname_need_to_change)
  
  # Perform the left join
  df_rename_temp = df %>%
    left_join(df_rename, by = left_join_by)
  
  # If Keep_original_if_unmap is TRUE, replace NA values in the new name column with the original column's values
  if (Keep_original_if_unmap) {
    df_rename_temp = df_rename_temp %>%
      mutate(!!colname_new_name_sym := if_else(is.na(!!colname_new_name_sym), !!colname_need_to_change_sym, !!colname_new_name_sym))
  }
  
  # If replace is TRUE, replace the original column with the new name column
  if (replace) {
    df_rename_temp = df_rename_temp %>%
      mutate(!!colname_need_to_change_sym := !!colname_new_name_sym) %>%
      select(-!!colname_new_name_sym)
  }
  
  # Return the modified dataframe
  return(df_rename_temp)
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
        m = as.matrix(df)
        
    }else{
        m = df
    }
    median_m=matrixStats::rowMedians(m,useNames=T)
    return(median_m)
}

pivot_longer_wrapper=function(df, id_col_index, id_col_name=NULL, names_to=NULL, values_to=NULL){
    ## if id_col_index==0, means the rowname should be export and index should +1
    if(id_col_index == 0){
        df = df %>% rownames_to_column(id_col_name)
        id_col_index = id_col_index+1
    }
    need_col = (df%>%colnames)[-id_col_index]
    df = df %>% pivot_longer(cols=all_of(need_col),names_to=names_to,values_to=values_to)
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
        if(ncol(df)!=length(vec)){
            stop("Please ensure the ncol of dataframe is equal to length of vector.")
        }
    }
    exp=paste("df",operator,"vec")
    new_df=eval(rlang::parse_expr(exp))
    return(new_df)
}
apply_vector_2_dataframe_col_by_col=function(df,vec,operator="/",strict=T){
    if(strict){
        if(nrow(df)!=length(vec)){
            stop("Please ensure the nrow of dataframe is equal to length of vector.")
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

full_join_debug = function(df1, df2, by, df1_marker = "X", df2_marker = "Y", keep = NULL) {
  # Add unique source columns to df1 and df2
  df1 = df1 %>% mutate(.source_full_join_debug = df1_marker)
  df2 = df2 %>% mutate(.source_full_join_debug = df2_marker)
  
  # Perform full join with the keep argument
  df_merged = full_join(df1, df2, by = by, keep = keep)
  
  # Adding debug information
  df_debug = df_merged %>% mutate(.match_full_join_debug = case_when(
                              is.na(.source_full_join_debug.x) & !is.na(.source_full_join_debug.y) ~ "missing in X",
                              !is.na(.source_full_join_debug.x) & is.na(.source_full_join_debug.y) ~ "missing in Y",
                              TRUE ~ "All presented"
                                                    )
                                )
  print(df_debug$.match_full_join_debug %>% table())

  return(df_debug)
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
rename_plus=function(.data,old_name,new_name){
    DESCRIPTION = function(){
        cat("
    This function renames any colname you want, just provided a old_name vector and corresponding new_name vector.
    Parameters:
    .data       - The dataframe
    old_name - The current name or index of the column to be renamed. If numeric, it is treated as the column index.
    new_name - The new name for the column.
            ")
    }

    if(!robust_check_type(.data,"data.frame")){
        .data=as.data.frame_plus(.data)
    }

    if(is.numeric(old_name)){
        old_name=colnames(.data)[old_name]
    }
    rename_vector=setNames(old_name,new_name)
    .data = .data%>%rename(rename_vector)
    return(.data)
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
arrange_by_row = function(df, metrics = "mean", ascending = T, save_metrics_col = F){
    df = df %>% data.frame
    if(metrics == "mean"){
        df[["metrics"]] = rowMeans(df)
    }else{
        df[["metrics"]] = rowMedians(df)
    }
    if(ascending){
        df = df %>% arrange(metrics)
    }else{
        df = df %>% arrange(desc(metrics))
    }
    if(!save_metrics_col){
        df = df %>% select(-metrics)
    }
    return(df)
}

arrange_by_col = function(df, metrics = "mean", ascending = T, save_metrics_col = F){
    df = df %>% as.data.frame_plus() %>% t_df
    df_res = arrange_by_row(df, metrics = metrics, ascending = ascending, save_metrics_col = save_metrics_col)

    df_res = df_res %>% t_df
    return(df_res)
}

arrange_by_count <- function(.data, col4arrange, ascending = TRUE, return_factor = TRUE, keep_calculation_process = F) {
    # ungroup before arrange
    .data <- .data %>% ungroup
    
    # Add a count of occurrences of each unique value in col4arrange
    arranged_data <- .data %>%
        add_count(!!sym(col4arrange), name = ".GroupCount")
    
    # Arrange the data based on the count, in ascending or descending order
    arranged_data <- if (ascending) {
        arranged_data %>% arrange(.GroupCount, !!sym(col4arrange))
    } else {
        arranged_data %>% arrange(desc(.GroupCount), !!sym(col4arrange))
    }
    
    # Convert col4arrange to a factor with levels in the order they appear
    if (return_factor) {
        arranged_data <- arranged_data %>%
                            mutate(!!sym(col4arrange) := factor(!!sym(col4arrange), levels = unique(!!sym(col4arrange))))
    }
    
    # Remove the temporary .GroupCount column
    if(!keep_calculation_process) {
        arranged_data <- arranged_data %>% select(-.GroupCount)
    }
    return(arranged_data)
}

arrange_by_count_condition <- function(.data, col4arrange, condition_expression, count_mode = "perc", ascending = TRUE, return_factor = TRUE, keep_calculation_process = FALSE) {
    
    # remove all the group factor
    .data = .data %>% ungroup
    # Check if the count_mode is valid
    if (!count_mode %in% c("perc", "abs")) {
        stop("Invalid count_mode. Use 'perc' for percentage or 'abs' for absolute count.")
    }
    
    # Create a temporary column to store the logical condition
    .data <- .data %>%
        mutate(.TempCondition = eval(parse(text = condition_expression)))
    
    # Group by the column for arrangement and summarise the count based on the mode
    summarised_data <- .data %>%
        group_by(!!sym(col4arrange)) %>%
        summarise(
            .Count = sum(.TempCondition),
            .Total = n()
        ) %>%
        mutate(
            .CountPerc = .Count / .Total,
            .ConditionExpression = condition_expression # Add the condition expression as a column
        )
    
    # Join the summarised data back to the original data
    .data <- .data %>%
        left_join(summarised_data, by = col4arrange)
    
    # Determine the column to sort by
    arrange_column <- ifelse(count_mode == "perc", ".CountPerc", ".Count")
    
    # Arrange the original data
    arranged_data <- .data %>%
        arrange(if (ascending) !!sym(arrange_column) else desc(!!sym(arrange_column)))
    
    # Optionally return the column as a factor
    if (return_factor) {
        arranged_data <- arranged_data %>%
            mutate(!!sym(col4arrange) := factor(!!sym(col4arrange), levels = unique(arranged_data[[col4arrange]])))
    }
    
    # Optionally drop temporary columns
    if (!keep_calculation_process) {
        arranged_data <- arranged_data %>%
            select(-.ConditionExpression, -.TempCondition, -.Count, -.Total, -.CountPerc)
    }
    
    # Return the arranged data
    arranged_data
}

# Example usage
# df <- data.frame(Description = rep(c("A", "B", "C"), each = 5), sig_marker = c("Sig", "Not Sig", "Sig", "Sig", "Not Sig", "Not Sig", "Not Sig", "Sig", "Sig", "Not Sig", "Sig", "Sig", "Sig", "Not Sig", "Sig"))
# result <- arrange_by_count_condition(df, "Description", 'sig_marker == "Sig"', count_mode = "perc", ascending = FALSE, keep_count_cols = TRUE)
# print(result)


#### arrange row by clustering method
arrange_by_hclust=function(df,dist_method="euclidean",hclust_method="average"){
    dist_mat=stats::dist(df,method=dist_method)
    hclust_avg=stats::hclust(dist_mat,method=hclust_method)
    df2=df[hclust_avg$order,]
    row_order=df2%>%rownames
    return(list(ordered_df=df2,row_order=row_order))
}










## ---------------------------------- rstatix -------------------------------------
add_xy_position_plus=function(subseted_rstatix_test_obj,x="taxa",dodge=0.8){
    quiet_library(rstatix)
    ## get y position
    rstatix_test_obj = subseted_rstatix_test_obj%>%add_y_position
    ## get data.frame
    df_rstatix_test_obj = rstatix_test_obj%>%as.data.frame
    # add xmin xmax
    dodge_each_side = dodge/4
    df_rstatix_test_obj = df_rstatix_test_obj %>% mutate(x=1:nrow(df_rstatix_test_obj)) %>%
                                                mutate(xmin = x - dodge_each_side, xmax = x + dodge_each_side)
    return(df_rstatix_test_obj)
}


## ---------------------------------- corr ------------------------------------
#library(ggcorrplot2)
#ggcorrplot(cor_matrix, method = "ellipse")
#library(GGally)#ggpairs(original_df)
#library(psych)#corr.test
#library(pheatmap)

# Define a function to select the upper triangle of a matrix, convert it to a data frame, and pivot to longer format
upper_tri_to_long = function(mat, value_name) {
    value_name_sym = value_name %>% as.name
    
    # Check if the matrix is itself to itself
    if (identical(rownames(mat), colnames(mat))) {
        print("Itself to itself correlation, will keep the upper triangle, but remove the diagonal.")
        mat = rstatix::pull_upper_triangle(mat, diagonal = F) %>% as.data.frame_plus
    } else {
        print("Not itself to itself, will keep the whole matrix instead of pulling the upper triangle.")
        mat = mat %>% as.data.frame_plus %>%
                        rownames_to_column("rowname")
        # mat remains unchanged
    }

    # Convert to long format and rename columns
    df_long = mat %>% as.data.frame_plus %>% 
                        pivot_longer_wrapper(id_col_index = 1, values_to = value_name, names_to = "to") %>%
                        filter(!!value_name_sym!="") %>%
                        rename("from"="rowname") %>%
                        mutate(!!value_name_sym:=as.numeric(!!value_name_sym))

    return(df_long)
}





# Define the parsing function
parse_corr_res = function(cor_res_obj, corr_marker = "r", p_marker = "p", padj_method = "fdr", arrange_p = T, round = 4) {
  
    # Preprocess the 'r' and 'p' matrices
    r_long = upper_tri_to_long(cor_res_obj[[corr_marker]], corr_marker)
    p_long = upper_tri_to_long(cor_res_obj[[p_marker]], p_marker)
    
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

psych_to_graph = function(corr_res_obj, r_marker, p_marker, p_thresh, weight_thresh){
    df_weight = corr_res_obj[[r_marker]] %>% as.data.frame_plus
    df_p = corr_res_obj[[p_marker]] %>% as.data.frame_plus
    
    filtered_df_weight = map2_df(.x = df_weight, .y = df_p, .f = function(x, y) { ifelse(y <= p_thresh & abs(x) >= weight_thresh, x, 0)}) %>% as.data.frame
    rownames(filtered_df_weight) = rownames(df_weight)
    g = igraph::graph_from_adjacency_matrix(filtered_df_weight %>% as.matrix,
                                mode = 'undirected',
                                weighted = TRUE,
                                diag = FALSE)
    num_nodes = igraph::vcount(g)
    num_edges = igraph::ecount(g)

    print(sprintf("Using p-value threshold: %s and absolute weight threshold: %s", p_thresh, weight_thresh))
    print(sprintf("Number of nodes and edges in the filtered graph: %s, %s", num_nodes, num_edges))
    return(g)
}
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

scale_plus = function(.data, by = "row", center = T, scale = T){
    if(by == "row"){
        .data = scale_by_row(.data)
    }else if(by == "col"){
        .data = scale_by_col(.data)
    }else{
        function_say("by should be row or col", type = "error")
    }
}
scale_by_row = function(.data, center = T, scale = T){
    .data = .data %>% t_df %>% scale_by_col(center = center, scale = scale) %>% t_df
    return(.data)
}
scale_by_col = function(.data, center = T, scale = T){
    .data = .data %>% mutate_all(~as.vector(scale(.x, center = center, scale = scale)))
    return(.data)
}

## ---------------------------------- ggplot2 ----------------------------------
ggplot2_aux_script = file.path(dirname(SOURCE_SCRIPT_PATH),"2import_ggplot2_aux.R")
source(ggplot2_aux_script)
## ---------------------------------- metagenomics ----------------------------------

metagenomic_script = file.path(dirname(SOURCE_SCRIPT_PATH),"2import_metagenomics.R")
source(metagenomic_script)

## ---------------------------------- publish version of test function ----------------------------------
publish_test_script = file.path(dirname(SOURCE_SCRIPT_PATH),"2import_publish_test.R")
source(publish_test_script)
## ---------------------------------- ggplot2 ----------------------------------
matrix_aux_script = file.path(dirname(SOURCE_SCRIPT_PATH),"2import_matrix_aux.R")
source(matrix_aux_script)
## ---------------------------------- functional ----------------------------------
matrix_aux_script = file.path(dirname(SOURCE_SCRIPT_PATH),"2import_functional_aux.R")
source(matrix_aux_script)