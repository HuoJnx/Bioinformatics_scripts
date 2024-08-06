## ---------------------------------- echo -------------------------------------
print("Importing publish test script.")


publish_t_test = function(.data, group_col, value_col, strata_col = NULL, desc_type = c("mean","sd"),paired_test = F, var_equal=F, adj_method = "fdr", digit_round = 3) {
    function_say("Hey!")
    # library
    quiet_library(rstatix)
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

    # Get summary statistics (Mean and SD/sem) for each group
    function_say(sprintf("Use %s for description", desc_type))
    if (desc_type[1] !="mean"){
        function_say("When using t-test, you should describe the data by mean first. So you need to specify the first element in $desc_type as 'mean'.",type="error")
    }
    if(desc_type[2]=="sd"){
        stat_col = "Mean ± SD"
    }else if (desc_type[2]=="se"){
        stat_col = "Mean ± s.e.m"
    }else{
        function_say("The second statistics can only be 'sd' or 'se'",type="error")
    }
    stat_col_sym = stat_col %>% as.name
    sd_se_col_sym = desc_type[2] %>% as.name
    
    df_summary_stats = .data %>% group_by(!!strata_col_sym, !!group_col_sym) %>%
                                get_summary_stats(!!value_col_sym,type = "common", show = desc_type) %>%
                                mutate_if(is.numeric, ~round(.x, digit_round)) %>%
                                mutate(!!stat_col_sym := sprintf("%s ± %s", mean, !!sd_se_col_sym)) %>%
                                mutate(!!group_col_sym := sprintf("%s (%s)", !!group_col_sym, stat_col)) %>%
                                pivot_wider(id_cols = strata_col, names_from = group_col, values_from = stat_col)

    # Perform a full join on strata_col and then relocate specific columns to the end
    if (is.null(strata_col)){
        df_final = df_t_test_res %>% cbind(df_summary_stats)
    }else{
        df_final = df_t_test_res %>% full_join(df_summary_stats, by = strata_col)    
    }
    df_final=df_final%>%relocate(all_of(c(rename_t, rename_p, rename_adj_method, rename_ci)), .after = last_col())

    function_say("Bye.")
    return(list("test"=df_t_test_res,"desc"=df_summary_stats,"merge"=df_final))
}

publish_wilcox_test = function(.data, group_col, value_col, strata_col = NULL,paired_test = F, adj_method = "fdr", digit_round = 3) {
    function_say("Hey!")
    # library
    quiet_library(rstatix)
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


    function_say("Bye.")
    return(list("test"=df_wilcox_res,"desc"=df_summary_stats,"merge"=df_final))
}

publish_adonis = function(Y, input_matrix, distance_method = "bray", digit_round = 3) {
  # Load required libraries
  quiet_library(vegan)
  quiet_library(broom)
  
  # Check if the input_matrix is a distance matrix
  if (class(input_matrix) != "dist") {
    # Calculate the distance matrix if it's not
    dist_matrix = vegdist(input_matrix, method = distance_method, na.rm = T)
  } else {
    dist_matrix = input_matrix
  }
  
  # Prepare the data frame for adonis2
  df_data = data.frame(".y."=Y)
  
  # Run the Adonis test
  res = adonis2(dist_matrix ~ .y., data = df_data, method = distance_method, na.action = na.exclude)
  # Extract and rename the results
  save_res = (res %>% tidy)[1,]
  save_res = save_res %>% rename(SS = SumOfSqs, `F-model` = statistic, `P-value` = p.value) %>% 
                          mutate(MS = SS / df) %>% 
                          select(term, df, SS, MS, `F-model`, R2, `P-value`) %>%
                          mutate_if(is.numeric, ~round(.x, digit_round))
  
  return(list("dist_matrix"=dist_matrix,".y."=df_data,"adonis_res"=res, "tidy_res"=save_res))
}

# Function to perform Mantel Test
publish_mantel_test = function(df_matrix1, df_matrix2, 
                                distance_method1 = "bray", distance_method2 = "bray", 
                                correlation_method = "pearson", digit_round = 3
                              ) {

    # Load the required library
    quiet_library(vegan)

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