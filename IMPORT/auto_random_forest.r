library(tidymodels)
library_parallel()

calculate_k_folds = function(sample_size) {
  if (sample_size < 100) {
    k = 10  # For very small datasets, use 10-fold CV
  } else if (sample_size >= 100 && sample_size < 1000) {
    k = 8   # For moderately small datasets, use 8-fold CV
  } else if (sample_size >= 1000 && sample_size < 10000) {
    k = 5   # For larger datasets, 5-fold CV is often sufficient
  } else {
    k = 3   # For very large datasets, even 3-fold CV can be sufficient
  }
  return(k)
}

auto_random_forest_all_train_all_test = function(df, target_col, mode = "classification", 
                                                 impute_na = T, remove_zero_var = T, 
                                                 normalize = T, 
                                                 metric_for_classification = "roc_auc", 
                                                 metric_for_regression = "rsq", 
                                                 top_n = 20, save_dir = ".", size=c(10,8)) {
    # check if workflow existed
    workflow_name = "Finalized_workflow"
    workflow_path = cat_file_path(workflow_name,save_dir,"rd")

    if(file.exists(workflow_path)){
        stop("Finalized workflow existed.")
    }

    # Choose the appropriate metric based on the mode
    chosen_metric = if (mode == "classification") metric_for_classification else metric_for_regression
    
    # Create initial recipe
    rec_obj = recipe(formula = as.formula(paste(target_col, "~ .")), data = df)
    if (impute_na) rec_obj = step_impute_median(rec_obj, all_predictors())
    if (remove_zero_var) rec_obj = step_zv(rec_obj, all_predictors())
    if (normalize) rec_obj = step_normalize(rec_obj, all_predictors())
    
    # Create random forest model specification
    mod_obj = rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
              set_engine("ranger", importance = "impurity_corrected") %>%
              set_mode(mode)
    
    # Create workflow
    wf_obj = workflow() %>%
             add_recipe(rec_obj) %>%
             add_model(mod_obj)
    
    # Bootstrap resampling
    boost_obj = bootstraps(df, times = 30, strata = all_of(target_col))
    
    # Model tuning
    tuned_wf = tune_grid(wf_obj, resamples = boost_obj, grid = 100, control = control_grid(verbose = T))
    fig_tunning = autoplot(tuned_wf)

    # Select best parameters and finalize workflow
    best_params = select_best(tuned_wf, metric = chosen_metric)
    final_wf = finalize_workflow(wf_obj, best_params)
    
    # Fit the final model on the full dataset
    last_fitted_wf = fit(final_wf, df)
    
    # Extract model
    final_mod = extract_fit_parsnip(last_fitted_wf) %>% .$fit

    # Get importance
    df_importance = ranger::importance(final_mod) %>% as.data.frame_plus %>% rownames_to_column("Taxa")
    colnames(df_importance) = c("Taxa","importance")
    df_importance_filter = df_importance %>% arrange(desc(importance)) %>%
                                             head(top_n) %>% 
                                             mutate(Taxa = fix_order(Taxa))
    
    
    # Cross-validation
    k_flod = calculate_k_folds(nrow(df))
    n_repeates = 500/k_flod
    cv_obj = vfold_cv(df, v = k_flod, repeats = n_repeates, strata = all_of(target_col))
    cv_fitted = fit_resamples(final_wf, resamples = cv_obj, control = control_resamples(verbose = T))
    df_cv = collect_metrics(cv_fitted, summarize = F) %>% mutate_if(is.double, ~round(.x, 4))
    
    ## t-test
    df_cv_for_test = df_cv %>% filter(.metric==chosen_metric)
    df_test = infer::t_test(df_cv_for_test,response=.estimate,mu=0.5,p.adjust.methods="none")

    ## summary
    df_cv_sum = cv_fitted %>% collect_metrics %>% mutate_if(is.double,~round(.x,4))

    # Save the data frame
    write_df_wrap(df_importance, "df_importance",save_dir)
    write_df_wrap(df_importance_filter, "df_importance_filter",save_dir)
    write_df_wrap(df_cv, "df_cv",save_dir)
    write_df_wrap(df_test, "df_test",save_dir)
    write_df_wrap(df_cv_sum, "df_cv_sum",save_dir)
    
    # save the final model
    write_RDS_plus(final_wf,workflow_name,save_dir)

    # Save the plot
    fig_top = plot_top_importance(df_importance_filter,"Taxa","importance")

    ggsave_wrap(fig_top,save_dir,fig_name="fig_importance",size=size)
    ggsave_wrap(fig_tunning,save_dir,fig_name="fig_tunning",size=c(8,8))

    # Return the final model, metrics, and top features
    return(list(final_model = final_mod, df_all_importance = df_importance, df_importance_filter = df_importance_filter))
}

plot_top_importance = function(df_top, taxa_col, importance_col,fill_col=NULL){
    if(is.null(fill_col)){
        fig_top = ggplot(df_top, aes_string(x = taxa_col, y = importance_col))
    }else{
        fig_top = ggplot(df_top, aes_string(x = taxa_col, y = importance_col, fill=fill_col))
    }
    fig_top = fig_top +
              geom_col() +
              coord_flip() +
              scale_x_discrete(limits=rev)
    return(fig_top)
}