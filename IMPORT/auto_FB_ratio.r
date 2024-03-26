auto_FB_ratio = function(phy_obj, 
                            group_col = NULL, 
                            Firmicutes_marker="Firmicutes",
                            Bacteroidetes_marker="Bacteroidetes",
                            plotting_style="color",
                            sig_test = T, 
                            p_type="p",
                            sig_type=".signif",
                            sig_thresh=0.05,
                            hide_ns_for_plot=F,
                            verbose_test_res_for_plot=T,
                            sample_label = "sample", 
                            plot_sample_label = T,
                            basic_font_size=10, 
                            save_dir = ".", 
                            filename_prefix = "", 
                            fig_fmt = "svg", 
                            size = c(8, 8)) {
    # print

    function_say("Welcome")
    # get df_meta

    df_otu = phy_obj %>% otu_table %>% as.data.frame_plus
    df_meta = phy_obj %>% sample_data
    df_meta = df_meta %>% as.data.frame_plus(make_rownames_to_column=T, var=sample_label)


    ## check if group_col existed
    #print(df_meta)
    adjusted_group_col_res=adjust_group_col(data=df_meta,group_col=group_col)
    df_meta = adjusted_group_col_res$data
    group_col = adjusted_group_col_res$group_col

    ## fix group_col order

    df_meta[[group_col]]=df_meta[[group_col]]%>%fix_order


    # check marker
    marker = phy_check_abs_rel_clr(df_otu)


    # Aggregate to phylum level
    
    phy_obj_phylum = phy_obj %>% aggregate_taxa(level = "phylum")

    # Get Bacteroidetes and Firmicutes abundances
    function_say("Calculating the F/B ratio.")
    df_FB_raw = phy_obj_phylum %>% otu_table %>% as.data.frame_plus %>% 
      filter(rownames(.) == Bacteroidetes_marker | rownames(.) == Firmicutes_marker)

    # Calculate F/B ratio
    df_FB_ratio = df_FB_raw[Firmicutes_marker, ] / df_FB_raw[Bacteroidetes_marker, ]


    # Merge with metadata

    df_FB_ratio_longer = df_FB_ratio %>% pivot_longer_wrapper(id_col_index = 0, id_col_name = "index", names_to = sample_label, values_to = "F_B_ratio") %>% 
                                            select(-index)

    df_FB_ratio_longer = df_FB_ratio_longer %>% left_join(df_meta)
                                        

    # Prepare for plotting
    df_plot = df_FB_ratio_longer
    ind = "F_B_ratio"
    function_say("Plotting figures and getting statistics.")


    fig_table=ggbox_points_pairwise_plus(df_plot=df_plot,
                                                  value_col=ind,
                                                  group_col=group_col,
                                                  plotting_style=plotting_style,
                                                  sig_test=sig_test,
                                                  general_stat_method="kruskal",
                                                  pairwise_stat_method = "wilcox", 
                                                  p_type = p_type, 
                                                  sig_type = sig_type, 
                                                  sig_thresh = sig_thresh, 
                                                  hide_ns_for_plot = hide_ns_for_plot, 
                                                  verbose_test_res_for_plot = verbose_test_res_for_plot, 
                                                  basic_font_size = basic_font_size
      )

    fig=fig_table$figure
    df_test=fig_table$test_results

    # save fig and df
    df_name = sprintf("%s_%s_%s",filename_prefix,marker,ind)
    fig_name = sprintf("%s_%s_%s",filename_prefix,marker,ind)
    
    # Optionally save test results
    if (sig_test && !is.null(df_test)) {
        test_results_name = sprintf("%s_%s_%s_test_results", filename_prefix, marker, ind)
        write_df_wrap(df_test, file_name=df_name, save_dir=save_dir)
    }
    
    # Save the figure
    ggsave_wrap(fig, file_name = fig_name, save_dir=save_dir, file_fmt = fig_fmt, size = size)


    return(list("figure"=fig,"data"=df_plot))
}
