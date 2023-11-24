auto_FB_ratio = function(phy_obj, 
                          group_col = "group", 
                          Firmicutes_marker="Firmicutes",
                          Bacteroidetes_marker="Bacteroidetes",
                          sample_label = "sample", 
                          plot_sample_label = T, 
                          sig_test = T, 
                          save_dir = ".", 
                          filename_prefix = "", 
                          fig_fmt = "svg", 
                          size = c(8, 8)) {
    # get df_meta
    df_otu = phy_obj %>% otu_table %>% as.data.frame_plus
    df_meta = phy_obj %>% sample_data %>% as.data.frame_plus %>% rownames_to_column(sample_label)

    # fix the order of df_meta
    df_meta[[group_col]]=df_meta[[group_col]]%>%fix_order


    # check marker
    marker = check_abs_rel_clr(df_otu)


    # Aggregate to phylum level
    phy_obj_phylum = phy_obj %>% aggregate_taxa_plus(level = "phylum")

    # Get Bacteroidetes and Firmicutes abundances
    df_FB_raw = phy_obj_phylum %>% abundances %>% as.data.frame_plus %>% 
      filter(rownames(.) == Bacteroidetes_marker | rownames(.) == Firmicutes_marker)

    # Calculate F/B ratio
    df_FB_ratio = df_FB_raw[Firmicutes_marker, ] / df_FB_raw[Bacteroidetes_marker, ]


    # Merge with metadata

    df_FB_ratio_longer = df_FB_ratio %>% pivot_longer_wrapper(id_col_index = 0, id_col_name = "index", names_to = sample_label, values_to = "F_B_ratio") %>% 
      select(-index) %>% 
      left_join(df_meta)

    # Prepare for plotting
    df_plot = df_FB_ratio_longer
    ind = "F_B_ratio"

    # Create plot
    pos = position_jitter(width = 0.5, seed = 1)
    fig = ggplot(df_plot, aes_string(x = group_col, y = ind, color = group_col)) +
              geom_point(position = pos) +
              geom_boxplot(outlier.shape = NA, fill = NA) +
              theme_bw()

    # Add sample labels
    if (plot_sample_label) {
      fig = fig + geom_text(aes_string(label = sample_label), position = pos)
    }

    # Add hypothesis test
    if (sig_test) {
      res_sig = df_plot %>% get_hypothesis(group_col, ind)
      fig = fig + stat_pvalue_manual(res_sig$pair_res, hide.ns = T)

      ## save stats res
      df_name = sprintf("%s_%s_%s",filename_prefix,marker,ind)
      res_sig$pair_res%>%as.data.frame_plus%>%write_df_wrap(df_name=df_name,df_dir=save_dir)
    }

    # Save plot
    fig_name = sprintf("%s_%s_%s",filename_prefix,marker,ind)
    ggsave_wrap(fig, save_dir, fig_name = fig_name, fig_fmt = fig_fmt, size = size)

    # Save table
    return(fig)
}
