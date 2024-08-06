
# library
library_metagenomic()
library_parallel()
library(ggpubr)


auto_alpha_boxplot = function(phy_obj,
                    group_col = NULL,
                    index_list = c("Shannon", "InvSimpson"),
                    plotting_style = "color",
                    sig_test = T, 
                    p_type="p",
                    sig_type=".signif",
                    sig_thresh=0.05,
                    hide_ns_for_plot=F,
                    verbose_test_res_for_plot=T,
                    plot_sample_label = T,
                    sample_label = "sample", 
                    basic_font_size=20, 
                    save_dir = ".", 
                    filename_prefix = "",
                    fig_fmt = "svg",
                    size = c(8, 8)) {

    # get data
    df_otu = otu_table(phy_obj) %>% data.frame()
    df_meta = sample_data(phy_obj) %>% data.frame()
    
    # Check if group_col is NULL
    if (is.null(group_col)) {
        sig_test = F
        df_meta$group = "All_Samples"
        group_col = "group"
    }
    
    # Check marker
    marker = phy_check_abs_rel_clr(df_otu)
    if (marker != "REL") {
        stop("Only REL can plot boxplot.")
    }

    # Calculate alpha-diversity
    richness_df = estimate_richness(phy_obj, measures = index_list) %>%
                    rownames_to_column("sample")

    # Merge with metadata
    df_plot = df_meta %>% rownames_to_column("sample") %>%
              left_join(richness_df, by = "sample")
    write_df_wrap(df_plot, file_name="All_raw_alpha_value_for_plotting", save_dir=save_dir,file_fmt="xlsx")

    fig_list = list()
    for (ind in index_list) {
        function_say(sprintf("Plotting figures and getting statistics for %s.",ind))
        # Call ggbox_points_pairwise_plus function
        fig_table = ggbox_points_pairwise_plus(df_plot=df_plot,
                                                      value_col=ind,
                                                      group_col=group_col,
                                                      plotting_style = plotting_style,
                                                      sig_test=sig_test,
                                                      general_stat_method="kruskal",
                                                      pairwise_stat_method = "wilcox", 
                                                      p_type = p_type, 
                                                      sig_type = sig_type, 
                                                      sig_thresh = sig_thresh, 
                                                      hide_ns_for_plot = hide_ns_for_plot, 
                                                      plot_sample_label = plot_sample_label,
                                                      sample_label = sample_label,
                                                      verbose_test_res_for_plot = verbose_test_res_for_plot, 
                                                      basic_font_size = basic_font_size
          )

        # Extract the figure and test results
        fig = fig_table$figure
        df_test = fig_table$test_results



        # save fig and df
        df_name = sprintf("%s_%s_%s",filename_prefix,marker,ind)
        fig_name = sprintf("%s_%s_%s",filename_prefix,marker,ind)

        # Optionally save test results
        if (sig_test && !is.null(df_test)) {
            # Perform additional tests in different algorithm for validation
            df_test_t = publish_t_test(df_plot, group_col = group_col, value_col = ind) %>% .$merge
            df_test_wilcox = publish_wilcox_test(df_plot, group_col = group_col, value_col = ind) %>% .$merge

            write_df_wrap(df_test_t, file_name=paste0(df_name,"_","validation_T"), save_dir = save_dir, file_fmt = "xlsx")
            write_df_wrap(df_test_wilcox, file_name=paste0(df_name,"_","validation_WILCOX"), save_dir = save_dir, file_fmt = "xlsx")
            write_df_wrap(df_test, file_name=df_name, save_dir=save_dir,file_fmt="xlsx")
            
        }

        # Save the figure
        ggsave_wrap(fig, file_name = fig_name, save_dir=save_dir, file_fmt = fig_fmt, size = size)

        # Store the figure in the list
        fig_list[[ind]] = fig

    }
    return(fig_list)
}



auto_alpha_rarefaction_curve = function(phy_obj, 
                                group_col=NULL,
                                sample_size=list("from"=0,"by"=5000,"length.out"=10),
                                index_list=c("Chao1", "Observed", "ACE"),
                                sig_test=T,
                                test_func=publish_wilcox_test,
                                basic_font_size = 12,
                                save_dir=".", 
                                filename_prefix="", 
                                fig_fmt="svg", 
                                size=c(8,8)) {
    # set fig size
    set_fig_size(size_obj=size)

    # get data
    df_otu=otu_table(phy_obj)%>%data.frame
    df_meta=sample_data(phy_obj)%>%data.frame
    
    # Check if group_col is NULL, if so, treat all samples as a single group
    if (is.null(group_col)) {
        sig_test = F
        df_meta$group = "All_Samples"
        group_col = "group"
    } else {
        # Fix the group col of df_meta
        #df_meta[[group_col]] = df_meta[[group_col]]
        df_meta=df_meta%>%select(all_of(group_col))
    }

    

    # check marker
    marker = phy_check_abs_rel_clr(df_otu)

    if(marker != "ABS") {
    stop("Only ABS can plot this curve")
    }

    # get along seq
    along_seq = seq(from = sample_size[["from"]], by = sample_size[["by"]], length.out = sample_size[["length.out"]]) %>% round(0)
    along_seq = ifelse(along_seq == 0, 1, along_seq)


    # calculate richness
    df_richness = foreach(select_sample_size = along_seq, .combine = "rbind") %dopar% {
                        rarefied_data = rarefy_even_depth(phy_obj, rngseed = 1024, sample.size = select_sample_size)
                        df_richness_temp = estimate_richness(rarefied_data, measures = index_list) %>%
                                                mutate(`Number of Sequences` = select_sample_size) %>%
                                                rownames_to_column("sample") %>%
                                                left_join(df_meta %>% rownames_to_column("sample")) %>%
                                                select(-starts_with("se."))
                        return(df_richness_temp)
    }

    df_plot = df_richness %>% pivot_longer(cols = index_list) %>%
                                drop_na #ACE will creates some NAs when n_seq = 1

    fig_list = list()
    for(ind in index_list) {
        df_plot_ind = df_plot %>% filter(name == ind)
        save_name = sprintf("%s_%s_%s", filename_prefix, marker, ind)

        ## hypothesis test
        if(sig_test){
            df_test = (df_plot_ind %>% test_func(group_col=group_col, value_col="value"))$merge
            write_df_wrap(df_test,file_name=save_name,save_dir=save_dir,file_fmt="xlsx")
        }
        ## plot
        fig = df_plot_ind %>% ggline(x = "Number of Sequences", y = "value", color = group_col, add = "mean_se") + 
                                    theme_bw() +
                                    theme(text = element_text(size = basic_font_size),axis.text.x = element_text(angle = 90)) +
                                    labs(y = sprintf("%s index", ind))
        fig_list[[ind]] = fig
        
        ggsave_wrap(fig, file_name = save_name, save_dir=save_dir, file_fmt = fig_fmt, size = size)



    }

    return(fig_list)
}
