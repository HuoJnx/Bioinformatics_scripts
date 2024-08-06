library_metagenomic()
library(ggpubr)

auto_stacked_bar=function(phy_obj_abs,
                            level,
                            group_col=NULL,
                            top=5,
                            unclassified_value="unclassified",## this value is for 1. sorting, keep the "unclassified_value" to the end; 2. Or remove the "unclassified"
                            remove_unclassified=F,
                            stats_test=F,
                            alpha = 0.8,
                            line_size=0,
                            basic_font_size=10,
                            line_color="black",
                            plot_x_ticks_label=F,
                            vertical_x_label=T,
                            palette="npg",
                            filename_prefix="default",
                            save_dir="default",
                            fig_fmt="svg",
                            size=c(10,10)){
    ## set fig size
    set_fig_size(size[1],size[2])


    

    # check marker
    marker = phy_check_abs_rel_clr(phy_obj_abs %>% otu_table %>% as.data.frame_plus)

    if(marker != "ABS") {
    stop("Only ABS can plot Stacked bar.")
    }

    # Whether to remove the "unclassified"?
    print(sprintf("The unclassified value you specified is '%s', all this value among the whole taxa table will be treated as unclassified.",unclassified_value))
    level_sym = level %>% as.name

    if(remove_unclassified){
        print(sprintf("Will remove the unclassified taxa in %s level",level))
        phy_obj_abs = phy_obj_abs %>% phy_filter_unclassified(level = level, unclassified_marker = unclassified_value, strict = F)
    }
    
    ## Aggregate to TOP
    phy_obj_top = phy_aggregate_top(phy_obj_abs, top, level) #### here will create a new OTU call "Other"


    ## get the relative 
    phy_obj_top_rel = phy_obj_top %>% phy_transform_abs_to_rel(multiply_factor = 100)

    ## df_otu
    df_top_abs = phy_obj_top %>% abundances %>% as.data.frame_plus
    df_top_rel = phy_obj_top_rel %>% abundances %>% as.data.frame_plus %>% mutate_all(~round(.x,4))

    ## df_meta
    df_meta = try(sample_data(phy_obj_abs)%>%
                        as.data.frame_plus(make_rownames_to_column=T,var="sample"), 
                        silent = TRUE)

    if ("try-error" %in% class(df_meta)) {
        function_say("Can't get df_meta, make one.",type="warning")
        df_meta = data.frame(sample = colnames(otu_table(phy_obj_abs)))
    }


    ## df_otu_longer
    df_top_abs_longer=df_top_abs%>%pivot_longer_wrapper(id_col_index = 0,id_col_name = level,names_to = "sample",values_to = "readcounts")%>%
                                    left_join(df_meta)

    df_top_rel_longer=df_top_rel%>%pivot_longer_wrapper(id_col_index = 0,id_col_name = level,names_to = "sample",values_to = "relative_abundance")%>%
                                    left_join(df_meta)



    ## fix the order of "Other" & "unclassified"
    unique_levels = unique(df_top_abs_longer[[level]])
    if(remove_unclassified){
        other_unclassified = c("Other")
    }else{
        other_unclassified = c("Other", unclassified_value)
    }
    
    unique_levels_without_other_unclassified = setdiff(unique_levels, other_unclassified)

    final_levels = c(unique_levels_without_other_unclassified, other_unclassified)


    df_top_abs_longer[[level]] = factor(df_top_abs_longer[[level]], levels = final_levels)
    df_top_rel_longer[[level]] = factor(df_top_rel_longer[[level]], levels = final_levels)

    ## statistics test 
    con1=(!is.null(group_col))
    con2=stats_test
    if(con1&con2){
        df_test_rel_wilcox=publish_wilcox_test(df_top_rel_longer,group_col,"relative_abundance",level) %>% .$merge
        df_test_rel_t=publish_t_test(df_top_rel_longer,group_col,"relative_abundance",level) %>% .$merge
    #    test_abs_list=(
    #                    phy_obj_top%>%ancombc2(fix_formula=group_col,
    #                                                assay_name="count",global=F,pairwise=T,dunnet=F,trend=F,
    #                                                group=group_col, struc_zero=T, neg_lb=F, 
    #                                                p_adj_method="fdr",
    #                                                pseudo=0,pseudo_sens=F,
    #                                                prv_cut=0,lib_cut=0,
    #                                                n_cl=1,verbose=F)
    #                )
    #    if(is.null(test_abs_list$res_pair)){
    #        df_test_abs=test_abs_list$res
    #    }else{
    #        df_test_abs=test_abs_list$res_pair
    #    }
    }else{
    #    df_test_abs=NULL
        df_test_rel_wilcox=NULL
        df_test_rel_t=NULL
    }

    ## fig
    fig_abs=df_top_abs_longer%>%ggbarplot(x = "sample",y="readcounts",alpha = alpha, fill = level,palette=palette,width=1,size=line_size,color=line_color)
                                    labs(y="Readcounts")
    fig_rel=df_top_rel_longer%>%ggbarplot(x = "sample",y="relative_abundance",alpha = alpha, fill = level,palette=palette,width=1,size=line_size,color=line_color)+
                                    labs(y="Relative abundance")
    
#    if(!is.null(group_col)){
#        form=sprintf("~%s",group_col)%>%as.formula
#        fig_abs=fig_abs+facet_wrap(form,scales="free_x",nrow = 1)
#        fig_rel=fig_rel+facet_wrap(form,scales="free_x",nrow = 1)
#    }
    fig_abs = fig_abs + scale_y_continuous(expand = c(0, 0))
    fig_rel = fig_rel + scale_y_continuous(expand = c(0, 0))

    # theme
    fig_abs=fig_abs + theme_transparent2(base_size = basic_font_size, base_family = "arial") + theme(legend.position = "top")
    fig_rel=fig_rel + theme_transparent2(base_size = basic_font_size, base_family = "arial") + theme(legend.position = "top")

    if(!plot_x_ticks_label){
        fig_abs=fig_abs + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x = element_blank())
        fig_rel=fig_rel + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x = element_blank())
    }

    if(vertical_x_label){
        fig_abs = fig_abs + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
        fig_rel = fig_rel + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    }

    ## save fig and df
    abs_name=sprintf("%s_ABS_TOP%s_%s",filename_prefix,top,level)
    rel_name=sprintf("%s_REL_TOP%s_%s",filename_prefix,top,level)

    rel_name_test_wilcox=paste0("Statistics_test_wilcox","_",rel_name)
    rel_name_test_t=paste0("Statistics_test_t","_",rel_name)
    
    fig_abs%>%ggsave_wrap(file_name = abs_name,save_dir = save_dir,file_fmt=fig_fmt,size = size, prompt = T)
    fig_rel%>%ggsave_wrap(file_name = rel_name,save_dir = save_dir,file_fmt = fig_fmt,size = size, prompt = T)
    
    write_df_wrap(df_top_abs, file_name = abs_name, save_dir = save_dir,  "tsv", rowname = "#OTU ID", prompt = T)
    write_df_wrap(df_top_rel, file_name = rel_name, save_dir = save_dir, "tsv", rowname = "#OTU ID", prompt = T)
    
    if(!is.null(df_test_rel_wilcox)){
     #   write_df_wrap(df_test_abs, df_dir = save_dir, df_name = abs_name_test, "tsv", prompt = T)
        write_df_wrap(df_test_rel_wilcox, file_name = rel_name_test_wilcox, save_dir = save_dir,  "xlsx", rowname = "#OTU ID", prompt = T)
        write_df_wrap(df_test_rel_t, file_name = rel_name_test_t, save_dir = save_dir,  "xlsx", rowname = "#OTU ID", prompt = T)
    }
    return(list(fig_abs,fig_rel))
}
