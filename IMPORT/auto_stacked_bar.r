library_metagenomic()
library(ggpubr)

auto_stacked_bar=function(phy_obj_abs,
                            level,
                            group_col=NULL,
                            top=5,
                            unclassified_value="unclassified",## this value is for sorting, keep the "unclassified_value" to the end
                            stats_test=T,
                            line_size=0,
                            line_color="black",
                            plot_x_ticks_label=F,
                            palette="npg",
                            filename_prefix="default",
                            save_dir="default",
                            fig_fmt="svg",
                            size=c(10,10)){
    ## set fig size
    set_fig_size(size[1],size[2])


    

    # check marker
    marker = check_abs_rel_clr(phy_obj_abs%>%otu_table%>%as.data.frame_plus)

    if(marker != "ABS") {
    stop("Only ABS can plot Stacked bar.")
    }

    ## phy_obj
    phy_obj_top=aggregate_top_taxa_plus(phy_obj_abs,top,level,only_keep_level_aggregate=T) #### here will create a new OTU call "Other"
    phy_obj_top_rel=phy_obj_top%>%transform_abs_to_rel
    
    ## df_otu
    df_top_abs=phy_obj_top%>%abundances%>%as.data.frame_plus
    df_top_rel=phy_obj_top_rel%>%abundances%>%as.data.frame_plus%>%mutate_all(~round(.x,4))

    ## df_meta
    df_meta=phy_obj_abs%>%sample_data%>%as.data.frame_plus(rownames_to_column=T,var="sample")

    ## df_otu_longer
    df_top_abs_longer=df_top_abs%>%pivot_longer_wrapper(id_col_index = 0,id_col_name = level,names_to = "sample",values_to = "readcounts")%>%
                                    left_join(df_meta)

    df_top_rel_longer=df_top_rel%>%pivot_longer_wrapper(id_col_index = 0,id_col_name = level,names_to = "sample",values_to = "relative_abundance")%>%
                                    left_join(df_meta)

    ## fix the order of "Other" & "unclassified"
    unique_levels = unique(df_top_abs_longer[[level]])
    other_unclassified = c("Other", unclassified_value)
    unique_levels_without_other_unclassified = setdiff(unique_levels, other_unclassified)
    final_levels = c(unique_levels_without_other_unclassified, other_unclassified)
    df_top_abs_longer[[level]] = factor(df_top_abs_longer[[level]], levels = final_levels)
    df_top_rel_longer[[level]] = factor(df_top_rel_longer[[level]], levels = final_levels)

    ## statistics test 
    con1=(!is.null(group_col))
    con2=stats_test
    if(con1&con2){

        df_test_rel=publish_wilcox_test(df_top_rel_longer,group_col,"relative_abundance",level)

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
        df_test_rel=NULL
    }

    ## fig
    fig_abs=df_top_abs_longer%>%ggbarplot(x = "sample",y="readcounts",fill = level,palette=palette,width=1,size=line_size,color=line_color)
    fig_rel=df_top_rel_longer%>%ggbarplot(x = "sample",y="relative_abundance",fill = level,palette=palette,width=1,size=line_size,color=line_color)
    
    if(!is.null(group_col)){
        form=sprintf("~%s",group_col)%>%as.formula
        fig_abs=fig_abs+facet_wrap(form,scales="free_x",nrow = 1)
        fig_rel=fig_rel+facet_wrap(form,scales="free_x",nrow = 1)
    }
    fig_abs=fig_abs+scale_y_continuous(expand = c(0, 0))
    fig_rel=fig_rel+scale_y_continuous(expand = c(0, 0))

    if(!plot_x_ticks_label){
        fig_abs=fig_abs + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x = element_blank())
        fig_rel=fig_rel + theme(axis.text.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x = element_blank())
    }

    
    ## save fig and df
    abs_name=sprintf("%s_ABS_TOP%s_%s",filename_prefix,top,level)
    rel_name=sprintf("%s_REL_TOP%s_%s",filename_prefix,top,level)
    abs_name_test=paste0("Statistics_test","_",abs_name)
    rel_name_test=paste0("Statistics_test","_",rel_name)
    
    fig_abs%>%ggsave_wrap(fig_dir = save_dir,fig_name = abs_name,fig_fmt=fig_fmt,size = size, prompt = T)
    fig_rel%>%ggsave_wrap(fig_dir = save_dir,fig_name = rel_name,fig_fmt = fig_fmt,size = size, prompt = T)
    
    write_df_wrap(df_top_abs, df_dir = save_dir, df_name = abs_name, "tsv", rowname = "#OTU ID", prompt = T)
    write_df_wrap(df_top_rel, df_dir = save_dir, df_name = rel_name, "tsv", rowname = "#OTU ID", prompt = T)
    
    if(!is.null(df_test_rel)){
     #   write_df_wrap(df_test_abs, df_dir = save_dir, df_name = abs_name_test, "tsv", prompt = T)
        write_df_wrap(df_test_rel, df_dir = save_dir, df_name = rel_name_test, "tsv", rowname = "#OTU ID", prompt = T)
    }
    return(list(fig_abs,fig_rel))
}
