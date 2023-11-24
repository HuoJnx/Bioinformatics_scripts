
# library
library_metagenomic()
library_parallel()
library(ggpubr)


auto_alpha_boxplot=function(phy_obj,
                    group_col="group",
                    index_list=c("Shannon","InvSimpson"),
                    sig_test=T,
                    plot_sample_label=T,
                    sample_label="sample",
                    save_dir=".",
                    filename_prefix="",
                    fig_fmt="svg",
                    size=c(8,8)){

    # get data
    df_otu=otu_table(phy_obj)%>%data.frame
    df_meta=sample_data(phy_obj)%>%data.frame
    
    # fix the group col of df_meta
    df_meta[[group_col]]=df_meta[[group_col]]
    
    # check marker
    marker = check_abs_rel_clr(df_otu)

    if(marker != "REL") {
    stop("Only REL can plot this curve")
    }


    # Calculate alpha-diversity using phyloseq::estimate_richness, support c("Observed", "Chao1", "ACE", "Shannon", "Simpson", "InvSimpson", "Fisher")
    richness_df = estimate_richness(phy_obj, measures = index_list)%>%
                    rownames_to_column("sample")


    # Merge with metadata
    df_plot = df_meta %>% rownames_to_column("sample")
    df_plot = df_plot%>%left_join(richness_df, by = "sample")
    df_plot %>% write_df_wrap(df_name="All_raw_alpha_diveristy",df_dir=save_dir)


    fig_list=list()
    for(ind in index_list){
        
        ## real plot
        pos = position_jitter(width = 0.5, seed = 1)
        fig=ggplot(df_plot,aes_string(x=group_col,y=ind,color=group_col))+
                geom_point(position = pos)+
                geom_boxplot(outlier.shape=NA,fill=NA)+
                theme_bw()

        #### plot label
        if(plot_sample_label){
            fig=fig+geom_text(aes_string(label=sample_label),position=pos)
        }
        #### plot hypothesis test
        if(sig_test){
            res_sig=df_plot%>%get_hypothesis(group_col,ind)
            #fig=fig+labs(subtitle=res_sig$general_text)
            fig=fig+stat_pvalue_manual(res_sig$pair_res,hide.ns=T)
            
            ## save stats res
            df_name = sprintf("%s_%s_%s",filename_prefix,marker,ind)
            res_sig$pair_res%>%as.data.frame_plus%>%write_df_wrap(df_name=df_name,df_dir=save_dir)
        }

        
        ## save
        fig_name=sprintf("%s_%s_%s",filename_prefix,marker,ind)
        ggsave_wrap(fig,save_dir,fig_name =fig_name,fig_fmt = fig_fmt,size=size)
        fig_list[[ind]]=fig
    }
    return(fig_list)
}


auto_alpha_rarefaction_curve = function(phy_obj, 
                                group_col="group",
                                sample_size=list("from"=0,"by"=5000,"length.out"=10),
                                index_list=c("Chao1", "Observed", "ACE"),
                                save_dir=".", 
                                filename_prefix="", 
                                fig_fmt="svg", 
                                size=c(8,8)) {

    # get data
    df_otu=otu_table(phy_obj)%>%data.frame
    df_meta=sample_data(phy_obj)%>%data.frame
    
    # fix the group col of df_meta
    df_meta[[group_col]]=df_meta[[group_col]]
    df_meta=df_meta%>%select(all_of(group_col))

    # check marker
    marker = check_abs_rel_clr(df_otu)

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
        df_test = (df_plot_ind %>% publish_wilcox_test(group_col=group_col, value_col="value"))$merge
        write_df_wrap(df_test,df_name=save_name,df_dir=save_dir)

        ## plot
        fig = df_plot_ind %>% ggline(x = "Number of Sequences", y = "value", color = group_col, add = "mean_se") +
                                   labs(y = sprintf("%s index", ind))
        fig_list[[ind]] = fig
      
        ggsave_wrap(fig, save_dir, fig_name = save_name, fig_fmt = fig_fmt, size = size)



    }

    return(fig_list)
}
