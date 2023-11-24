## library
library(vegan)
library(ape)
library(pairwiseAdonis) 

plot_basic_mds=function(mds_obj,df_meta,group_col,shape_col=NULL,stat_ellipse=T,plot_sample_label=F,label_size=5,basic_text_size=15){
    ## get argvs if use this function directly
    if(is.null(shape_col)){
        shape_col=group_col
    }
    ## get info
    coord_obj=mds_obj$coord_obj
    xy_real=colnames(coord_obj)
    xylabs=mds_obj$xylabs
    ## confrim the order are right
    #### if nrow(df_meta)!=nrow(coord_obj), give warning
    n_df_meta=nrow(df_meta)
    n_coord_obj=nrow(coord_obj)
    if(n_df_meta!=n_coord_obj){
        warning(sprintf("The coord_obj has %s rows, and df_meta has %s rows, not equal!! Will shape the coord_obj by df_meta.",n_df_meta,n_coord_obj))
    }
    
    coord_obj_match_row=coord_obj%>%match_row(df_meta)

    
    plot_obj=cbind.data.frame(coord_obj_match_row,df_meta)



    fig={
    ggplot(plot_obj,mapping = aes_string(x=xy_real[1],y=xy_real[2],color=group_col,shape=shape_col))+
        geom_point()+
        labs(x=xylabs[1],y=xylabs[2])+
        theme_bw(base_size=basic_text_size)
        }
    ## if plot stat_ellipse
    if(stat_ellipse){
        fig = fig + stat_ellipse()
    }
    ## if plot sample name
    if(plot_sample_label){
        fig = fig + geom_text(aes_string(label="name"),size=label_size)
    }
    return(fig)
}

add_p_beta=function(fig,adonis2_res){

    df_adonis2_res=adonis2_res%>%data.frame(check.names = F)%>%mutate_if(is.double,~round(.x,4))
    
    p_val=df_adonis2_res[1,"Pr(>F)"]
    p_val_anno=ifelse(p_val<0.001,"P < 0.0001",sprintf("P = %s",p_val))
    fig=fig+annotate(geom = "text", x = Inf, y = Inf, hjust = 1.2, vjust = 1.2,label=p_val_anno)
    return(fig)
}

adonis_from_dist_obj=function(dist_obj,df_meta,group_col,pairwise=F,p.adjust.m="BH"){
    print("Use adonis.")
    ##confrim the order are right
    #### if nrow(df_meta)!=nrow(coord_obj), give warning
    n_df_meta=nrow(df_meta)
    n_dist_obj=length(labels(dist_obj))
    if(n_df_meta!=n_dist_obj){
        warning(sprintf("The dist_obj has %s rows, and df_meta has %s rows, not equal!! Will shape the dist_obj by df_meta.",n_dist_obj,n_df_meta))
    }
    
    dist_obj_match_row=((dist_obj%>%as.matrix)[rownames(df_meta),rownames(df_meta)])%>%as.dist
    

    form=formula(sprintf("dist_obj_match_row ~ %s",group_col))

    res = adonis2(form, data = df_meta)
    
    if(pairwise){

        group_info=df_meta[[group_col]]
        pairwise_res=pairwise.adonis(dist_obj_match_row, group_info, p.adjust.m=p.adjust.m)
    }else{
        pairwise_res=NULL
    }
    return(list(global=res,pairwise=pairwise_res))
}

anosim_from_dist_obj=function(dist_obj,df_meta,group_col, pairwise = F,p.adjust.m="BH"){
    print("Use anosim.")
    ##confrim the order are right
    #### if nrow(df_meta)!=nrow(coord_obj), give warning
    n_df_meta=nrow(df_meta)
    n_dist_obj=length(labels(dist_obj))
    if(n_df_meta!=n_dist_obj){
        warning(sprintf("The dist_obj has %s rows, and df_meta has %s rows, not equal!! Will shape the dist_obj by df_meta.",n_dist_obj,n_df_meta))
    }
    
    dist_obj_match_row=((dist_obj%>%as.matrix)[rownames(df_meta),rownames(df_meta)])%>%as.dist

    res = anosim(dist_obj_match_row, grouping = df_meta[[group_col]])
    df_res = data.frame(`Pr(>F)` = res$signif,check.names = F)
    pairwise_res= NULL
    return(list(global=df_res,pairwise=pairwise_res))
}

adonis_from_phy_obj=function(dist_obj,group_col="group",dis_method="bray", pairwise=F, p.adjust.m="BH"){

    ## extract data from phyloseq object
    df_meta=sample_data(phy_obj)%>%data.frame
    ## distance matrix
    dist_obj=distance(phy_obj,method = dis_method,type = "sample")
    res_list=adonis_from_dist_obj(dist_obj,df_meta,group_col,pairwise,p.adjust.m)
    return(res_list)
}


mds_plus=function(dis_obj,NMDS=F){
    if(NMDS){
        nmds_obj=vegan::metaMDS(dis_obj)
        coord_obj=nmds_obj$points
        colnames(coord_obj)=c("NMDS1","NMDS2")
        xylabs=colnames(coord_obj)
    }else{

        ## calculate mds
        mds_obj=cmdscale(dis_obj,eig=T)
        
        ## calculate importance
        importance_obj = data.frame(eig=mds_obj$eig[1:2])%>%
                            mutate(sum_eig=sum(mds_obj$eig))%>%
                            mutate(pro_eig=eig/sum_eig)

        importance_obj = importance_obj%>%mutate(str_pro=sprintf("%.2f%%",pro_eig*100))

        ## select 2 axis
        coord_obj=(mds_obj$points)%>%as.data.frame
        colnames(coord_obj)=c("MDS1","MDS2")
        ## format xylabs
        xylabs_raw=colnames(coord_obj)
        xylabs=paste0(xylabs_raw," [",importance_obj$str_pro, "]")
        
    }
    
    
    return(list(coord_obj=coord_obj,xylabs=xylabs))
}




auto_beta=function(phy_obj=NULL,dist_obj=NULL,df_meta=NULL,group_col="group",shape_col=NULL,plot_sample_label=F,
                    label_size=5,basic_text_size=15,distance="bray",
                    sig_test=T, test_method="adonis",stat_ellipse=T, color_vec=NULL,fix_x=NULL,fix_y=NULL,
                    directly_save=T,filename_prefix="default",save_dir="default",fig_fmt="svg",size=c(8,8)){
    
    # read phy_obj or dist_obj
    if(is.null(phy_obj)){
        print("No phy_obj, will read dist_obj and df_meta instead.")
        dist_obj=dist_obj
        df_meta=df_meta
        marker="Dist"
        distance="From_user"
    }else{
        #to check the data type
        df_otu=abundances(phy_obj)%>%data.frame
        if(all(df_otu>=0)&any(df_otu>1)){
            marker="ABS"
            pretty_print("Detect ABS.")
        }else if(all(df_otu<1) & all(df_otu>=0)){
            marker="REL"
            pretty_print("Detect REL.")
        }else if(any(df_otu<0) & any(df_otu>1)){
            marker="CLR"
            distance="euclidean"
            pretty_print("Detect CLR format, distance will change to 'euclidean'")
        }else{
            warning("Not ABS or REL or CLR! Please check df_otu!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
            marker="Unknown"
        }
        pretty_print(sprintf("Distance=%s",distance))
        # get a new variable: name for ploting label

        
        # get dist
        if(distance!="jaccard"){
            dist_obj=distance(phy_obj,method = distance, type = "sample")
        }else{
            dist_obj=distance(phy_obj,method = distance, type = "sample",binary=T)
        }

        # get df_meta
        df_meta=sample_data(phy_obj)%>%data.frame
    }

    # make a new column for label
    df_meta=df_meta%>%mutate(name=rownames(.))


    # hypothesis test
    if(sig_test){
        if(test_method=="adonis"){
            list_df=adonis_from_dist_obj(dist_obj,df_meta,group_col,pairwise=T,p.adjust.m="BH")
        }else if(test_method=="anosim"){
            list_df=anosim_from_dist_obj(dist_obj,df_meta,group_col,pairwise=T,p.adjust.m="BH")
        }else{
            stop("Only support adonis or anosim.")
        }
        df_adon_gen=list_df$global%>%data.frame(check.names = F)%>%mutate_if(is.double,~round(.x,4))
        df_adon_pair=list_df$pairwise
        
        df_name1=sprintf("%s_%s_%s_test_general",filename_prefix,marker,distance)
        
        if(directly_save){
            write_df_wrap(df_adon_gen,df_dir = save_dir,df_name = df_name1,"tsv","rowname",prompt = T)
        }
        if(!is.null(df_adon_pair)){
            df_adon_pair=df_adon_pair%>%mutate_if(is.double,~round(.x,4))
            df_name2=sprintf("%s_%s_%s_test_pair",filename_prefix,marker,distance)
            if(directly_save){
                write_df_wrap(df_adon_pair,df_dir = save_dir,df_name = df_name2,"tsv","rowname",prompt = T)
            }
        }
    }
    
    # calculate ordination
    
    mds_obj=mds_plus(dist_obj,NMDS=F)
    nmds_obj=mds_plus(dist_obj,NMDS=T)
    
    
    # plot ordination
    ## basic plot
    basic_mds_fig=plot_basic_mds(mds_obj,df_meta,group_col,shape_col,stat_ellipse,plot_sample_label,label_size,basic_text_size)
    basic_nmds_fig=plot_basic_mds(nmds_obj,df_meta,group_col,shape_col,stat_ellipse,plot_sample_label,label_size,basic_text_size)
    
    ## if plot statistics
    if(sig_test){
        fig_mds=basic_mds_fig%>%add_p_beta(list_df$global)
        fig_nmds=basic_nmds_fig%>%add_p_beta(list_df$global)
        
    }else{
        fig_mds=basic_mds_fig
        fig_nmds=basic_nmds_fig
    }
    
    ## plot color
    if(!is.null(color_vec)){
        fig_mds=fig_mds+scale_color_manual(values = color_vec)
        fig_nmds=fig_nmds+scale_color_manual(values = color_vec)
    }
    
    ## fix xy axis
    if(!(is.null(fix_x)&is.null(fix_y))){
        fig_mds=fig_mds+xlim(fix_x)+ylim(fix_y)
        fig_nmds=fig_mds+xlim(fix_x)+ylim(fix_y)
    }
    # save
    ## name
    figname_mds=sprintf("%s_%s_%s_MDS",filename_prefix,marker,distance)
    figname_nmds=sprintf("%s_%s_%s_NMDS",filename_prefix,marker,distance)

    ## real save
    if(directly_save){
        fig_mds%>%ggsave_wrap(save_dir,figname_mds,fig_fmt,size,prompt = T)
        fig_nmds%>%ggsave_wrap(save_dir,figname_nmds,fig_fmt,size,prompt = T)
    }

    ## return
    return(list(dist_obj=dist_obj,MDS=fig_mds,NMDS=fig_nmds,figname=c(figname_mds,figname_nmds),save_dir=save_dir))
}