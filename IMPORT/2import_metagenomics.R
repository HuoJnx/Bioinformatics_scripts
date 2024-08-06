## ---------------------------------- echo -------------------------------------
print("Importing R metagenomic related script.")

library_metagenomic=function(){
    quiet_library(phyloseq)
    quiet_library(microbiome)
    quiet_library(microViz)
}

phy_filter_taxa=phyloseq::subset_taxa

phy_transform_to_clr = function(phy_obj){
    # Check if the data is in ABS or REL format
    data_type = phy_check_abs_rel_clr(phy_obj %>% otu_table)

    # Proceed only if the data is in ABS or REL format
    if (data_type %in% c("ABS", "REL")) {
        phy_obj_clr = phy_obj %>% microbiome::transform(transform = "clr")
        function_say(sprintf("Transformed %s to CLR.",data_type))
        return(phy_obj_clr)
    } else {
        function_say("Only ABS or REL data can be transformed to CLR.", type = "stop")
    }
}

phy_transform_abs_to_rel = function(phy_obj, multiply_factor = 1e06){

    data_type = phy_check_abs_rel_clr(phy_obj %>% otu_table)
    
    if(data_type=="ABS"){
        phy_obj_rel = phy_obj%>%transform_sample_counts(function(x){x * multiply_factor/sum(x)})
        function_say(sprintf("Transform ABS to REL, multiply factor is %s.", multiply_factor))
    }else{
        function_say("Only support ABS to transform.",type="stop")
    }
    return(phy_obj_rel)
}

phy_check_abs_rel_clr=function(df_otu, round_precision=10){
    function_say("Checking data type, ABS, REL or CLR.")
    df_otu = df_otu %>% as.data.frame_plus

    if( all(df_otu >= 0) & any(df_otu > 1)){
        marker="ABS"
    }else if( all(df_otu <= 1) & all(df_otu >= 0)){
        marker="REL"
    }else if( any(df_otu < 0) & any(df_otu > 1)){
        colmeans_sum=df_otu %>% colMeans %>% sum %>% round(round_precision)
        if(colmeans_sum != 0){
            print_t="Maybe trim df_otu in CLR, or even not a CLR. Please check cautiously."
            function_say(print_t,type="warning")
        }
        marker="CLR"
    }else{
        marker="Unknown"
        function_say("Not ABS or REL or CLR! Please check df_otu!!!!!!!!!!!!",type="warning")
    }
    function_say(sprintf("Detect %s",marker))
    return(marker)
}

phy_filter_unclassified = function(phy_obj,level="species",unclassified_marker="unclassified", strict=F){
    DESCRIPTION = "This function will filter out unclassifed taxa in a specific taxa. Please
                    If strict = T, will only remove that is unclassified from Top rank to the level specified by user,
                    If strict = F, will remove that is unclassified to the level specified by user."

    ## to dataframe
    df_taxa=tax_table(phy_obj)%>%as.data.frame_plus
    if(strict){
        ## select from the first to a specific column of that level
        exp_obj=sprintf("1:%s",level)
        
        df_taxa=df_taxa%>%select(!!rlang::parse_expr(exp_obj))
        
        ## all should not be == unclassififed_marker
        #print(df_taxa%>%colnames)
        df_taxa=df_taxa%>%mutate_all(~ifelse(.x==unclassified_marker,NA, .x))
        df_taxa_no_unclassified=df_taxa%>%drop_na
    }else{
        ## the column should not be == unclassified_marker
        
        df_taxa[[level]]=ifelse(df_taxa[[level]]==unclassified_marker,NA,df_taxa[[level]])
        df_taxa_no_unclassified=df_taxa%>%drop_na(all_of(level))
    }
    keep_taxa_name=df_taxa_no_unclassified%>%rownames

    #print(remove_taxa)
    phy_obj=prune_taxa(keep_taxa_name,phy_obj)
    return(phy_obj)
}


phy_aggregate_top = function(phy_obj, top, level, other_marker="Other") {
    
    ## tackle others
    phy_obj = aggregate_taxa(phy_obj, level)

    top_taxas = top_taxa(phy_obj, top)
    df_taxa = tax_table(phy_obj)

    inds = which(!rownames(df_taxa) %in% top_taxas)

    df_taxa[inds, ] = other_marker
    
    tax_table(phy_obj) = tax_table(df_taxa)

    phy_obj=aggregate_taxa(phy_obj, level)

    return(phy_obj)
}

