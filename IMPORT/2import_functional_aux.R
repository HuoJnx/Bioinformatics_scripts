keggrest_get_kegg_pathway_intro = function(gene_ids, save_dir) {
    # Ensure the save directory exists
    if (!dir.exists(save_dir)) {
        dir.create(save_dir, recursive = TRUE)
    }
    
    for (gene in gene_ids) {
        file_path = file.path(save_dir, sprintf("%s.txt", gene))
        
        if (!file.exists(file_path)) {
            message(sprintf("Fetching %s", file_path))
            
            # Using tryCatch to handle potential errors in KEGGREST::keggGet
            tryCatch({
                ko_anno = KEGGREST::keggGet(gene)
                sink(file = file_path)
                print(ko_anno)
                sink()
            }, error = function(e) {
                message(sprintf("Error fetching %s: %s", file_path, e$message))
            })
            
        } else {
            message(sprintf("%s already exists.", file_path))
        }
    }
}

keggrest_get_KO_kegg_mapping = function(path = "/home/junsheng/BIN/junsheng/IMPORT/Functional_KEGG_pathway_desc.tsv", force = F){
    if(file.exists(path) && !force){
        print("Existed, read.")
        df_kegg = read_tsv(path)
    } else {
        if(force){
            function_say("Force update, need network to build a new one.")
        } else {
            function_say("Non existed, need network to build a new one.")
        }
        if(!requireNamespace("KEGGREST", quietly = TRUE)){
            function_say("The KEGGREST package is not installed. Please install it using install.packages('KEGGREST')",type="error")
        }
        ## real run
    df_kegg = data.frame(description = KEGGREST::keggList("pathway"))
    df_kegg = df_kegg %>% rownames_to_column("map")%>%
                            mutate(number=str_extract(map,"[0-9]+"))%>%
                            mutate(ko=paste0("ko",number))%>%
                            select(number,ko,map,description)
    }

    return(df_kegg)
}

egg_parse = function(file, need_features = c("Preferred_name", "KEGG_ko", "KEGG_Pathway", "GOs")) {

    # Supported features list
    supported_list = c("Preferred_name", "KEGG_ko", "KEGG_Pathway", "GOs")
    
    # Check each needed feature against the supported list
    for(feature in need_features) {
        if(!(feature %in% supported_list)) {
            function_say(sprintf("The %s is not supported.", feature),type="error")
        }
    }
    # Read and preprocess the raw annotation data
    df_annotation_raw = read_tsv(file, skip = 4) %>%
                        rename(query = "#query") %>%
                        filter(!str_detect(query, "#"))
    
    feature_list = list()
    
    # Check and parse for Preferred_name
    feature_list$df_annotation_raw = df_annotation_raw

    if("Preferred_name" %in% need_features) {
        df_anno_preferred_name = df_annotation_raw %>% 
                                 select(query, Preferred_name) %>%
                                 rename(gene_name = Preferred_name) %>%
                                 filter(gene_name != "-")
        feature_list$df_anno_preferred_name = df_anno_preferred_name
    }
    
    # Check and parse for KEGG_ko
    if("KEGG_ko" %in% need_features) {
        df_anno_KO = df_annotation_raw %>% 
                     select(query, KEGG_ko) %>%
                     rename(KO = KEGG_ko) %>%
                     separate_rows(KO, sep = ",") %>%
                     filter(KO != "-") %>%
                     mutate(KO = str_remove(KO, "ko:"))
        feature_list$df_anno_KO = df_anno_KO
    }
    
    # Check and parse for KEGG_Pathway
    if("KEGG_Pathway" %in% need_features) {
        df_anno_kegg_pathway = df_annotation_raw %>%
                               select(query, KEGG_Pathway) %>%
                               rename(kegg_pathway = KEGG_Pathway) %>%
                               separate_rows(kegg_pathway, sep = ",") %>%
                               filter(!str_detect(kegg_pathway, "map")) %>%
                               filter(kegg_pathway != "-")
        feature_list$df_anno_kegg_pathway = df_anno_kegg_pathway
    }
    
    # Check and parse for GOs
    if("GOs" %in% need_features) {
        df_anno_go = df_annotation_raw %>%
                     select(query, GOs) %>%
                     rename(GO = GOs) %>%
                     separate_rows(GO, sep = ",") %>%
                     filter(GO != "-")
        feature_list$df_anno_go = df_anno_go
    }
    
    return(feature_list)
}




Functional_get_COG_desc=function(path="/home/junsheng/BIN/junsheng/IMPORT/Functional_COG_desc.tsv"){
    if(file.exists(path)){
        print("Existed, read.")
        df_cog=read_tsv(path)
    }else{
        stop("Non existed.")
    }
    return(df_cog)
}