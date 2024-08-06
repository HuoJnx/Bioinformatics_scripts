library(clusterProfiler)
library(enrichplot)
clusterprofiler_heatmap_rewrite = function(enrich_obj=NULL,
                                            df_enrich=NULL, 
                                            df_gene_fc,
                                            pathway_col_df_enrich = "Description", 
                                            gene_col_df_enrich = "core_enrichment", 
                                            fc_col_df_gene_fc = "log2FoldChange", 
                                            gene_ID_col_df_gene_fc = "geneID", 
                                            max_categroy_to_plot = 10,
                                            fig_size = c(24, 8)) {
    function_say("Plotting heatmap.")

    if(is.null(enrich_obj)){
        if(is.null(df_enrich)||is.null(df_gene_fc)){
            function_say("Should provide either enrich_obj or df_enrich")
        }
    }else{
        df_enrich = enrich_obj@result
    }

    
    # Convert column names to symbols for dplyr operations
    pathway_col_df_enrich_sym = rlang::sym(pathway_col_df_enrich)
    gene_col_df_enrich_sym = rlang::sym(gene_col_df_enrich)

    # Filter and select relevant data
    df_core_gene = df_enrich %>% filter(!is.na(!!pathway_col_df_enrich_sym)) %>%
                                    select(!!pathway_col_df_enrich_sym, !!gene_col_df_enrich_sym)
    
    # if more than the max, then only select Top N
    n_sig_pathway = nrow(df_core_gene)
    if(n_sig_pathway > max_categroy_to_plot){
        df_core_gene = df_core_gene %>% filter(row_number() <= max_categroy_to_plot) 
        plot_title = sprintf("Top %s significant pathways", max_categroy_to_plot)
    }else{
        plot_title = sprintf("All %s significant pathways", n_sig_pathway)
    }

    # Because the genes are stored as a string which separated by "/"
    df_core_gene = df_core_gene %>% separate_rows(!!gene_col_df_enrich_sym, sep = "/")

    # Join and reshape data
    df_core_gene_expression = df_core_gene %>% left_join(df_gene_fc, by = setNames(gene_ID_col_df_gene_fc, gene_col_df_enrich)) %>%
                                                    pivot_wider(id_cols = pathway_col_df_enrich, names_from = gene_col_df_enrich, values_from = fc_col_df_gene_fc) %>%
                                                    column_to_rownames(var = pathway_col_df_enrich) %>%
                                                    mutate_all(~ifelse(is.na(.x), 0, .x))

    # Plotting

    ggplot_heatmap = ggheatmap::ggheatmap(df_core_gene_expression, legendName = fc_col_df_gene_fc, scale = "none", border = "grey60", cluster_rows = F, cluster_cols = F) +
                            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), plot.title = element_text(color = "blue")) +
                            scale_fill_gradient2(low = "#0077BE", high = "#FF2400", mid = "white", midpoint = 0) +
                            labs(title = plot_title)

    return(ggplot_heatmap)
}