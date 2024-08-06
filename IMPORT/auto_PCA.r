library(PCAtools)
library(ggrepel)
auto_PCA = function(.data, feature_are_row, df_col_meta=NULL, scale = T, center = T, plot_screep_comp_lims = 10,axis_for_plotting = c(1,2),
                        color_col = NULL, shape_col = NULL, point_size =3, plot_sample_label = T, sample_label_col_general = "sample", label_repel = T, label_size = 5, basic_text_size = 18, stat_ellipse = T, 
                        plot_centroid = F, group_vec_calculating_centroid = NULL, sample_label_col_centroid = NULL, 
                        plot_arrow_in_centroid = F, group_vec_calculating_arrow = NULL, arrow_type = "closed", arrow_length = 0.1
                        ){
    ## set theme
    theme_set(theme_gray(base_size = basic_text_size))
    
    ## if not feature are row
    if(feature_are_row){
        .data = .data %>% t_df
    }
    
    ## fetch the sample name for sample_label annotation
    if(is.null(df_col_meta)){
        df_col_meta = data.frame("sample" = rownames(.data)) %>%
                        set_rownames(rownames(.data))
    }else{
        df_col_meta = df_col_meta %>% mutate("sample" = rownames(df_col_meta))
    }
    ## get PCA object
    p_obj = prcomp(x = .data, center = center, scale. = scale)

    ## get the cooridination
    df_plot = p_obj[["x"]] %>% as.data.frame_plus %>% .[,axis_for_plotting] %>% left_join_index(df_col_meta)
    
    ## screep plot
    var_explained = p_obj[["sdev"]] ^2 /sum(p_obj[["sdev"]] ^2) * 100
    df_var_explained = data.frame(Component = seq_along(var_explained), Comp_var = var_explained) %>%
                        mutate(Sum_var = cumsum(Comp_var))
    if(nrow(df_var_explained) > plot_screep_comp_lims){
        df_var_explained = df_var_explained[1:plot_screep_comp_lims, ]
    }

    x_ticks = df_var_explained$Component
    y_ticks = seq(from = 0,to = 100, by = 10)
    fig_screep = ggplot(df_var_explained) + 
                    geom_bar(mapping = aes(x= Component, y = Comp_var), stat = "identity", fill = "#19C4C9") +
                    geom_line(mapping = aes(x = Component, y = Sum_var), color = "#C61C1C", linewidth = 2) +
                    scale_x_continuous(breaks = x_ticks)+
                    scale_y_continuous(breaks = y_ticks)+
                    labs(x = "Principal Component", y = "Proportion of Variance Explained (%)", title = "Scree Plot")+
                    theme_grey(base_size = 22)+
                    theme(plot.title = element_text(colour = "blue"))

    ## get_x_y_labs
    raw_xy_labs = p_obj[["x"]] %>% colnames %>% .[axis_for_plotting]
    var_perc = sprintf("%s %%", round(var_explained, 3)) %>% .[axis_for_plotting]
    final_xy_labs = sprintf("%s [%s]",raw_xy_labs,var_perc) 
    
    ## plot general
    first_axis = raw_xy_labs[1]
    second_axis = raw_xy_labs[2]
    first_axis_name = first_axis %>% as.name
    second_axis_name = second_axis %>% as.name

    
    fig_general = ggplot(df_plot, aes_string(x = first_axis, y = second_axis, color = color_col, shape = shape_col)) +
                    geom_point(size = point_size) +
                    labs(x = final_xy_labs[1], y = final_xy_labs[2]) + 
                    tune::coord_obs_pred(ratio = 1)
    
    if(plot_sample_label){
        if(label_repel){
            fig_general = fig_general + geom_text_repel(mapping = aes_string(label = sample_label_col_general), max.overlaps = Inf, size = label_size) 
        }else{
            fig_general = fig_general + geom_text(mapping = aes_string(label = sample_label_col_general), size = label_size)
        }
        
    }

    if(stat_ellipse){
        fig_general = fig_general + stat_ellipse()
    }
    
    x_y_lims = get_ggplot_limits(fig_general)
    x_limits = x_y_lims[["xlim"]]
    y_limits = x_y_lims[["ylim"]]
    
    ## plot centroids

    if(plot_centroid){
        if(is.null(group_vec_calculating_centroid)){
            function_say("Must provide group_vec_calculating_centroid, otherwise the calculation will failed.", type = "error")
        }
        first_centroids = sprintf("%s_centroids", first_axis)
        second_centroids = sprintf("%s_centroids", second_axis)
        first_centroids_name = first_centroids %>% as.name
        second_centroids_name = second_centroids %>% as.name
        
        df_plot_centroids = df_plot %>% group_by(across(all_of(group_vec_calculating_centroid))) %>% 
                                            summarise(!!first_centroids_name := mean(!!first_axis_name), !!second_centroids_name := mean(!!second_axis_name), .groups = "drop")
                                            
        
        fig_centroids = ggplot(df_plot_centroids, aes_string(x = first_centroids, y = second_centroids, color = color_col, shape = shape_col)) +
                        geom_point(size = point_size) + geom_text_repel(mapping = aes_string(label = sample_label_col_centroid), max.overlaps = Inf, size = label_size) + 
                        tune::coord_obs_pred(ratio = 1) +
                        labs(x = final_xy_labs[1], y =final_xy_labs[2]) + 
                        lims(x=x_limits, y = y_limits)
        
        if(plot_arrow_in_centroid){
            if(is.null(group_vec_calculating_arrow)){
                function_say("Must provide group_vec_calculating_arrow, otherwise the calculated result will be incorrected.", type = "error")
            }
            df_plot_centroids_arrow = df_plot_centroids %>% group_by(across(all_of(group_vec_calculating_arrow))) %>%
                                                    mutate(x_end = lead(!!first_centroids_name), y_end = lead(!!second_centroids_name)) %>%
                                                    filter(!is.na(x_end))
            fig_centroids_arrow = fig_centroids + geom_segment(aes_string(x = first_centroids, y = second_centroids, xend = "x_end", yend = "y_end", color = color_col), 
                                                               data = df_plot_centroids_arrow, arrow = arrow(type = arrow_type, length = unit(arrow_length,"inches")))
        }else{
            fig_centroids_arrow = NULL
        }
    }else{
        df_plot_centroids = NULL
        fig_centroids = NULL
        fig_centroids_arrow = NULL
   }
    return(list("df_plot" = df_plot, "df_plot_centroids" = df_plot_centroids, "fig_screep" = fig_screep, "fig_general" = fig_general, "fig_centroids" = fig_centroids, "fig_centroids_arrow" = fig_centroids_arrow))
    
}