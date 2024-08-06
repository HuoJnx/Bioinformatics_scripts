## ---------------------------------- echo -------------------------------------
print("Importing ggplot2 aux script.")


quiet_library(paletteer)

frequently_used_palette=function(){
    pal2 = c("Red" = "#B7444F","Blue" = "#4E688F")
    pal3 = c("Red" = "#DB9F99", "Yellow" = "#EEC975", "Blue" = "#9BABC8")
    pal_avocado = c("fill" = "#93B779", "color" = "#75A753", "fill_alpha" = 0.2 )
}

theme_transparent = ggpubr::theme_transparent
theme_transparent2 = function (base_size = 12, base_family = "") {
                        theme_classic(base_size = base_size, base_family = base_family) + 
                            theme(panel.background = element_rect(fill = "transparent", 
                                colour = NA), plot.background = element_rect(fill = "transparent", 
                                colour = NA), legend.background = element_rect(fill = "transparent", 
                                colour = NA), legend.box.background = element_rect(fill = "transparent", 
                                colour = NA)
                                )
                    }

theme_set(theme_transparent2(base_size = 8, base_family = "arial"))
#theme_set(theme_bw(base_size = 18))

## plus
format_scientific_plus = function(num, num_decimals) {
  # Calculate the threshold based on the number of decimal places
  threshold = 10^(-num_decimals)

  # Use ifelse() instead of if() to handle vectors
  ifelse(num <= threshold, format(num, scientific = T), round(num, num_decimals))
}


ggsave_wrap=function(fig, file_name, save_dir=".", file_fmt="svg", size=c(8,8), prompt=T){
    function_say("Saving ggplot figure.")
    ## if no fig input, skip saving

    if(is.null(fig)){
        function_say(sprintf("The figure with expected name '%s' is acntually NULL, skip saving.", file_name), type = "warning")
        return()
    }

    ## build dir and fig_path
    dir.create(save_dir,recursive=T,showWarnings=F)
    file_name=paste0(file_name,".",file_fmt)
    file_name = fs::path_sanitize(file_name)
    fig_path=file.path(save_dir,file_name)
    if(file_fmt != "pptx"){
        ggsave(fig_path,fig,width=size[1],height=size[2])
    }else{
        export::graph2ppt(fig, file=fig_path, width=size[1], height=size[2]) 
    }
    
    if(prompt){
        tt=sprintf("Successfully save figure to %s.",fig_path)
        function_say(tt)
    }
}

get_ggplot_limits = function(fig){
    # get built object
    plot_build = ggplot_build(fig)

    # Extract x and y limits
    x_limits = plot_build$layout$panel_params[[1]]$x.range
    y_limits = plot_build$layout$panel_params[[1]]$y.range
    
    return(list(xlim = x_limits, ylim = y_limits))
}

close_all_device = function() {
  while (dev.cur() > 1) dev.off()
}

get_hypothesis_for_plot = function(data, group_col, value_col,
    general_m = "kruskal", pair_m = "wilcox", adj_m = "fdr", add_xy_function="max",detail_text=T) {
    quiet_library(rstatix)
    # prompt
    function_say(sprintf("Hello, group_col is %s, value_col is %s.",group_col,value_col))
    # check 

    if(!inherits(data,"data.frame")){
        data = data%>%as.data.frame_plus
    }
    #  conduct hypothesis test 
    
    ## general test
    form = formula(paste(value_col, "~", group_col))
    if (general_m == "kruskal") {
        df_general_res = data %>% kruskal_test(form)
    }else if (general_m == "anova"){
        df_general_res = data %>% anova_test(form)
    }
    
    ## pair test
    if (pair_m == "wilcox") {
        pair_res = data %>% wilcox_test(form, p.adjust.method = "none")
    } else if (pair_m == 't.test') {
        pair_res = data %>% t_test(form,p.adjust.method = "none")
    }
    pair_res=pair_res%>%adjust_pvalue(method="fdr") %>%
                            add_significance("p.adj",output.col="p.adj.signif") %>%
                            add_significance("p",output.col="p.signif")

    # get xy to plot 
    pair_res = pair_res %>%add_xy_position(x = group_col,fun=add_xy_function)%>%data.frame


    # ------------------------------------ return ------------------------------------
    general_text = get_test_label(df_general_res, detailed = detail_text)

    return(list(df_general_res=df_general_res,pair_res=pair_res,general_text=general_text))
}

adjust_group_col = function(data, group_col = NULL) {
    if (is.null(group_col)) {
        group_value = "All_samples"
        data[["group_AUTO"]] = "All_samples"
        group_col = "group_AUTO"
        function_say(sprintf("》》No group_col, will mix all samples together to plot. Assign %s as group_col and %s as values.",group_col,group_value),type="warning")
    } else {
        # Ensure that the specified group_col exists in the dataframe
        if (!group_col %in% names(data)) {
            function_say(sprintf("》》The specified group column '%s' does not exist in the dataframe.", group_col),type="error")
        }
    }
    return(list(data = data, group_col = group_col))
}

ggheatmap_plus = function(data_matrix, id_col_index = 0, 
                          id_col_name = "rownames", names_to = "colnames", values_to = "values", 
                          scale = "none", center = F, 
                          fill_func = scale_fill_gradient2, fill_func_args = list(low = "#2B5C8A", mid = "white", high = "#9E3D22", midpoint = 0),
                          plot_number = F, number_format = "%.2f", number_size = 4, number_color = "black",
                          transpose_xy = F,
                          basic_theme = theme_minimal, basic_theme_argv = list(base_size = 18)) {

    # move the id_col to rownames
    if(id_col_index!=0){
        if(is.numeric(id_col_index)){
            rowname = colnames(data_matrix)[id_col_index]
            data_matrix = data_matrix %>% column_to_rownames(rowname)
        }else{
            data_matrix = data_matrix %>% column_to_rownames(id_col_index)
            id_col_name = id_col_index
        }
    }
    # save the order of rownames and colnames
    order_row = data_matrix %>% rownames
    order_col = data_matrix %>% colnames

    if (scale == "row") {
        data_matrix_scale = data_matrix %>% scale_by_row(center = center, scale = T)
    } else if (scale == "column") {
        data_matrix_scale = data_matrix %>% scale_by_col(center = center, scale = T)
    } else if (scale == "none") {
        data_matrix_scale = data_matrix
    }else {
        stop("scale receive 'row', 'column' or 'none'.")
    }

    # Transform wider format to longer format
    data_matrix_scale_longer = data_matrix_scale %>% 
                                pivot_longer_wrapper(id_col_index = 0, id_col_name = id_col_name, names_to = names_to, values_to = values_to) 
   
    # keep the original row, col order

    if(!transpose_xy){
        data_matrix_scale_longer = data_matrix_scale_longer %>%
                                        mutate(!!sym(id_col_name) := factor(!!sym(id_col_name), levels = rev(order_row))) %>%
                                        mutate(!!sym(names_to) := factor(!!sym(names_to), levels = order_col))
    }else{
        data_matrix_scale_longer = data_matrix_scale_longer %>%
                                        mutate(!!sym(id_col_name) := factor(!!sym(id_col_name), levels = order_row)) %>%
                                        mutate(!!sym(names_to) := factor(!!sym(names_to), levels = rev(order_col)))
    }

 
    # Plotting the heatmap
    fig = ggplot(data_matrix_scale_longer, aes(x = !!sym(names_to), y = !!sym(id_col_name), fill = !!sym(values_to))) +
            geom_tile() + 
            do.call(basic_theme, args = basic_theme_argv) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            do.call(fill_func, args = fill_func_args)

    if(plot_number){
        fig = fig + geom_text(aes(label = ifelse(is.na(!!sym(values_to)),"", sprintf(number_format, !!sym(values_to)))), size = number_size, color= number_color)
    }
    # transpose
    if(transpose_xy){
        fig = fig + coord_flip()
    }

    return(fig)
}

ggbox_points_pairwise_plus = function(df_plot, value_col, group_col = NULL, plotting_style = c("color","fill"), sig_test = T, general_stat_method = "kruskal", 
                                pairwise_stat_method = "wilcox", p_type = c("p","p.adj"), sig_type = c("",".sig"), 
                                sig_thresh = 0.05, hide_ns_for_plot = T, 
                                plot_sample_label = F, sample_label = "sample",
                                verbose_test_res_for_plot = T, basic_font_size = 12) {

    ## check if group_col existed
    adjusted_group_col_res=adjust_group_col(data=df_plot,group_col=group_col)
    df_plot = adjusted_group_col_res$data
    group_col = adjusted_group_col_res$group_col


    ## plot
    pos = position_jitter(width = 0.5, seed = 1)
    if(length(plotting_style) == 2) {
        plotting_style = plotting_style[1]
    }
    if(plotting_style=="color"){ # Don't use global aes, otherwise it will raise error in stat_manual_pvalue
        fig = ggplot(df_plot, aes_string(x = group_col, y = value_col)) +
                    geom_boxplot(outlier.shape = NA, aes_string(color = group_col), fill = NA) +
                    geom_point(position = pos, aes_string(color = group_col))
                    

    }else if(plotting_style=="fill"){
        fig = ggplot(df_plot, aes_string(x = group_col, y = value_col)) +
                    geom_boxplot(outlier.shape = NA, aes_string(fill = group_col) , color = "black") +
                    geom_point(position = pos, color = "black")
                    
    }else{
        function_say("Plotting style can be either 'color' or 'fill'.",type="error")
    }


    fig = fig + theme_transparent2() +
                  theme(text = element_text(size = basic_font_size)) +
                  labs(y = value_col)

    if(plot_sample_label){
        if(plotting_style=="color"){
            fig = fig + geom_text(aes_string(color=group_col,label=sample_label),position = pos)
        }else if(plotting_style=="fill"){
            fig = fig + geom_text(aes_string(label=sample_label),position = pos,color="black")
        }
        
    }

    test_results = NULL ## prevent giving error when sig_test = F.
    if (sig_test) {
        res_sig_plot = df_plot %>% get_hypothesis_for_plot(group_col = group_col, value_col = value_col, 
                                                           general_m = general_stat_method, pair_m = pairwise_stat_method) 
        if(length(p_type) == 2) {
            p_type = p_type[1]
        }
        if(length(sig_type) == 2) {
            sig_type = sig_type[1]
        }
        label_type = paste0(p_type, sig_type)
        label_type_sym = as.name(label_type)

        if(hide_ns_for_plot) {
            plot_pair_res = res_sig_plot$pair_res %>%
                                filter(!!label_type_sym <= sig_thresh)
        } else {
            plot_pair_res = res_sig_plot$pair_res
        }

        if(verbose_test_res_for_plot) {
            function_say("Print the test results for check.")
            print(res_sig_plot)
        }
        fig = fig + stat_pvalue_manual(plot_pair_res, label = label_type, color = "black")
        print(fig)

        test_results = df_plot %>% publish_wilcox_test(group_col = group_col, value_col = value_col) %>%
                                    .$merge
    }


    results = list("figure" = fig, "test_results" = test_results)
    return(results)
}

ggfactor_proportion = function(df, col_you_want_as_x, col_you_want_as_legend,mode=c("stack","mode")) {
    # set mode
    if (length(mode)==2){
        mode=mode[1]
        print(sprintf("Use %s mode.",mode))
    }
    # Ensure the condition column is a factor
    df[[col_you_want_as_legend]] = as.factor(df[[col_you_want_as_legend]])
    
    
    # Calculate the proportion for each species
    group_by_vec=c(col_you_want_as_x, col_you_want_as_legend)
    data_summary = df %>% group_by(across(all_of(group_by_vec))) %>%
                    summarise(count = n()) %>%
                    mutate(proportion = count / sum(count))
    # Plot the bar plot
    plot = ggplot(data_summary, aes_string(x = col_you_want_as_x, y = "proportion", fill = col_you_want_as_legend)) +
            geom_bar(stat = "identity", position = mode)

    return(plot)
}

ggbutterfly <- function(data, x, y, fill, facet_func = NULL, facet_argv = list(), negative_group = NULL, symmetry_ylim = TRUE, bar_alpha = 0.9, 
                        show_numbers = TRUE, number_color = NULL, number_size = 5, number_hvjust_factor = 1.5, y_limits_expand_factor = 1.4,
                        flip_xy_axis = TRUE, reverse_x_axis = TRUE, x_breaks = NULL,
                        basic_theme = theme_gray, theme_argv = list(base_size = 18)) {
    # check if negative values in data
    if (any(data[[y]] < 0)) {
      stop("Plotting negative values is not allowed.")
    }
    # Set negative_group to the first value of the fill column if it is NULL
    if (is.null(negative_group)) {
        negative_group <- unique(data[[fill]])[1]
    }
    
    # Determine x type (discrete or continuous)
    if (is.factor(data[[x]]) || is.character(data[[x]])) {
        x_type <- "discrete"
    } else {
        x_type <- "continuous"
    }
    
    # Prepare the data
    data[[y]] <- ifelse(data[[fill]] == negative_group, -data[[y]], data[[y]])
    data[["hv_just"]] <- ifelse(data[[y]] < 0, number_hvjust_factor, -number_hvjust_factor/4)
    
    # Determine the limits for the y-axis
    if (symmetry_ylim) {
        y_max <- round(max(abs(data[[y]])) * y_limits_expand_factor)
        y_limits <- c(-y_max, y_max)
    } else {
        y_min <- min(data[[y]])
        y_max <- max(data[[y]]) 
        y_limits <- c(y_min, y_max)  * y_limits_expand_factor
    }
    
    # Generate the plot
    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(fill))) +
        geom_bar(stat = "identity", alpha = bar_alpha)
    
    if(flip_xy_axis){
        p <- p + coord_flip()
    }
    
    # Add text if required
    if (show_numbers) {
        if (is.null(number_color)) {
            if (flip_xy_axis) {
                p <- p + geom_text(aes(label = abs(!!sym(y)), hjust = hv_just, color = !!sym(fill)), size = number_size)
            } else {
                p <- p + geom_text(aes(label = abs(!!sym(y)), vjust = hv_just, color = !!sym(fill)), size = number_size)
            }
        } else {
            if (flip_xy_axis) {
                p <- p + geom_text(aes(label = abs(!!sym(y)), hjust = hv_just), color = number_color, size = number_size)
            } else {
                p <- p + geom_text(aes(label = abs(!!sym(y)), vjust = hv_just), color = number_color, size = number_size)
            }
        }
    }
    # Adjust the y-axis, all use absolute value, and set the y limits
    p <- p + scale_y_continuous(labels = abs, limits = y_limits)
    
    # Flip coordinates if required
    if(is.null(x_breaks)){
        x_breaks = unique(data[[x]])
    }
    if (reverse_x_axis) {
        if (x_type == "discrete") {
            p <- p + scale_x_discrete(limits = rev, breaks = x_breaks)
        } else {
            p <- p + scale_x_reverse(breaks = x_breaks)
        }
    }else{
        if (x_type == "discrete") {
            p <- p + scale_x_discrete(breaks = x_breaks)
        } else {
            p <- p + scale_x_continuous(breaks = x_breaks)
        }
    }
    
    # Facet_wrap if require
    if(!is.null(facet_func)){
        facets_formula <- as.formula(paste("~", facet_func))
        # Combine the facets formula and the additional arguments
        facet_argv <- c(list(facets = facets_formula), facet_argv)
        p <- p + do.call(facet_wrap, facet_argv)
    }

    # give theme
    p <- p + do.call(basic_theme, theme_argv)
    
    return(p)
}

paletteer_get_central_trimmed <- function(color_palette, trim_percentage) {
    if (trim_percentage < 0 || trim_percentage >= 1) {
        stop("Trim percentage must be between 0 and 1 (exclusive).")
    }
    
    n_col_pal <- length(color_palette)
    strat_ind <- round(n_col_pal * trim_percentage / 2) + 1
    end_ind <- n_col_pal - round(n_col_pal * trim_percentage / 2)
    
    # Ensure indices are within valid range
    if (strat_ind > end_ind) {
        stop("Trim percentage too large, resulting in invalid trim.")
    }
    
    trimmed_color_palette <- color_palette[strat_ind:end_ind]
    return(trimmed_color_palette)
}

paletteer_get_unbalanced_palette <- function(balanced_palette, .data, midpoint, corresponding_color_data_map = F) {
    data_range <- range(.data, na.rm = T)
    n <- length(balanced_palette)
    
    if(n < 30){
        warning("length of color palette should > 30 to get precise unbalanced palette.")
    }
    
    # Calculate the proportions of the range below and above the midpoint
    low_prop <- (midpoint - data_range[1]) / (data_range[2] - data_range[1])
    high_prop <- (data_range[2] - midpoint) / (data_range[2] - data_range[1])
    
    # Calculate the number of colors for each part, the wired calculation is just for forcing the ind to integer and continous
    ## if balance
    balance_n_low <- round(n/2)
    balance_n_high <- n - balance_n_low
    ## now
    n_low <- round(n * low_prop)
    n_high <- n - n_low
    
    ## force the ind to integer, and continous
    whole_ind <- 1:n
    balance_lower_ind <- 1:balance_n_low
    balance_upper_ind <- setdiff(whole_ind , balance_lower_ind)
    
    # Trimming the palette
    if (!corresponding_color_data_map) { # No trimming
        ## if not trim, just use balance index
        lower_ind = balance_lower_ind
        upper_ind = balance_upper_ind
    } else {
        if (low_prop > high_prop) {
            # Trim the higher part (right end)
            diff_n = balance_n_low - n_high # if balanced, both n_low and n_high should == n/2, but if not equal, that means need trim.
            lower_ind = 1:balance_n_low # No trim
            upper_ind = balance_n_high : (n - diff_n) # Trim colors on right hand
            
        } else {
            # Trim the lower part (left end)
            diff_n = balance_n_low - n_low
            lower_ind = (1 + diff_n) : balance_n_low # Trim colors on left hand
            upper_ind = balance_n_high : n # No trim
            print("Trim Left hand.")
        }
    }
    
    lower_palette <- balanced_palette[lower_ind]
    upper_palette <- balanced_palette[upper_ind]
    
    
    # Adjust the N elements of the upper and lower part, to make the original palette non-balanced
    adjusted_palette <- c(
                            colorRampPalette(lower_palette)(n_low), # The balanced color palette should converted to a unbalanced one
                            colorRampPalette(upper_palette)(n_high)# This step can also be deploy by the values argument in scale_*_gradientn 
        )
    #print(list(balance_n_low= balance_n_low, balance_n_high = balance_n_high, balance_lower_ind = balance_lower_ind, balance_upper_ind = balance_upper_ind,diff_n = diff_n, lower_ind = lower_ind, upper_ind = upper_ind))
    return(adjusted_palette)
}

