
## library

# get imports
import_dir=Sys.getenv("MY_IMPORT",unset="~/BIN/junsheng/IMPORT")
import_path=file.path(import_dir,"import_R.R")
source(import_path)

#### library others

library(ggnetwork)
library(igraph)
library(ggnewscale)

## custom functions


#### get node attr
get_node_attr=function(graph,marker="default"){
    # weight should be all positive in calculation
    E(graph)$corr = E(graph)$weight
    E(graph)$weight = abs(E(graph)$weight)
    # to df
    df_node=tibble(get.data.frame(graph,what = "vertices"))
    # calculate attributes
    df_node["degree"]=degree(graph)
    df_node["weight_degree"]=strength(graph)
    df_node["neighbor"]=graph.knn(graph, V(graph),weights = NA )$knn
    df_node["weight_neighbor"]=graph.knn(graph, V(graph))$knn
    df_node["closeness_centrality"] = closeness(graph)
    df_node["betweenness_centrality"]=betweenness(graph)
    df_node["eigenvector_centrality"]=evcent(graph)$vector
    
    # give a marker col
    df_node["marker"]=marker
    return(df_node)
}

#### filter function
filter_by_name=function(graph,name_vector,include=T){
    if(include){
        remove_name=which(!(V(graph)$name%in%name_vector))
    }else{
        remove_name=which((V(graph)$name%in%name_vector))
    }
    graph=graph%>%delete_vertices(remove_name)
    return(graph)
}
filter_by_degree=function(graph,thresh=0){
    V(graph)$degree=degree(graph)
    remove_index=which(V(graph)$degree<=thresh)
    graph_filtered=graph%>%delete_vertices(remove_index)
    return(graph_filtered)
}

#### get dataframe
ggnetwork_split_plus=function(graph,layout=NULL,weight_direction=T,infer_attr=T){
    if(is.null(layout)){
        df_graph=ggnetwork(graph,layout=igraph::layout_with_mds(graph))
    }else{
        df_graph=ggnetwork(graph,layout=layout(graph))
    }
    df_graph=df_graph%>%mutate(strength=abs(weight))
    if(weight_direction){
        df_graph=df_graph%>%
            mutate(weight_direction=if_else(weight>0,"pos","neg"))%>%
            mutate(weight_direction=factor(weight_direction,level=c("pos","neg")))
    }
    if(infer_attr){
        df_node_attr=get_node_attr(graph)
    }
    df_node=df_graph%>%distinct(x,y,name,.keep_all=T)%>%left_join(df_node_attr)
    df_edge=df_graph%>%filter(!is.na(weight))
    return(list(df_node=df_node,df_edge=df_edge))
}

plot_network_mds=function(graph,color,need_species){
    graph_filter=graph%>%filter_by_name(need_species,include = T)%>%filter_by_degree(thresh = 1)
    df_graph_list=ggnetwork_split_plus(graph_filter,layout = igraph::layout_with_mds)
    df_node=df_graph_list$df_node
    df_edge=df_graph_list$df_edge
    fig=ggplot()+
                geom_nodes(mapping = aes(x,y,size=degree,alpha=degree),data = df_node,color=color)+
                scale_size(range=c(3,6))+


                new_scale("size")+
                new_scale("alpha")+
                geom_edges(mapping = aes(x,y,xend=xend,yend=yend,size=strength,alpha=strength),data=df_edge,color=color)+
                scale_size(range = c(1,3))+
                scale_alpha(range=c(0.3,0.6))+


                new_scale("size")+
                geom_nodetext_repel(mapping=aes(x,y,label=name,size=degree),data=df_node,)+
                scale_size(range = c(3,6))+

                coord_fixed()+
                theme_blank()
    return(fig)
    
}

plot_network_circular=function(graph,need_species,layout="c"){
    graph_filter=graph%>%filter_by_name(need_species,include = T)%>%filter_by_degree(thresh = 1)
    
    if(layout=="c"){
        layout=igraph::layout_in_circle
    }else if(layout=="s"){
        layout=igraph::layout_as_star
    }
    df_graph_list=ggnetwork_split_plus(graph_filter,layout =layout )
    df_node=df_graph_list$df_node
    df_edge=df_graph_list$df_edge
    fig=ggplot()+
                geom_nodes(mapping = aes(x,y,size=degree),data = df_node)+
#               scale_size(range=c(3,6))+


#               new_scale("size")+
#               new_scale("alpha")+
                geom_edges(mapping = aes(x,y,xend=xend,yend=yend,color=weight_direction),data=df_edge,alpha=0.6,size=1)+
#               scale_size(range = c(1,3))+
#               scale_alpha(range=c(0.3,0.6))+
                scale_color_manual(breaks=c("pos","neg"),values=c("#E57C6F","#5DBDC3"))+

#               new_scale("size")+
                geom_nodetext_repel(mapping=aes(x,y,label=name),data=df_node,)+
#               scale_size(range = c(3,6))+

                coord_fixed()+
                theme_blank()
    return(fig)
    
}

## get attributes
args = commandArgs(trailingOnly = TRUE)
graph_path=args[1]
width_obj=args[2]
height_obj=args[3]
size_obj=c(width_obj%>%as.numeric,height_obj%>%as.numeric)
file_marker=graph_path%>%basename

## plot
g=read_graph(graph_path,format="gml")
need_species=V(g)$name

fig1=plot_network_circular(g,need_species = need_species,layout = "c" )

ggsave_wrap(fig1,fig_dir = "plot_network_special",fig_name = sprintf("%s_circular",file_marker),fig_fmt = "pdf",size = size_obj)


