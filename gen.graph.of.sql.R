# generate graph from sql script by DiagrammeR package

# if (gen.type=="csp_as_node") edge.label <- edge.list$rel

# edge data frame
if (gen.type=="csp_as_node") {
    edge.list <- df1
    setorder(edge.list, i)
    edge.from.id <- lapply(edge.list$from, function(i) which(i==ndf$name)) %>% unlist
    edge.to.id <- lapply(edge.list$to, function(i) which(i==ndf$name)) %>% unlist
    ndf1 <- ndf
    edf <- create_edge_df(
        from = edge.from.id,
        to = edge.to.id,
        rel = edge.list$rel)
} else { # csp_as_edge
    edge.list <- df
    setorder(edge.list, i)
    edge.from.id <- lapply(edge.list$from, function(i) which(i==ndf$name)) %>% unlist
    edge.to.id <- lapply(edge.list$to, function(i) which(i==ndf$name)) %>% unlist
    edge.label.id <- lapply(edge.list$rel, function(i) which(i==ndf$name)) %>% unlist
    ndf1 <- ndf[ndf$type!="csp",]
    edf <- create_edge_df(
        from = edge.from.id,
        to = edge.to.id,
        rel = edge.list$rel,
        label = edge.label.id)
}
# Create the graph object
i_graph_1 <-
    create_graph(
        nodes_df = ndf1,
        edges_df = edf
    )

if (gen.type == "csp_as_node") {
    i_graph_4 <- i_graph_1 %>%
        select_nodes(
            conditions = type == "csp") %>%
        set_node_attrs_ws(
            node_attr = shape,
            value = "circle") %>%
        invert_selection() %>%
        set_node_attrs_ws(
            node_attr = shape,
            value = "rectangle") %>%
        clear_selection() %>% 
        select_edges(
            conditions = rel == "input") %>%
        set_edge_attrs_ws(
            edge_attr = color,
            value = "RoyalBlue") %>%
        invert_selection() %>%
        set_edge_attrs_ws(
            edge_attr = color,
            value = "SeaGreen") %>%
        clear_selection()
} else { # csp_as_edge
    i_graph_4 <- i_graph_1 %>% select_nodes() %>% 
        set_node_attrs_ws(
            node_attr = shape,
            value = "rectangle") %>% 
        clear_selection() #%>% 
    # select_nodes(conditions = type == "csp") %>% 
    # delete_nodes_ws()
}
render_graph(i_graph_4, layout = "nicely")
target.sql.name <- gsub(".sql", "", target.sql)
gen.type.name <- gsub("csp_as_", "", gen.type)
viz.label.name <- gsub("label_as_", "", viz.label)
export_graph(i_graph_4, 
             file_name = file.path("plots", 
                                   sprintf("%s_%s_%s.svg", target.sql.name, 
                                           gen.type.name, viz.label.name)), 
             file_type = "svg")

if (viz.label.name=="id") {
    generate_dot(i_graph_4) %>% cat(file = 'temp.gv')
    system(
        sprintf('sed "s/layout = \'neato\',/layout = \'dot\',rankdir=\'LR\',/g" temp.gv > data/output/x/%s_%s_%s.gv', 
                target.sql.name, gen.type.name, viz.label.name))
    grViz(diagram = file.path("data/output/x", sprintf("%s_%s_%s.gv", target.sql.name, gen.type.name, viz.label.name))) %>%
        export_svg %>% charToRaw %>% 
        rsvg_pdf(file.path("data/output/x", sprintf("%s_%s_%s.pdf", target.sql.name, gen.type.name, viz.label.name)))
}
