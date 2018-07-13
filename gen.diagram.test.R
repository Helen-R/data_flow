source("../auxiliary/mylib.R")
mylib("DiagrammeR")

example_graph <-
    create_graph() %>%
    add_pa_graph(
        n = 50,
        m = 1,
        set_seed = 23) %>%
    add_gnp_graph(
        n = 50,
        p = 1/100,
        set_seed = 23) %>%
    join_node_attrs(
        df = get_betweenness(.)) %>%
    join_node_attrs(
        df = get_degree_total(.)) %>%
    colorize_node_attrs(
        node_attr_from = total_degree,
        node_attr_to = fillcolor,
        palette = "Greens",
        alpha = 90) %>%
    rescale_node_attrs(
        node_attr_from = betweenness,
        to_lower_bound = 0.5,
        to_upper_bound = 1.0,
        node_attr_to = height) %>%
    select_nodes_by_id(
        nodes = get_articulation_points(.)) %>%
    set_node_attrs_ws(
        node_attr = peripheries,
        value = 2) %>%
    set_node_attrs_ws(
        node_attr = penwidth,
        value = 3) %>%
    clear_selection() %>%
    set_node_attr_to_display(
        attr = NULL)
#> `select_nodes_by_id()` INFO: created a new selection of 34 nodes
#> `clear_selection()` INFO: cleared an existing selection of 34 nodes

example_graph %>%
    render_graph(layout = "nicely")



a_graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    add_edge(
        from = 1,
        to = 2)

b_graph <-
    a_graph %>%
    delete_edge(
        from = 1,
        to = 2)

c_graph <-
    b_graph %>%
    add_node(
        from = 1,
        to = 2)

d_graph <-
    c_graph %>%
    add_node(
        type = "type_a",
        node_aes = node_aes(
            color = "steelblue",
            fillcolor = "lightblue",
            fontcolor = "gray35"),
        node_data = node_data(
            value = 2.5)) %>%
    add_edge(
        from = 1,
        to = 3,
        rel = "interacted_with",
        edge_aes = edge_aes(
            color = "red",
            arrowhead = "vee",
            tooltip = "Red Arrow"),
        edge_data = edge_data(
            value = 2.5))

render_graph(d_graph,layout = "nicely")
