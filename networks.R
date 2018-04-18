prepare_network_viz <- function(
  raw_df,
  project_year_of_interest = c(2018, 2017, 2016, 2015, 2014, 2013),
  project_size_of_interest = c("large", "small", NA),
  height = "800px",
  threshold = 2
) {
  graph_df <- raw_df %>% 
    prepare_input_df(
      project_year_of_interest = project_year_of_interest,
      project_size_of_interest = project_size_of_interest) %>% 
    prepare_graph_df(threshold = threshold)
  
  edges <- graph_df %>%
    prepare_edges()
  
  nodes <- graph_df %>%
    prepare_nodes()
  
  nodes <- filter_nodes(nodes, edges)
  
  if (any(is.na(project_size_of_interest))) {
    title <- paste(
      "All projects in", 
      paste(project_year_of_interest, collapse = " & ")
    )
  } else {
    title <- paste(
      paste(project_size_of_interest, collapse = " & "),
      "projects in",
      paste(project_year_of_interest, collapse = " & ")
    )
  }
  
  create_network_viz(
    nodes = nodes,
    edges = edges,
    title = title
  )
}

prepare_input_df <- function(
  df,
  project_year_of_interest = c(2018, 2017, 2016, 2015, 2014, 2013),
  project_size_of_interest = c("large", "small", NA)
) {
  df %>% 
    dplyr::filter(
      .data$project_year %in% project_year_of_interest,
      .data$project_size %in% project_size_of_interest,
      .data$Applicant != "None"
    ) %>% 
    dplyr::distinct(
      .data$Applicant,
      .data$project_title,
      .keep_all = TRUE
    ) %>% 
    dplyr::mutate(
      Applicant = paste0("PERSON~", .data$Applicant),
      project_title = paste0("PROJECT~", .data$project_title)
    ) %>% 
    dplyr::select(
      .data$Applicant, 
      .data$project_title
    ) %>% 
    dplyr::arrange(
      .data$Applicant,
      .data$project_title
    )
}

prepare_graph_df <- function(df, threshold) {
  df %>% tidygraph::as_tbl_graph(directed = FALSE) %>%
    dplyr::mutate(size = tidygraph::centrality_degree(mode = 'in')) %>% 
    dplyr::filter(
      stringr::str_detect(name, "PROJECT~") | size >= threshold
    )
}

prepare_nodes <- function(graph_df) {
  graph_df %>% 
    tidygraph::activate(nodes) %>%
    dplyr::rename(label = name) %>%
    dplyr::as_tibble() %>% 
    dplyr::mutate(
      id = row_number(),
      group = dplyr::case_when(
        stringr::str_detect(label, "PERSON~") ~ "Bridging collaborator",
        stringr::str_detect(label, "PROJECT~") ~ "Project"
      ),
      label = stringr::str_replace(label, pattern = "PERSON~|PROJECT~", ""),
      title = dplyr::case_when(
        group == "Bridging collaborator" ~ paste0(
          stringr::str_replace(label, pattern = "PERSON~|PROJECT~", ""), "<br>",
          size, " projects"
        ),
        group == "Project" ~ paste0(
          stringr::str_replace(label, pattern = "PERSON~|PROJECT~", ""), "<br>",
          size, " collaborators"
        )
      ),
      font.size = 0
    )
}

prepare_edges <- function(graph_df) {
  graph_df %>% 
    tidygraph::activate(edges) %>%
    dplyr::as_tibble()
}

filter_nodes <- function(nodes, edges) {
  nodes %>% 
    dplyr::filter(.data$id %in% edges$from | .data$id %in% edges$to)
}

create_network_viz <- function(
  nodes,
  edges,
  title,
  height = "500px"
) {
  visNetwork::visNetwork(
    nodes = nodes,
    edges = edges,
    width = "100%",
    height = height
  ) %>%
    visNetwork::visInteraction(hover = T) %>%
    visNetwork::visEvents(
      hoverNode = "function(n){
      this.body.data.nodes.update({id: n.node, font: {size : 14}});
    }") %>%
    visNetwork::visEvents(
      blurNode = "function(n){
      this.body.data.nodes.update({id: n.node, font: {size : 0}});
    }") %>%
    visNetwork::visOptions(
      highlightNearest = TRUE, 
      nodesIdSelection = TRUE,
      manipulation = TRUE
    ) %>%
    visNetwork::visLegend() %>%
    visNetwork::visGroups(
      groupname = "Project", shape = "icon",
      icon = list(code = "f0c0", size = 75)) %>%
    visNetwork::visGroups(
      groupname = "Bridging collaborator", shape = "icon",
      icon = list(code = "f007", color = "red")) %>%
    visNetwork::addFontAwesome() 
}