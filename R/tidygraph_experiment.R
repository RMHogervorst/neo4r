### NEO4r tidgraph implementation
# Mon Jun 25 11:30:14 2018 ------------------------------
library(neo4r)
library(tidygraph)

# A. sending and recieving a graph from and to tidygraph and neo4j
# B. translating queries from tidygraph to cypher

# A. sending and recieving a graph from and to tidygraph and neo4j ====
#
# 1. create a minimal tidygraph object with nodes and edges
#   both nodes and edges need to have attributes.
# 2. parse tidygraph structure into cypher
# 3. send cypher query to database
# 4. check result in database
# 5. read back result into tidygraph (objects should be the same)
#
# # 1. create a minimal tidygraph object with nodes and edges -----
#   both nodes and edges need to have attributes.
# https://github.com/mathbeveridge/asoiaf
edges <- readr::read_csv("https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-book1-edges.csv")%>%
  mutate(RELATIONSHIP = "appeared within 15 words of one another") %>%
  select(-Type)
nodes <- readr::read_csv("https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-book1-nodes.csv")
# create dataset
asoiaf1 <-
  as_tbl_graph(edges, directed = FALSE) %>%
  activate(nodes) %>%
  left_join(nodes, by = c("name"="Id"))
asoiaf1 # yes it is a graph

library(ggraph)
ggraph(asoiaf1)+
  geom_edge_link()+
  geom_node_point()

# 2. parse tidygraph structure into cypher -----
# - pull out the two dataframes?
# - use from and to as labels in cypher?
# - create nodes statement using rownumber as alias?
extract_nodes <- function(tbl_graph, node_type = "person"){
  activate(tbl_graph, nodes) %>%
    tibble::as_data_frame() %>%
    tibble::rowid_to_column(var = "node_id") %>%
    mutate(
      node_id = paste0("n",node_id),
      node_type = node_type
      )
}


create_nodes_statement <- function(df, nodeid = "node_id", nodetype ){

  paste_glue1 <- paste0("MERGE ( {",nodeid,"}",
                        ifelse(missing(nodetype), " ",paste0(":{", nodetype, "}")))
   col_names <- colnames(df)[!colnames(df) %in% c(nodeid, ifelse(base::missing(nodetype), NA, nodetype)) ]
  glue_query <- paste0(paste_glue1, " {{", paste0(col_names, ":'{", col_names,"}'",collapse = ", "), " }})")
  glue_data(df, glue_query)
}

#
# - create edges statement using from and to as nodes
extract_edges <- function(tbl_graph){
  activate(tbl_graph, edges) %>%
    tibble::as_data_frame() %>%
    mutate(
      from = paste0("n",from),
      to = paste0("n",to)
    )
}
extract_edges(asoiaf1)

# TODO: treat boolean, character and number different. only single quote characters

create_edges_statement <- function(df, relationship = "RELATIONSHIP"){
  glue_partstart <- paste0()
  col_names <- colnames(df)[!colnames(df) %in% c(relationship, "from", "to" )]
  if(length(col_names)!= 0){
    glue_query <-
      paste0("MERGE ({from})-[:",relationship, " {{",
             paste0(col_names, ":'{", col_names,"}'",collapse = ", "),
             " }}] ->({to})")
  }else {
    glue_query <-
      paste0("MERGE ({from})-[:",relationship, "] ->({to})")
  }
  glue_data(df, glue_query)
}
extract_edges(asoiaf1) %>% create_edges_statement()



# 3. send cypher query to database
graph_to_cypher <- function(tbl_graph, nodetype = "node_type", relationship="RELATIONSHIP"){
  nodes <- create_nodes_statement(
    df = extract_nodes(tbl_graph),
    nodetype = nodetype
  )
  edges <- create_edges_statement(
    df = extract_edges(tbl_graph),
    relationship = relationship
  )
  glue::collapse(c(nodes, edges),sep = "\n")
}
result <- graph_to_cypher(asoiaf1) %>%
  call_api(con)
#### maybe it was too big.
subgraph_idea <-
  asoiaf1 %>%
  activate(edges) %>%
  filter(weight > 50) %>%
  activate(nodes) %>%
  filter(!node_is_isolated())

subgraph_idea %>%
  graph_to_cypher() %>%
  call_api(con)

# what is related and how
"CALL db.schema()" %>% call_api(con)

# count all relationships
"MATCH ()-->() RETURN count(*)" %>% call_api(con)

# list node labels
"CALL db.labels()" %>% call_api(con)
# list relatinohsip types
"CALL db.relationshipTypes();" %>% call_api(con)
# display constraints and indexes
result3 <- ":schema" %>% call_api(con)
# count all nodes
"MATCH (n) RETURN count(n)" %>% call_api(con)

# show metagraph
result4 <- "CALL db.schema()" %>% call_api(con)

# get all nodes
result <- "MATCH (n) return n" %>% call_api(con, type = "graph")
result2 <- "MATCH ()-->() as b RETURN b"


# 4. check result in database
# 5. read back result into tidygraph (objects should be the same)

result5 <- "MATCH (n)-[r]->(d)
RETURN n, r, d" %>% call_api(con)

result6 <- "MATCH p = ()-[RELATION {}]->() return p" %>% call_api(con)

"MATCH ()-[r]-() return r" %>% call_api(con)
"MATCH p = (a)-[b]-(c) return p,a,b,c" %>% call_api(con, type = "graph", meta = TRUE)
"MATCH p = ()-[]-() return p" %>% call_api(con,type = "graph")

# but not directly tidygraph like.
# I want, start end node, relationship, properties of relationship
# and per node all properties
# mix queries?

# B. translating queries from tidygraph to cypher =====
# 1. list the main manipulation functions from tidygraph
# 2. create a new class and set of functions e.g.
# from filter.tbl_graph  to filter.tbl_neo4r, select.tbl_neo4r,
# a print method for the class.
#
