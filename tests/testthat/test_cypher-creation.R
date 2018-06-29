context("Test cypher-creation")
# TODO: this needs to be a smaller working example
setup({
  edges <- readr::read_csv("https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-book1-edges.csv")
  nodes <- readr::read_csv("https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-book1-nodes.csv")
  # create dataset
  asoiaf1 <-
    as_tbl_graph(edges, directed = FALSE) %>%
    activate(nodes) %>%
    left_join(nodes, by = c("name"="Id"))
}
)

test_that("nodes extraction into dataframe works",{
  dataframe_n <- extract_nodes(asoiaf1)
  expect_equal(nrow(dataframe_n), nrow(nodes),label = "Is it the dataframesize we expect?")
  expect_true(is.character( dataframe_n$node_id))
})

test_that("edges extraction into dataframe works", {
  dataframe_e <- extract_edges(asoiaf1)
  expect_true(all(unique(c(dataframe_e$from, dataframe_e$to)) %in% dataframe_n$node_id), label = "all nodes in edges are in nodes")
  expect_equal(sum(colnames(dataframe_e) %in% c("from", "to")), 2,label = "does it contain a from and to column?" )
})


# create examples
nodes1_example <- extract_nodes(asoiaf1) %>%
  create_nodes_statement( "node_id") %>%
  .[1]
nodes2_example <- extract_nodes(asoiaf1) %>%
  create_nodes_statement( "node_id", "node_type") %>%
  .[1]

test_that("node creation statement works", {
  expect_equal(
    nodes1_example,
    "CREATE ( n1  {name:'Addam-Marbrand', Label:'Addam Marbrand', node_type:'person' })"
  )
  expect_equal(
    nodes2_example,
    "CREATE ( n1:person {name:'Addam-Marbrand', Label:'Addam Marbrand' })"
  )
})


teardown({
  unlink(asoiaf1)
  unlink(edges)
  unlink(nodes)
}
)
