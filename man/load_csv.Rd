% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_csv.R
\name{load_csv}
\alias{load_csv}
\title{Load a CSV to Neo4J}
\usage{
load_csv(on_load = "", con, url, header = TRUE,
  periodic_commit = 1000, as = "csv", type = c("row", "graph"),
  output = c("r", "json"), include_stats = TRUE, meta = FALSE)
}
\arguments{
\item{on_load}{the code to execute on load}

\item{con}{the connexion object}

\item{url}{the url of the csv}

\item{header}{does the csv have a header?}

\item{periodic_commit}{the PERIODIC COMMIT cypher arg}

\item{as}{the AS cypher arg}

\item{meta}{should the result include meta ?}

\item{format}{the format to return}
}
\value{
a csv loaded to Neo4J
}
\description{
Load a CSV to Neo4J
}
