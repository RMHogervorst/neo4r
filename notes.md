# Notes on what can and cannot be expressed in neo4j

properties can be

* number (Integer, Float)   13, 15.6
* String 'joke', "jokes"
* Boolean  true, TRUE, FALSE, false
* Spatial type Point
* Temporal types (Date, Time, LocalTime, DateTime, LocalDateTime, Duration) # unfortunately igraph f*cks this up


names must start with alphabetic letter (can use escaping with backtic `` )
can use underscore, $ is used for parameter
names are case sensitive
keywords are NOT CASE sensitive. mAtch MATCH work .

Neo4j recommondations 

- Node labels camel case ':VehicleOwner' 
- Relationship types upper case with underscore to seperate words ':OWNS_VEHICLE'


# notes on scope of tidygraph-neo4j

I will dump the entire graph into neo4j. 
The first approach is creating a tbl_graph to cypher translation of the two tables.

I hope to manipulate the graph from R with tidygraph verbs. 

There are some limitations, I will start with the basic verbs from tidygraph, 
neo4j needs a relationship label on every relationship, but igraph does not. I will enforce that before pushing to the 
database. neo4j forces you to create direction on every connection
