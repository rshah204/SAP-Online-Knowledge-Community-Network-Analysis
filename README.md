Network Analysis: SAP Online Knowledge Community Network
----------------

The SAP online knowledge community network is a directed graph forming a discussion thread structure, where a directed edge represents an answer provided by a user to a question posted by another user. The network consists of 3415 nodes and 6090 edges. 

Such a network structure can be used to rank people’s expertise in SAP online community. In order to analyze the network, several network metrics and network cohesion measures are used which are discussed in this project. 

The provided CSV file is a directed edge list. The right-hand column represents the ID of a user who posts a question that starts a new thread in a SAP community user forum. The left-hand column represents the ID of a user who provides an answer to the posted question. So, a directed edge from the left-hand node to a right-hand node represents an answer provided to a question. Since, a user can answer on one or more threads, the file contains duplicate edge-pairs, which are combined to form directed edges of varying weights in the analysis.