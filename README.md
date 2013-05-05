cs51_final
==========

CS51 Final project

First, pull all our files from git. 

The following are all the files contained in our directory with the brief description of what is contained in each:

dij.ml - Our code for running Dijkstra's algorithm
graphs.ml - Our adjacency list and matrix representations
prio_q.ml - Our priority queue implentations 
dict.ml	- dicts from Moogle for our graph
myset.ml - sets from Moogle for our dicts
order.ml - the order module for our graphs 
fibsource.ml - the FibLib library for our Fibonnacci heaps


To run Djikstra's, the user first creates a graph by calling from_edges on a list of int * float * int triples. The ints must include only numbers between 0 and one less than the number of distinct nodes. 

To run Djikstra's the user types :
   dij <index of starting node> graph

To chose between the type of graph or priority queue, the user can choose which fields to uncomment at the top of dij.ml. For D-ary heaps, the user creates a module passing in the desired "d". 