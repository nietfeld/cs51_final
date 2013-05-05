cs51_final
==========

CS51 Final project

First, pull all our files from git or download our directory folder (cs51_final)

The following are all the files contained in our directory with the brief description of what is contained in each:

dij.ml - Our code for running Dijkstra's algorithm
graphs.ml - Our adjacency list and matrix representations
prio_q.ml - Our priority queue implentations 
dict.ml	- dicts from Moogle for our graph (adapted to our needs)
myset.ml - sets from Moogle for our dicts (adapted to our needs)
order.ml - the order module for our graphs 
fibsource.ml - the FibLib library for our Fibonnacci heaps

The user can create a graph by calling from_edges on a list of int * float * int triples. The ints must include only numbers between 0 and one less than the number of distinct nodes. The ints represent the nodes and the floats represent the weights of the edges connecting the specified pair of nodes. 

To run Djikstra's the user must type :
   dij <index of starting node> graph

To chose between the type of graph or priority queue, the user can choose which fields to uncomment at the top of dij.ml. For D-ary heaps, the user creates a module passing in the desired "d". 

To run our code, first make by typing "make" into the command line and then run it by typing "./cs51_final" at the command line. The results of running Dijkstra's on the specified graph will be printed to stdout.