all: cs51_final

# These must be in the right order--no forward refs
<<<<<<< HEAD
# order.ml dict.ml myset.ml prio_q.ml bin_heap.ml graphs.ml matrix.ml new_dij.ml

FILES = order.ml prio_q.ml bin_heap.ml
=======
FILES = order.ml dict.ml myset.ml fibsource.ml prio_q.ml graphs.ml matrix.ml new_dij.ml
### matrix.ml 
>>>>>>> ccbd2a0ac718f11cfb06beacd3a2aa91d380f66e

cs51_final: $(FILES)
	ocamlc -g -o cs51_final str.cma $(FILES)
#unix.cma
clean: 
	rm -f *.cmi *.cmo cs51_final
