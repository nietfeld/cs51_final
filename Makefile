all: cs51_final

# fibsource.ml
FILES = order.ml dict.ml myset.ml fibsource.ml prio_q.ml graphs.ml dij.ml
### matrix.ml 

cs51_final: $(FILES)
	ocamlc -g -o cs51_final str.cma $(FILES)
#unix.cma
clean: 
	rm -f *.cmi *.cmo cs51_final *.cma
