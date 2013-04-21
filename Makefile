all: cs51_final

# These must be in the right order--no forward refs
FILES = order.ml dijkstras.ml prio_q.ml myset.ml graphs.ml

cs51_final: $(FILES)
	ocamlc -g -o cs51_final  str.cma $(FILES)
#unix.cma
clean: 
	rm -f *.cmi *.cmo
