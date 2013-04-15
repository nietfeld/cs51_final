all: cs51_final

# These must be in the right order--no forward refs
FILES = dijkstras.ml graphs.ml prio_q.ml

cs51_final: $(FILES)
	ocamlc -g -o cs51_final unix.cma str.cma $(FILES)

clean: 
	rm -f *.cmi *.cmo
