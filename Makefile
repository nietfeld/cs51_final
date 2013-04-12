#all: cs51_final

# These must be in the right order--no forward refs
FILES = dijkstras.ml graphs.ml prio_q.ml

moogle: $(FILES)
	ocamlc -g -o unix.cma str.cma $(FILES)

clean: 
	rm -f *.cmi *.cmo
