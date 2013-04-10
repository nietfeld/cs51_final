#all: moogle

# These must be in the right order--no forward refs
FILES = main.ml

moogle: $(FILES)
	ocamlc -g -o unix.cma str.cma $(FILES)

clean: 
	rm -f *.cmi *.cmo
