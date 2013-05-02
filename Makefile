all: cs51_final

# These must be in the right order--no forward refs
FILES = order.ml dict.ml myset.ml fibsource.ml prio_q.ml graphs.ml matrix.ml dij.ml
# order.ml dict.ml myset.ml prio_q.ml graphs.ml dij.ml

cs51_final: $(FILES)
	ocamlc -g -o cs51_final str.cma $(FILES) graphics.cma  -cclib -lgraphics -cclib -L/usr/X11/lib -cclib -lX11
#unix.cma
clean: 
	rm -f *.cmi *.cmo cs51_final
