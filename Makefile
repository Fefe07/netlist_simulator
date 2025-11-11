all: main
	./main
	
main:
	ocamlbuild netlist_simulator.byte
