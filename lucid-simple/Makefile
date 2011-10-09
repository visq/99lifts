# Makefile
OBJ = data.cmo lift.cmo sim.cmo main.cmo 
BIN = main

OCAMLC = ocamlc
LUCYLIB= ../lucy/lib/lucid-synchrone/
LUCYC = lucyc -stdlib $(LUCYLIB)

$(BIN): $(OBJ) 
	$(OCAMLC) -o $(BIN) -custom unix.cma graphics.cma $(OBJ) 

main.ml: lift.ml sim.ml
	$(LUCYC) -s main -sampling 10 lift.ls sim.ls

nodesim:
	lucyc -stdlib $(LUCYLIB) -print type -s $(NODE) $(M).ls
	lucys -s $(NODE) $(M).lci
	ocamlc -o $(NODE)_sim unix.cma -I +lablgtk2 lablgtk.cma $(M).ml $(NODE).ml $(NODE)_sim.ml 

clean:
	/bin/rm -f *~ *.cmi *.cmo *.cmx *.o *.lci *.dcc sim.ml lift.ml main.ml *_demo.* *_sim.* \
	           *demo_sim $(BIN)

.SUFFIXES: .ls .ml .cmo .dcc .mli .cmi .lci

.ls.ml:
	$(LUCYC) $<
.ml.cmo:
	$(OCAMLC) -c $<
.mli.cmi:
	$(OCAMLC) -c $<
.mli.lci:
	$(LUCYC) $<

#dependences
data.cmo: data.cmi
lift.ml: data.lci
lift.cmo: data.cmi
