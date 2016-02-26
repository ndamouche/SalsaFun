

OCAMLC=ocamlc
OCAMLOPT=ocamlopt -g
OCAMLDEP=ocamldep
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

SRC=./

SALSA-EVAL-OPT = salsaTypes.cmx prelude.cmx measure.cmx  ssa.cmx print.cmx float.cmx epeg_types.cmx epeg_prelude.cmx ast2peg.cmx peg2epeg.cmx epeg2epegsharp.cmx profitability.cmx mainEPEG.cmx parser.ml parser.mli parser.cmi parser.cmx lexer.cmx rewrite.cmx function.cmx maineval.cmx 

SALSAEVALOPT = nums.cmxa salsaTypes.cmx prelude.cmx measure.cmx ssa.cmx print.cmx float.cmx epeg_types.cmx epeg_prelude.cmx ast2peg.cmx peg2epeg.cmx epeg2epegsharp.cmx profitability.cmx  mainEPEG.cmx lexer.cmx parser.cmx rewrite.cmx function.cmx maineval.cmx 

SALSA-RW-OPT = salsaTypes.cmx prelude.cmx measure.cmx  ssa.cmx print.cmx float.cmx epeg_types.cmx epeg_prelude.cmx ast2peg.cmx peg2epeg.cmx epeg2epegsharp.cmx profitability.cmx mainEPEG.cmx parser.ml parser.mli parser.cmi parser.cmx lexer.cmx  rewrite.cmx function.cmx mainrw.cmx 

SALSARWOPT = nums.cmxa salsaTypes.cmx prelude.cmx measure.cmx ssa.cmx print.cmx float.cmx epeg_types.cmx epeg_prelude.cmx ast2peg.cmx peg2epeg.cmx epeg2epegsharp.cmx profitability.cmx mainEPEG.cmx lexer.cmx parser.cmx rewrite.cmx function.cmx mainrw.cmx 


SALSA-MEAS-OPT = salsaTypes.cmx prelude.cmx measure.cmx  ssa.cmx print.cmx float.cmx epeg_types.cmx epeg_prelude.cmx ast2peg.cmx peg2epeg.cmx epeg2epegsharp.cmx profitability.cmx  mainEPEG.cmx parser.ml parser.mli parser.cmi parser.cmx lexer.cmx rewrite.cmx function.cmx mainmeas.cmx 

SALSAMEASOPT = nums.cmxa salsaTypes.cmx prelude.cmx measure.cmx ssa.cmx print.cmx float.cmx epeg_types.cmx epeg_prelude.cmx ast2peg.cmx peg2epeg.cmx epeg2epegsharp.cmx profitability.cmx mainEPEG.cmx lexer.cmx parser.cmx rewrite.cmx function.cmx mainmeas.cmx 

SALSA-OPT = prelude.cmx ssa.cmx print.cmx float.cmx lexer.cmx parser.cmx salsa.cmx

SALSAOPT = nums.cmxa prelude.cmx ssa.cmx print.cmx float.cmx lexer.cmx parser.cmx salsa.cmx

salsa:  $(SALSA-OPT)
	$(OCAMLOPT) -o salsa $(SALSAOPT)

salsa-eval-opt: $(SALSA-EVAL-OPT)
	$(OCAMLOPT) -o salsa-eval-opt $(SALSAEVALOPT)

salsa-rw-opt: $(SALSA-RW-OPT)
	$(OCAMLOPT) -o salsa-rw-opt $(SALSARWOPT)

salsa-meas-opt: $(SALSA-MEAS-OPT)
	$(OCAMLOPT) -o salsa-meas-opt $(SALSAMEASOPT)


# Common rules
.SUFFIXES: .ml .mli .cmx .mll .mly .cmo 

.mll.ml:
	$(OCAMLLEX) $(SRC)$<

.mly.mli:
	$(OCAMLYACC) $(SRC)$<

.mli.ml:
	$(OCAMLC) $(INCLUDES) -c $(SRC)$<

.ml.cmx:
	$(OCAMLOPT) $(INCLUDES) -c $(SRC)$<

.ml.cmo:
	$(OCAMLC) $(INCLUDES) -c $(SRC)$<


# Clean up
clean:
	rm -f $(SRC)*.cmx
	rm -f $(SRC)*.cmi
	rm -f $(SRC)*.cmo
	rm -f $(SRC)*.o
	rm -f $(SRC)lexer.mli lexer.ml 
	rm -f $(SRC)parser.mli parser.ml
	rm -f $(SRC)salsa-meas-opt
	rm -f $(SRC)salsa-eval-opt
	rm -f $(SRC)salsa
	rm -f $(SRC)salsa-rw-opt
	rm -f $(SRC)depend

depend:
	touch depend ; $(OCAMLDEP) $(INCLUDES) *.mli *.ml > depend

all: depend salsa-eval-opt salsa-rw-opt salsa-meas-opt salsa

include depend
