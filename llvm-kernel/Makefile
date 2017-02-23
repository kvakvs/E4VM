OCB_FLAGS   = -tag bin_annot -pkgs llvm,llvm.executionengine -use-ocamlfind
PROJECTNAME = kerlc

.PHONY: byte native
byte:
	ocamlbuild $(OCB_FLAGS) $(PROJECTNAME).byte

native:
	ocamlbuild $(OCB_FLAGS) $(PROJECTNAME).native
