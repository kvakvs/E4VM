OCB_FLAGS   = -tag bin_annot -I src -I lib
PROJECTNAME = kerlc

.PHONY: byte native
byte:
	ocamlbuild $(OCB_FLAGS) $(PROJECTNAME).byte

native:
	ocamlbuild $(OCB_FLAGS) $(PROJECTNAME).native
