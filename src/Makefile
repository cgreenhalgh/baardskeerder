OCAML_FIND ?= ocamlfind

build:
	ocamlbuild -use-ocamlfind bsmgr.native baardskeerder.cma baardskeerder.cmxa baardskeerder.a bs_internal.cma bs_internal.cmxa baardskeerder_unix.cma baardskeerder_unix.cmxa baardskeerder_unix.a baardskeerder_mirage.cma baardskeerder_mirage.cmxa baardskeerder_mirage.cmi baardskeerder_mirage.a

clean:
	ocamlbuild -use-ocamlfind -clean
install:
	$(OCAML_FIND) install mirage-baardskeerder META \
	  _build/baardskeerder.cma \
	  _build/baardskeerder.cmxa \
	  _build/bs_internal.cma \
	  _build/bs_internal.cmi \
	  _build/bs_internal.cmxa \
	  _build/baardskeerder.mli \
	  _build/baardskeerder.cmi \
	  _build/libbaardskeerder_c.a \
	  _build/baardskeerder.a \
	  _build/baardskeerder_unix.cma \
	  _build/baardskeerder_unix.cmxa \
	  _build/baardskeerder_unix.mli \
	  _build/baardskeerder_unix.cmi \
	  _build/baardskeerder_unix.a \
	  _build/libbaardskeerder_unix_c.a \
	  _build/baardskeerder_mirage.cma \
	  _build/baardskeerder_mirage.cmxa \
	  _build/baardskeerder_mirage.mli \
	  _build/baardskeerder_mirage.cmi \
	  _build/baardskeerder_mirage.a

uninstall:
	$(OCAML_FIND) remove baardskeerder

coverage:
	ocamlbuild -use-ocamlfind \
	-tag 'package(bisect)' \
	-tag 'syntax(camlp4o -no_quot)' \
	-tag 'syntax(bisect_pp)' \
	test.byte

default: build
