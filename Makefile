coq: Makefile.coq
	$(MAKE) -f Makefile.coq

clean: Makefile.coq
	$(MAKE) -f Makefile.coq clean

Makefile.coq: _CoqProject Makefile
	coq_makefile -f _CoqProject -o Makefile.coq

dist:
	git archive --prefix=mirror-core/ -o mirror-core.tgz HEAD

install: coq
	$(MAKE) -f Makefile.coq install

deps.pdf:
	@ coqdep -dumpgraph deps.dot `sed '/COQLIB/d' _CoqProject` > /dev/null
	@ sed -i '/ext-lib/d' deps.dot
	@ dot -Tpdf deps.dot -o deps.pdf

.PHONY: all clean dist init coq deps.pdf check-imports universes todo admit

todo:
	git grep TODO

admit:
	git grep -i admit
