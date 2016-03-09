VERSION = $(shell grep 'Version:' _oasis | sed 's/Version: *//')
VFILE   = bin/version.ml
SETUP   = ocaml setup.ml
PREFIX ?= $(shell opam config var prefix)

.PHONY: build doc test all install uninstall reinstall clean distclean
.PHONY: configure $(VFILE)

build: setup.data $(VFILE)
	$(SETUP) -build $(BUILDFLAGS)

all: setup.data
	$(SETUP) -all $(ALLFLAGS)

setup.ml: _oasis
	rm -f _tags myocamlbuild.ml
	oasis setup
	echo 'true: debug, bin_annot' >> _tags
	echo 'true: warn_error(+1..49), warn(A-4-41-44)' >> _tags
	echo 'Ocamlbuild_plugin.mark_tag_used "tests"' >> myocamlbuild.ml

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test:
	$(SETUP) -configure --enable-tests --prefix $(PREFIX)
	$(MAKE) build
	$(SETUP) -test $(TESTFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	if [ -f setup.ml ]; then $(SETUP) -clean $(CLEANFLAGS); fi
	rm -f setup.data setup.ml myocamlbuild.ml _tags configure
	rm -f lib/*.odocl lib/META setup.log lib/*.mldylib lib/*.mllib
	rm -f $(VFILE)
	rm -rf _tests lib_test/_tests ./test-db

setup.data: setup.ml
	$(SETUP) -configure --prefix $(PREFIX)

$(VFILE):
	echo "let current = \"$(VERSION)\"" > $@

doc/html/.git:
	cd doc/html && (\
	  git init &&\
	  git remote add origin git@github.com:samoht/dog.git &&\
	  git checkout -b gh-pages)

gh-pages: doc/html/.git
	cd doc/html && git checkout gh-pages
	rm -f doc/html/*.html
	cp dog.docdir/*.html doc/html/
	cd doc/html && git add *.html
	cd doc/html && git commit -a -m "Doc updates"
	cd doc/html && git push origin gh-pages

NAME    = $(shell grep 'Name:' _oasis    | sed 's/Name: *//')
ARCHIVE = https://github.com/samoht/$(NAME)/archive/$(VERSION).tar.gz

release:
	git tag -a $(VERSION) -m "Version $(VERSION)."
	git push origin $(VERSION)
	$(MAKE) pr

pr:
	opam publish prepare $(NAME).$(VERSION) $(ARCHIVE)
	OPAMPUBLISHBYPASSCHECKS=1 OPAMYES=1 opam publish submit $(NAME).$(VERSION) && rm -rf $(NAME).$(VERSION)
