VERSION = $(shell grep 'Version:' _oasis | sed 's/Version: *//')
VFILE   = bin/version.ml
SETUP   = ocaml setup.ml

.PHONY: build doc test all install uninstall reinstall clean distclean configure

build: setup.data $(VFILE)
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)
	rm -f $(VFILE) $(SFILE)
	rm -rf lib_test/_tests
	rm -rf ./test-db

$(VFILE): _oasis
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
