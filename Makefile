# Core names, package and collection(s)
PACKAGENAME=ansi-terminal
COLLECTS=

SRCDIR=./$(COLLECTS)

# The root for all directly generated files
BUILDOUTDIR=$(PWD)/build

# The `package` target output files.
PACKAGEFILE=$(BUILDOUTDIR)/$(PACKAGENAME).zip
CHECKSUMFILE=$(PACKAGEFILE).CHECKSUM

# The `test` target test-case directory
TESTDIR=$(COLLECTS)/tests
COVEROUTDIR=$(BUILDOUTDIR)/coverage

# The `doc` (`html`, `pdf`, and `markdown`) target inputs
SCRBLDIR=$(SRCDIR)/scribblings
SCRBLSRC=$(SCRBLDIR)/$(PACKAGENAME).scrbl

# The `doc` (`html`, `pdf`, and `markdown`) target outputs
HTMLOUTDIR=$(BUILDOUTDIR)/html
PDFOUTFILE=$(BUILDOUTDIR)/$(PACKAGENAME).pdf
MARKDOWNOUTFILE=$(BUILDOUTDIR)/$(PACKAGENAME).md

# ==============================================================================
# The default target
# ==============================================================================
all: setup test

$(BUILDOUTDIR):
	mkdir -p $(BUILDOUTDIR)

# ==============================================================================
# Packaging targets
# ==============================================================================
setup:
	raco setup \
		--tidy --check-pkg-deps --unused-pkg-deps \
		--only $(PACKAGENAME)

link:
	raco pkg install \
		--link --auto \
		--name $(PACKAGENAME) $(PWD)

unlink:
	raco pkg remove $(PACKAGENAME)

package: check test doc
	raco pkg create \
		--built --dest $(BUILDOUTDIR) \
		-- $(COLLECTS)
	mv $(BUILDOUTDIR)/core.zip $(PACKAGEFILE)
	mv $(BUILDOUTDIR)/core.zip.CHECKSUM $(CHECKSUMFILE)

# ==============================================================================
# Tests, and related targets
# ==============================================================================
test:
	raco test \
		--make --table \
		--collection $(PACKAGENAME)

# raco pkg install --force --name cover
coverage:
	raco cover \
		--exclude-pkg-basics --format coveralls \
		--directory $(COVEROUTDIR) \
		--package $(-range)

check:
	raco check-requires $(SRCDIR)*.rkt

# ==============================================================================
# Documentation generation targets
# ==============================================================================
doc: html $(PDFOUTFILE) $(MARKDOWNOUTFILE)

html: $(SCRBLSRC)
	raco scribble \
		--make \
		--html \
		--dest $(HTMLOUTDIR) --dest-name index \
		++main-xref-in \
		--redirect-main http://docs.racket-lang.org/ \
		$(SCRBLSRC)

$(PDFOUTFILE): $(SCRBLSRC)
	raco scribble \
		--make \
		--xelatex \
		--dest $(BUILDOUTDIR) \
		--redirect-main http://docs.racket-lang.org/ \
		$(SCRBLSRC)

$(MARKDOWNOUTFILE): $(SCRBLSRC)
	raco scribble \
		--make \
		--markdown \
		--dest $(BUILDOUTDIR) \
		--redirect-main http://docs.racket-lang.org/ \
		$(SCRBLSRC)

viewdocs:
	raco docs

# ==============================================================================
# Clean all temporary artifacts
# ==============================================================================
clean:
	find . -name compiled -type d | xargs rm -rf
	rm -rf $(BUILDOUTDIR)
	rm -rf $(COLLECTS)/doc
