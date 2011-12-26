#==============================================================================
# File     : GNUmakefile
# Purpose  : The eburg makefile.
# Author   : j@svirfneblin.org
# License  : BSD
# Created  : 2007-08-30 13:34:34 j
# Modified : $Id: GNUmakefile,v 1.1.1.1 2008/08/07 16:31:33 j Exp $
#==============================================================================

# Configurables
PREFIX    = /opt/lib/erlang


# Variables
PROGRAM   = eburg
VERSION   = 0.6
DIRS      = include src
DISTFILES = GNUmakefile LICENSE MANUAL NEWS README bin/ doc/ ebin/ include/ src/ t/
DISTNAME  = $(PROGRAM)-$(VERSION)
INSTALL   = $(PREFIX)/$(PROGRAM)


# Build Targets
.PHONY:	all clean distclean clobber test

all: src/eburg_lexer.erl src/eburg_parser.erl
	cd src && erl -make all

src/eburg_lexer.erl: src/eburg_lexer.xrl
	cd src && erl -noshell -eval "leex:file(eburg_lexer)" -s erlang halt

src/eburg_parser.erl: src/eburg_parser.yrl
	cd src && erlc eburg_parser.yrl

clean: clobber
distclean: clobber
clobber:
	rm -f `find . -name "*.beam"`
	rm -f `find . -name "erl_crash.dump"`
	rm -f src/eburg_lexer.erl
	rm -f src/eburg_parser.erl
	rm -f TAGS

test:
	cd t && ./test.sh -v


# Utility Targets
.PHONY:	doc check fluff dist tarball tags install uninstall

# doc: clobber
# 	erl -noshell -s edoc files `ls $(DIRS) | grep .[eh]rl` -s erlang halt
# 	@for d in $(DIRS);            \
# 	do                            \
#           cd $$d && mv *.html ../doc; \
# 	done

check: all
	dialyzer --verbose -c ebin

fluff:
	find include src -name "*.[eyx]rl" | xargs awk 'length > 80'
	find include src -name "*.[eyx]rl" | xargs awk '/[ ]+$$/' 

dist: tarball
tarball: clobber src/eburg_lexer.erl
	mkdir $(DISTNAME)
	cp -R $(DISTFILES) $(DISTNAME)
	tar -s /.*CVS.*// -czvf $(DISTNAME).tar.gz $(DISTNAME)
	rm -rf $(DISTNAME)
	md5 $(DISTNAME).tar.gz

tags: clobber
	erl -noshell -s tags dirs $(DIRS) -s erlang halt

install: all
	mkdir $(INSTALL)
	cp -R $(DISTFILES) $(INSTALL)
	@echo "You might want to add a line similar to\n\
code:add_pathz(\"$(INSTALL)/ebin\")\nto your ~/.erlang file."

uninstall:
	rm -rf $(INSTALL)


# Local Variables:
# mode: makefile-gmake
# End:
