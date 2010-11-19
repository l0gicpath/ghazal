# This simple Makefile uses rebar to compile/install/clean if it
# exists, else does it explicitly.

APP = ghazal
EBINDIR = ebin
SRCDIR = src
INCDIR = include
DOCDIR = doc

VPATH = $(SRCDIR)

ERLCFLAGS = -W0 +debug_info
ERLC = erlc -I $(INCDIR) -o $(EBINDIR) $(ERLCFLAGS)

## The .erl and .beam files
SRCS = $(notdir $(wildcard $(SRCDIR)/*.erl))
EBINS = $(SRCS:.erl=.beam)

INSTALLDIR = "$$ERL_LIBS"/ghazal

all: compile docs

compile: lexer parser
	if which -s rebar; \
	then rebar compile; \
	else $(ERLC) $(addprefix $(SRCDIR)/, $(SRCS)); \
	fi

lexer:
	$(ERLC) $(SRCDIR)/$(APP)_leex.erl
	cd $(SRCDIR); erl -noshell -pa ../$(EBINDIR) -s $(APP)_leex gen_lexer -s init stop; cd ..

parser:
	$(ERLC) $(SRCDIR)/$(APP)_yecc.erl
	cd $(SRCDIR); erl -noshell -pa ../$(EBINDIR) -s $(APP)_yecc gen_parser -s init stop; cd ..

install:
	if which -s rebar; \
	then rebar install; \
	elif [ "$$ERL_LIBS" != "" ]; \
	then mkdir -p $(INSTALLDIR)/$(EBINDIR) ; \
	     cp -a $(EBINDIR) $(INSTALLDIR); \
	     cp -a $(EMACSDIR) $(INSTALLDIR); \
	else exit 1; \
	fi

docs:

clean:
	if which -s rebar; \
	then rebar clean; \
	else rm -rf $(EBINDIR)/*.beam; \
	fi
	rm -rf erl_crash.dump

test: clean
	$(ERLC) -DTEST -I test/ src/*.erl
	erl -pa $(EBINDIR) -noshell -s ghazal_leex test -s ghazal_yecc test -s init stop
