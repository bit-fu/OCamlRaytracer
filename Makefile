# vim:sts=8 sw=8 ts=8 noexpandtab:
#
#   Makefile
#   ~~~~~~~~
#
#   Project:		OCamlRaytracer.git
#
#   Target language:    OCaml-4.03+flambda
#
#   Created 2016-06-28:	Ulrich Singer
#

# Tools
#PATH		= $(PATH)
SHELL		= /bin/sh
INSTALL		= /usr/bin/install -C

OPT		= -safe-string
LNK		= -package unix,graphics -linkpkg

MKBIN		= ocamlfind ocamlopt $(LNK) $(OPT) -O3 -unbox-closures
MKTOP		= ocamlfind ocamlmktop $(LNK) $(OPT) -short-paths

SRCS		= raytrax.ml


# Targets
.PHONY: all clean

all: raytrax repl tags

raytrax: $(SRCS)
	$(MKBIN) -o $@ $^

repl: $(SRCS)
	$(MKTOP) -o $@ $^

clean:
	$(RM) *.o *.cm? raytrax repl

tags: *.ml
	ctags $^


# ~ Makefile ~ #
