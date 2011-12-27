### Makefile --- 

## Author: bob@bobmatoMacBook-Pro.local
## Version: $Id: Makefile,v 0.0 2011/12/25 19:52:23 bob Exp $
## Keywords: 
## X-URL: 

CLEANSCRIPT=./_build/sanitize.sh
need_clean := $(wildcard $CLEANSCRIPT)
term : 
	  ocamlbuild -cflags -w,aPF fc_syntax.cmo

parse : 
	# # ifeq ($(strip $(need_clean)),)
	# # 	echo "already cleaned"
	# # else 
	# # 	$CLEANSCRIPT
	# # endif 
	ocamlbuild fc_l.inferred.mli
	mv ./_build/fc_l.inferred.mli ./fc_l.mli
	ocamlbuild  fc_l.byte -- 
prj:
	ocamlbuild prj.cma

all:  parse prj