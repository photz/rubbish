file=game.lisp
sbclflags=

.PHONY: watch server

watch:
	echo game.lisp \
	| entr sbcl $(sbclflags) --script $(file)

server:
	echo server.lisp \
	| entr -r sbcl $(sbclflags) --script server.lisp
