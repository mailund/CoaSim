
all:
	(cd Core  ; make)
	(cd Guile ; make)
	(cd GUI ; qmake ; make)

check:
	(cd Core  ; make check)
	(cd Guile ; make check)
	# FIXME: no gui test...

.phony: all check
