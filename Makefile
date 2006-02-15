
all:
	(cd Core  ; make)
	(cd Guile ; make)
	(cd Python ; python setup.py build)
	(cd GUI ; qmake ; make)

check:
	(cd Core  ; make check)
	(cd Guile ; make check)
	(cd Python ; ./run-tests.sh)
	# FIXME: no gui test...

.phony: all check
