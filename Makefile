
version=2.0.0b

install_prefix=/usr/local
escaped_prefix=\/usr\/local
bindir=$(install_prefix)/bin
sharedir=$(install_prefix)/share/coasim
escaped_sharedir=$(escaped_prefix)\/share\/coasim

all:
	cd src && make all
	cd gui && make all

test:
	cd src && make test
	# FIXME: no gui test

install: all
	cd src && make install bindir=$(bindir) sharedir=$(sharedir)

	cp gui/coasim_gui	$(bindir)

	cp coasim_to_dot.xsl	$(sharedir)
	cat coasim_to_ps | sed "s/'coasim_to_dot.xsl'/'$(escaped_sharedir)\/coasim_to_dot.xsl'/" > $(bindir)/coasim_to_ps
	cp coasim_separate.py	$(bindir)

dist:
	mkdir coasim-$(version)
	mkdir coasim-$(version)/src 
	cp src/*.cc src/*.hh   	coasim-$(version)/src
	cp src/makefile        	coasim-$(version)/src
	cp src/coasim.dtd      	coasim-$(version)/src
	cp src/coasim.py       	coasim-$(version)/src

	mkdir coasim-$(version)/gui
	mkdir coasim-$(version)/gui/.obj
	mkdir coasim-$(version)/gui/.moc
	cp gui/*.cc gui/*.hh    coasim-$(version)/gui
	cp gui/*.cpp gui/*.h    coasim-$(version)/gui
	cp gui/coasim_gui.pro   coasim-$(version)/gui
	cp gui/*.ui             coasim-$(version)/gui
	cp gui/Makefile         coasim-$(version)/gui

	cp Makefile 	       	coasim-$(version)
	cp COPYING 	       	coasim-$(version)
	cp README 		coasim-$(version)
	cp README_separate 	coasim-$(version)
	cp coasim_to_dot.xsl	coasim-$(version)
	cp coasim_to_ps		coasim-$(version)
	cp coasim_separate	coasim-$(version)


	tar zcvf coasim-$(version).tar.gz coasim-$(version)

	rm -rf coasim-$(version)
