
version=2.0.0

install_prefix=/usr/local
escaped_prefix=\/usr\/local
bindir=$(install_prefix)/bin

# the shared dir is where we put the DTD; if the coasim.dtd file is
# not found in this place, the xml processing will not work
sharedir=$(install_prefix)/share/coasim
escaped_sharedir=$(escaped_prefix)\/share\/coasim

makeflags=bindir=$(bindir) sharedir=$(sharedir)

all: make_cli make_gui

make_cli:
	cd src && make all $(makeflags)

make_gui:
	cd gui && make all $(makeflags)

test:
	cd src && make test $(makeflags)
	# FIXME: no gui test

install: install_cli install_gui
	cp coasim_to_dot.xsl	$(sharedir)
	cat coasim_to_ps | sed "s/'coasim_to_dot.xsl'/'$(escaped_sharedir)\/coasim_to_dot.xsl'/" > $(bindir)/coasim_to_ps
	chmod a+x $(bindir)/coasim_to_ps
	cp coasim_separate	$(bindir)

install_cli:
	cd src && make install $(makeflags)

install_gui:
	cp gui/coasim_gui	$(bindir)


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
	cp INSTALL 		coasim-$(version)
	cp README_separate 	coasim-$(version)
	cp coasim_to_dot.xsl	coasim-$(version)
	cp coasim_to_ps		coasim-$(version)
	cp coasim_separate	coasim-$(version)


	tar zcvf coasim-$(version).tar.gz coasim-$(version)

	rm -rf coasim-$(version)
