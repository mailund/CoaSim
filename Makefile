
version=2-0-0

all:
	cd src && make all
	cd gui && make all

test:
	cd src && make test
	# FIXME: no gui test

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
	cp README 		coasim-$(version)
	cp README_separate 	coasim-$(version)
	cp coasim_to_dot.xsl	coasim-$(version)
	cp separate.py		coasim-$(version)


	tar zcvf coasim-$(version).tar.gz coasim-$(version)

	rm -rf coasim-$(version)
