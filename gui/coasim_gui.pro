SOURCES	+= coasim_gui.cc \
	coasim_gui_impl.cc \
        add_marker_impl.cc \
        run_simulation_impl.cc
HEADERS	+= coasim_gui_impl.hh \
        add_marker_impl.hh \
        run_simulation_impl.hh
unix {
  UI_DIR = .ui
  MOC_DIR = .moc
  OBJECTS_DIR = .obj
}
FORMS	= coasim_gui_form.ui addmarkerform.ui runsimulationform.ui
TEMPLATE	=app
CONFIG	+= qt warn_on release exceptions 
LANGUAGE	= C++
LIBS += -lcoasim
