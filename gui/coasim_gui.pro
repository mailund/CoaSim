SOURCES	+= coasim_gui.cc \
	coasim_gui_impl.cc \
        add_marker_impl.cc \
        sim_feedback_impl.cc \
        show_sim_results_impl.cc \
        baps_float_spin_box.cc
HEADERS	+= coasim_gui_impl.hh \
        add_marker_impl.hh \
        sim_feedback_impl.hh \
        show_sim_results_impl.hh \
        baps_float_spin_box.hh

unix {
  UI_DIR = .
  MOC_DIR = .moc
  OBJECTS_DIR = .obj
}
FORMS	= coasim_gui_form.ui addmarkerform.ui \
          simfeedbackform.ui showsimresults.ui
TEMPLATE = app
CONFIG	+= qt warn_on release exceptions 
LANGUAGE = C++
INCLUDEPATH += ../src/
LIBS += ../src/libcoasim.a
