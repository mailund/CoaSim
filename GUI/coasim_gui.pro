TEMPLATE	= app
LANGUAGE	= C++

CONFIG	+= qt warn_on release exceptions

unix:LIBS	+= -L../Core -lcoasim

INCLUDEPATH	+= ../../QtWidgets/

HEADERS	+= ../../QtWidgets/baps_float_spin_box.hh

SOURCES	+= ../../QtWidgets/baps_float_spin_box.cc

FORMS	= maindialog.ui \
	simulationdialog.ui \
	simresultsdialog.ui

HEADERS	+= simulation_monitor.hh 

SOURCES	+= simulation_monitor.cc


INCLUDEPATH += ../


SOURCES += main.cc

unix {
  UI_DIR = .ui
  MOC_DIR = .moc
  OBJECTS_DIR = .obj
}

