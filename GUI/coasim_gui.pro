TEMPLATE	= app
LANGUAGE	= C++

CONFIG	+= qt warn_on release exceptions debug_on

unix:LIBS	+= -L../Core -lcoasim

INCLUDEPATH	+= custom_widgets

HEADERS	+= simulation_monitor.hh

SOURCES	+= simulation_monitor.cc

FORMS	= simulationdialog.ui \
	simresultsdialog.ui \
	mainwindow.ui \
	simulationparametersdialog.ui

IMAGES	= images/coasim-icon.png \
	images/CoaSim-splash.png \
	images/bioinformatics-exit-icon.png \
	images/bioinformatics-run-icon.png \
	images/bioinformatics-delete-icon.png \
	images/bioinformatics-clear-icon.png

INCLUDEPATH += ../


HEADERS	+= custom_widgets/float_spin_box.hh
HEADERS	+= custom_widgets/trait_or_snp_table.hh
HEADERS	+= custom_widgets/ms_table.hh

SOURCES	+= custom_widgets/float_spin_box.cc
SOURCES	+= custom_widgets/trait_or_snp_table.cc
SOURCES	+= custom_widgets/ms_table.cc




SOURCES += main.cc

unix {
  UI_DIR = .ui
  MOC_DIR = .moc
  OBJECTS_DIR = .obj
}

