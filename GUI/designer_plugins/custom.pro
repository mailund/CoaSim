
PROJECTNAME = CoaSimCustomWidgets

CONFIG   += qt warn_on release exceptions plugin

TEMPLATE = lib
LANGUAGE = C++
DESTDIR = $(HOME)/.qt/plugins/designer

TARGET   = coasim_custom_widgets
target.path=$$plugins.path
isEmpty(target.path):target.path=$$QT_PREFIX/plugins

INSTALLS += target

INCLUDEPATH += ../custom_widgets
HEADERS  += ../custom_widgets/float_spin_box.hh
HEADERS  += ../custom_widgets/trait_or_snp_table.hh
SOURCES  += ../custom_widgets/float_spin_box.cc 
SOURCES  += ../custom_widgets/trait_or_snp_table.cc 
SOURCES  += custom_widgets.cc


unix {
  UI_DIR = .ui
  MOC_DIR = .moc
  OBJECTS_DIR = .obj
}

