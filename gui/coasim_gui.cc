
#include <qapplication.h>
#include "coasim_gui_impl.hh"

int main( int argc, char* argv[] )
{
  QApplication app( argc, argv );

  CoasimGuiImpl gui(0, "CoaSim");
  app.setMainWidget( &gui );
  gui.show();

  int ret = app.exec();
  return ret;
}
