
#include <qapplication.h>
#include "coasim_gui_impl.hh"

QApplication *coasim_main_app;

int main( int argc, char* argv[] )
{
  coasim_main_app = new QApplication(argc, argv);

  CoasimGuiImpl gui(0, "CoaSim");
  coasim_main_app->setMainWidget(&gui);
  gui.show();

  int ret = coasim_main_app->exec();
  return ret;
}
