
#include "maindialog.h"

#include <qapplication.h>
#include <qmessagebox.h>

#include <stdexcept>

QApplication *coasim_main_app;

int main( int argc, char* argv[] )
{
    coasim_main_app = new QApplication(argc, argv);

    MainDialog dialog(0, "CoaSim");
    coasim_main_app->setMainWidget(&dialog);
    dialog.show();

    try {
	return coasim_main_app->exec();
    } catch (std::exception &ex) {
	QMessageBox::critical(0, "UNEXPECTED EXCEPTION!",
			      QString("The exception \"%1\" was unexpectedly "
				      "throw.\n"
				      "Please report this to "
				      "<mailund@birc.dk>.\n"
				      "Thank you.").arg(ex.what()),
			      "Shut down");
	return 2;
    }
}
