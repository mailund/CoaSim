
//#include "maindialog.h"
#include "mainwindow.h"

#include <qapplication.h>
#include <qmessagebox.h>
#include <qsplashscreen.h>
#include <qsettings.h>

#include <stdexcept>

QApplication *coasim_main_app;

int main( int argc, char* argv[] )
{
    coasim_main_app = new QApplication(argc, argv);

    QSplashScreen *splash =
	new QSplashScreen(QPixmap::fromMimeSource("CoaSim-splash.png"));
    splash->show();

    splash->message(QObject::tr("Setting up main window..."),
		    Qt::AlignRight | Qt::AlignTop, Qt::white);

    MainWindow main_window(0, "CoaSim");
    coasim_main_app->setMainWidget(&main_window);

    splash->message(QObject::tr("Reading settings..."),
		    Qt::AlignRight | Qt::AlignTop, Qt::white);
    sleep(1); // cheating...

    QSettings *settings = new QSettings();
    settings->setPath("bioinformatics.dk","CoaSim");
    settings->beginGroup("/CoaSim");
    main_window.noLeaves = settings->readNumEntry("/noLeaves", 500);
    main_window.rho      = settings->readDoubleEntry("/rho", 500.0); 
    main_window.G        = settings->readDoubleEntry("/G", 0.0); 
    main_window.Q        = settings->readDoubleEntry("/Q", 0.0); 
    main_window.beta     = settings->readDoubleEntry("/beta", 0.0); 
    settings->endGroup();
    delete settings;

    main_window.show();
    splash->finish(&main_window);
    delete splash;

    try {
	int ret = coasim_main_app->exec();

	// save settings for next session...
	QSettings *settings = new QSettings();
	settings->setPath("bioinformatics.dk","CoaSim");
	settings->beginGroup("/CoaSim");
	// FIXME: this only works as long as we stick to a single
	// window.  later on, it should probably be handled explicitly
	// in a menu or something...
	settings->writeEntry("/noLeaves", main_window.noLeaves); 
	settings->writeEntry("/rho",      main_window.rho); 
	settings->writeEntry("/G",        main_window.G); 
	settings->writeEntry("/Q",        main_window.Q); 
	settings->writeEntry("/beta",     main_window.beta); 
	settings->endGroup();
	delete settings;

	return ret;

    } catch (std::exception &ex) {
	QMessageBox::critical(0, QObject::tr("Unexpected Exception!"),
			      QString(QObject::tr("The exception \"%1\" was "
					 "unexpectedly throw.\n"
					 "Please report this to "
					 "<mailund@birc.dk>.\n"
					 "Thank you.")).arg(ex.what()),
			      QObject::tr("Shut down"));
	return 2;
    }
}
