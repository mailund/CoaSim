/****************************************************************************
 ** ui.h extension file, included from the uic-generated form implementation.
 **
 ** If you want to add, delete, or rename functions or slots, use
 ** Qt Designer to update this file, preserving your code.
 **
 ** You should not define a constructor or destructor in this file.
 ** Instead, write your code in functions called init() and destroy().
 ** These will automatically be called by the form's constructor and
 ** destructor.
 *****************************************************************************/

#include "simulationdialog.h"
#include "simulationparametersdialog.h"
#include <qmessagebox.h>

void MainWindow::init()
{
    noLeaves = 500;
    rho = 500.0;
    G = 0;
    Q = 0;
    beta = 0;

    // set sizes for traits and SNPs to match the larger MS spinboxes
    traitPosition->setMinimumSize(MStheta->sizeHint());
    SNPPosition->setMinimumSize(MStheta->sizeHint());

    // and the same for buttons -- setting them to the larger SNP button
    addTraitButton->setMinimumSize(addSNPButton->sizeHint());
    addMSButton->setMinimumSize(addSNPButton->sizeHint());
}

void MainWindow::fileExit()
{
    close();
}

void MainWindow::helpAbout()
{
    QMessageBox::about(this,
		       tr("About CoaSim"),
		       tr("<h2>CoaSim 3.0</h2>"

			  "<p>Copyright &copy; 2004 Bioinformatics ApS "
			  "<tt>http://www.bioinformatics.dk</tt>."

			  "<p>CoaSim is an ancestral recombination graph "
			  "simulator that simulates evolution under "
			  "various population and mutation models."
     
			  "<p>If you have any questions or comments, "
			  "please direct them to <tt>mailund@birc.dk</tt>."
			  ));
}

int MainWindow::updateNextPosition( int pos )
{
    int curNextPos =  nextPosSlider->value();
    if (pos != curNextPos) return curNextPos;
    
    int step = nextPositionStepSize->value();
    int nextPos = curNextPos + step;
    
    nextPosSlider->setValue(nextPos);
    return nextPos;
}

static bool checkTable(int pos, QTable *table, QString markerType)
{
    // FIXME: we use linear search in the marker tables here -- if
    // this is too slow, use binary search (we keep the markers
    // sorted), but I doubt that this will be a problem.
    
    int noRows = table->numRows();
    for (int i = 0; i < noRows; ++i) { 
	int rowPos = table->text(i,0).mid(2).toInt();
	if (rowPos == pos)  {
	    QMessageBox::warning(0, QObject::tr("Position Error"),
				 QString(QObject::tr("The chosen position is already occupied by a %1 marker.")).arg(markerType),
				 "OK");
	    return false;
	} 
	if (rowPos > pos) return true; // safe since the table is sorted
    }
    return true;
}

bool MainWindow::checkMarkerPosition( int pos )
{
    if (!checkTable(pos, traitMarkerTable, tr("trait"))) return false;
    if (!checkTable(pos, SNPMarkerTable, tr("SNP"))) return false;
    if (!checkTable(pos, MSMarkerTable, tr("micro-satellite"))) return false; 
    return true;
}


void MainWindow::addTraitMarker()
{
    int pos = traitPosition->value();
    if (!checkMarkerPosition(pos)) return;
    
    traitMarkerTable->insertRows(0);
    traitMarkerTable->setText(0,0,QString("").sprintf("0.%03d", pos));
    traitMarkerTable->setText(0,1,traitLowFreq->text());
    traitMarkerTable->setText(0,2,traitHighFreq->text());
    
    traitMarkerTable->sortColumn(0,true,true);
    
    traitPosition->setValue(updateNextPosition(pos));
}


void MainWindow::addSNPMarker()
{
    int pos = SNPPosition->value();
    if (!checkMarkerPosition(pos)) return;
    
    SNPMarkerTable->insertRows(0);
    SNPMarkerTable->setText(0,0,QString("").sprintf("0.%03d", pos));
    SNPMarkerTable->setText(0,1,SNPLowFreq->text());
    SNPMarkerTable->setText(0,2,SNPHighFreq->text());
    
    SNPMarkerTable->sortColumn(0,true,true);
    
    SNPPosition->setValue(updateNextPosition(pos));
}


void MainWindow::addMSMarker()
{
    int pos = MSPosition->value();
    if (!checkMarkerPosition(pos)) return;
    
    MSMarkerTable->insertRows(0);
    MSMarkerTable->setText(0,0,QString("").sprintf("0.%03d", pos));
    MSMarkerTable->setText(0,1,MSAlphabetSize->text());
    MSMarkerTable->setText(0,2,MStheta->text());
    
    MSMarkerTable->sortColumn(0,true,true);
    
    MSPosition->setValue(updateNextPosition(pos));
}


void MainWindow::nextPosChanged( int pos )
{
    nextPos->setText(QString("").sprintf("0.%03d",pos));
}





void MainWindow::runSimulation()
{
    SimulationDialog *simulation = 
	new SimulationDialog(this, "Simulate", false, WDestructiveClose);
    simulation->show();
}


void MainWindow::editSimulationParameters()
{
    SimulationParametersDialog dialog(this);

    dialog.noChromosomes->setValue(noLeaves);
    dialog.rho->setValue((int)rho*100);
    dialog.Q->setValue((int)Q*100);
    dialog.G->setValue((int)G*100);
    dialog.beta->setValue((int)beta*100);

    if (dialog.exec()) 
	{
	    noLeaves = dialog.noChromosomes->text().toInt();
	    rho = dialog.rho->text().toDouble();
	    Q = dialog.Q->text().toDouble();
	    G = dialog.G->text().toDouble();
	    beta = dialog.beta->text().toDouble();
	}
}
