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

#include <qframe.h>

void MainDialog::init()
{
   extraSimParameters->hide();
}

void MainDialog::nextPosChanged( int pos )
{
    nextPos->setText(QString("").sprintf("0.%03d",pos));
    traitPosition->setValue(pos);
    SNPPosition->setValue(pos);
    MSPosition->setValue(pos);
}


int MainDialog::updateNextPosition( int pos )
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
     QMessageBox::warning(0, "Position Error",
     QString("The chosen position is already occupied by a %1 marker.").arg(markerType),
     "OK");
     return false;
 } 
 if (rowPos > pos) return true; // safe since the table is sorted
    }
    return true;
}

bool MainDialog::checkMarkerPosition( int pos )
{
    if (!checkTable(pos, traitMarkerTable, "trait")) return false;
    if (!checkTable(pos, SNPMarkerTable, "SNP")) return false;
    if (!checkTable(pos, MSMarkerTable, "micro-satellite")) return false; 
    return true;
}


void MainDialog::addTraitMarker()
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


void MainDialog::addSNPMarker()
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


void MainDialog::addMSMarker()
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

#include <iostream>


void MainDialog::runSimulation()
{
    SimulationDialog *simulation = 
 new SimulationDialog(this, "Simulate", false, WDestructiveClose);
    simulation->show();
}
