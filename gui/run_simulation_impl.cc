
#include "run_simulation_impl.hh"

#include <qtable.h>
#include <qcheckbox.h>
#include <qlineedit.h>
#include <qfiledialog.h>
#include <qmessagebox.h>

#include <fstream>
#include <vector>

RunSimulationImpl::RunSimulationImpl(QString &output_file, bool &leaves_only,
				     QWidget* parent,  const char* name, 
				     WFlags fl)
  : RunSimulationForm( parent, name, fl ),
    _output_file(output_file), _leaves_only(leaves_only)
{
  // set "abort" defaults
  _output_file = "";
  _leaves_only = true;
}

RunSimulationImpl::~RunSimulationImpl()
{
  // no need to delete child widgets, Qt does it all for us
}

void RunSimulationImpl::set_out_file()
{
  QString fname = QFileDialog::getSaveFileName();
  if (fname == "") return; // cancel
  xml_file_name->setText(fname);
}


void RunSimulationImpl::run_simulation()
{
  QString fname = xml_file_name->text();
  bool leaves_only = mode->isOn();

 retry:
  std::ofstream xml_file(fname);
  if (!xml_file)
    {
      if (QMessageBox::warning(this, "Error opening file",
			       QString("Could not open file ")
			       .append(fname),
			       "Retry", "Cancel",
			       0, 0, 1) == 0)
	{
	  fname = QFileDialog::getSaveFileName();
	  if (fname == "") return; // cancel    
	  goto retry;
	}
      close();
      return;
    }

  // set return values
  _output_file = fname;
  _leaves_only = leaves_only;

  // close run sim. dialog
  close();
}
