
#include "run_simulation_impl.hh"

#ifndef QTABLE_H_INCLUDED
# include <qtable.h>
# define QTABLE_H_INCLUDED
#endif
#ifndef QCHECKBOX_H_INCLUDED
# include <qcheckbox.h>
# define QCHECKBOX_H_INCLUDED
#endif
#ifndef QLINEEDIT_H_INCLUDED
# include <qlineedit.h>
# define QLINEEDIT_H_INCLUDED
#endif
#ifndef QFILEDIALOG_H_INCLUDED
# include <qfiledialog.h>
# define QFILEDIALOG_H_INCLUDED
#endif
#ifndef QMESSAGEBOX_H_INCLUDED
# include <qmessagebox.h>
# define QMESSAGEBOX_H_INCLUDED
#endif

#ifndef FSTREAM_INCLUDED
# include <fstream>
# define FSTREAM_INCLUDED
#endif

RunSimulationImpl::RunSimulationImpl(QString &output_file, bool &leaves_only,
				     QWidget* parent,  const char* name, 
				     WFlags fl)
  : RunSimulationForm( parent, name, fl ),
    i_output_file(output_file), i_leaves_only(leaves_only)
{
  // set "abort" defaults
  i_output_file = "";
  i_leaves_only = true;
}

RunSimulationImpl::~RunSimulationImpl()
{
  // no need to delete child widgets, Qt does it all for us
}

void RunSimulationImpl::set_out_file()
{
  QString fname = QFileDialog::getSaveFileName();
  if (fname == "") return; // cancel
  i_xml_file_name->setText(fname);
}


void RunSimulationImpl::run_simulation()
{
  QString fname = i_xml_file_name->text();
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
  i_output_file = fname;
  i_leaves_only = leaves_only;

  // close run sim. dialog
  close();
}
