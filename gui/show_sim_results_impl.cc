
#include "show_sim_results_impl.hh"

#ifndef NODE_HH_INCLUDED
# include <node.hh>
#endif

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

ShowSimResultsImpl::ShowSimResultsImpl(Configuration &conf, ARG &arg,
				       QWidget* parent,  const char* name, 
				       WFlags fl)
  : ShowSimResultsForm( parent, name, fl ), i_conf(conf), i_arg(arg)
{
  unsigned int no_markers    = i_conf.no_markers();
  unsigned int no_haplotypes = i_conf.no_leaves();
  i_haplotype_table->setNumCols(no_markers);
  i_haplotype_table->setNumRows(no_haplotypes);

  for (unsigned int h = 0; h < no_haplotypes; ++h)
    for (unsigned int m = 0; m < no_markers; ++m)
      i_haplotype_table->setText(h,m, 
				 QString("%1").arg(arg.leaves()[h]->state(m)));
}

ShowSimResultsImpl::~ShowSimResultsImpl()
{
  // no need to delete child widgets, Qt does it all for us
}

void ShowSimResultsImpl::save_xml()
{
  QString fname = QFileDialog::getSaveFileName();
  bool leaves_only = mode->isOn();

 retry:
  std::ofstream output_file(fname);
  if (!output_file)
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
      return;
    }

  i_arg.to_xml(output_file,!leaves_only);
}

void ShowSimResultsImpl::save_text()
{
  QString fname = QFileDialog::getSaveFileName();

 retry:
  std::ofstream output_file(fname);
  if (!output_file)
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
      return;
    }

  i_arg.to_text(output_file);
}


