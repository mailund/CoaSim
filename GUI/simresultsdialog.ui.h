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

#include <Core/node.hh>

#include <qfiledialog.h>
#include <qmessagebox.h>

#include <fstream>


void SimResultsDialog::saveText()
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

    i_arg->to_text(output_file);
}
