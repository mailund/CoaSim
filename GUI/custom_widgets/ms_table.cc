
#include "ms_table.hh"
#include "float_spin_box.hh"
#include "position_checker.hh"

#include <qlineedit.h>
#include <cassert>

MSTable::MSTable(QWidget *parent, const char *name)
    : QTable(parent,name), checker(0)
{
}

MSTable::~MSTable()
{
    delete checker;
}

QWidget *
MSTable::createEditor(int row, int col, bool initFromCell) const
{
    switch (col)
	{
	case 0:
	    {
		FloatSpinBox *editor = new FloatSpinBox(viewport());
		editor->setDecimals(3);
		editor->setLineStep(10);
		editor->setMaxValue(999);
    
		if (initFromCell) 
		    {
			double d_val = text(row,col).toFloat();
			int    i_val = static_cast<int>(d_val * 1000 + 0.5);
			editor->setValue(i_val);
		    }

		return editor;
	    }
    
	case 1:
	    {
		QSpinBox *editor = new QSpinBox(viewport());
		editor->setMaxValue(100);
		editor->setMinValue(2);

		if (initFromCell) editor->setValue(text(row,col).toInt());

		return editor;
	    }

	case 2:
	    {
		FloatSpinBox *editor = new FloatSpinBox(viewport());
		editor->setDecimals(2);
		editor->setMaxValue(100);
		editor->setLineStep(10);

		if (initFromCell) 
		    {
			double d_val = text(row,col).toFloat();
			int    i_val = static_cast<int>(d_val * 100 + 0.5);
			editor->setValue(i_val);
		    }

		return editor;
	    }
	}

    // shouldn't reach here!
    assert(false);
    return 0;
}

void
MSTable::endEdit(int row, int col, bool accepted, bool wasReplacing)
{
    QSpinBox *editor = dynamic_cast<QSpinBox*>(cellWidget(row,col));
    if (!editor) return;

    int new_value = editor->value();
    QString string_value;
    switch (col)
	{
	case 0:
	    string_value.sprintf("0.%03d", new_value);
	    break;
	case 1:
	    string_value.sprintf("%d", new_value);
	    break;
	case 2:
	    string_value.sprintf("0.%02d", new_value);
	    break;
	}

    QTable::endEdit(row,col,false,wasReplacing);

    if (accepted) 
	{
	    // check validity
	    if (col == 0 and checker and !checker->check(editor->value())) return;
	    setText(row,col,string_value);
	}
}
