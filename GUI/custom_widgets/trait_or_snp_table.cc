
#include "trait_or_snp_table.hh"
#include "float_spin_box.hh"
#include "position_checker.hh"

//#include "mainwindow.h"

#include <qlineedit.h>
#include <cassert>

TraitOrSNPTable::TraitOrSNPTable(QWidget *parent, const char *name)
    : QTable(parent,name), checker(0)
{
}

TraitOrSNPTable::~TraitOrSNPTable()
{
    delete checker;
}

QWidget *
TraitOrSNPTable::createEditor(int row, int col, bool initFromCell) const
{
    FloatSpinBox *editor = new FloatSpinBox(viewport());
    double scale = 0;

    switch (col)
	{
	case 0:
	    scale = 1000;
	    editor->setDecimals(3);
	    editor->setLineStep(10);
	    editor->setMaxValue(999);
	    break;
	case 1:
	case 2:
	    scale = 100;
	    editor->setDecimals(2);
	    editor->setMaxValue(100);
	    editor->setLineStep(10);
	    break;
	}

    if (initFromCell) 
	{
	    double d_val = text(row,col).toFloat();
	    int    i_val = static_cast<int>(d_val * scale + 0.5);
	    editor->setValue(i_val);
	}

    return editor;
}

void
TraitOrSNPTable::endEdit(int row, int col, bool accepted, bool wasReplacing)
{
    FloatSpinBox *editor = dynamic_cast<FloatSpinBox*>(cellWidget(row,col));
    if (!editor) return;

    int new_value = editor->value();
    QString string_value;
    switch (col)
	{
	case 0:
	    string_value.sprintf("0.%03d", new_value);
	    break;
	case 1:
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
