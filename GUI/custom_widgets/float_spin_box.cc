
#include "float_spin_box.hh"

#ifndef CMATH_INCLUDED
# include <cmath>
# define CMATH_INCLUDED
#endif

FloatSpinBox::FloatSpinBox(QWidget *parent, const char *name)
    : QSpinBox(parent,name),
      i_decimals(2),
      i_double_validator(0,0,0,this)
{
    scaleRange();		// update scale factor
    updateDisplay();		// correctly format initial text
    setValidator(&i_double_validator);
}

FloatSpinBox::~FloatSpinBox()
{
}


void
FloatSpinBox::setDecimals(unsigned int decimals)
{
    i_decimals = decimals;
    scaleRange();
}

unsigned int
FloatSpinBox::decimals() const
{
    return i_decimals;
}


QString
FloatSpinBox::mapValueToText(int value)
{
    double real_value = value / i_scale;
    return QString("%1").arg(real_value, 0, 'f', i_decimals);
}

int
FloatSpinBox::mapTextToValue(bool *ok)
{
    double real_value = text().toFloat();
    int    value = int (real_value * i_scale + 0.5);
    *ok = true;
    return value;
}

void
FloatSpinBox::scaleRange()
{
    i_scale = ::pow(10,i_decimals);
    double bottom = minValue()/i_scale;
    double top    = maxValue()/i_scale;
    i_double_validator.setRange(bottom,top,i_decimals);
    updateDisplay();
}
