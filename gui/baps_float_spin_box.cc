
#include "baps_float_spin_box.hh"

#ifndef CMATH_INCLUDED
# include <cmath>
# define CMATH_INCLUDED
#endif

BapsFloatSpinBox::BapsFloatSpinBox(QWidget *parent, const char *name)
    : QSpinBox(parent,name),
      i_decimals(2),
      i_double_validator(0,0,0,this)
{
    scaleRange();		// update scale factor
    updateDisplay();		// correctly format initial text
    setValidator(&i_double_validator);
}

BapsFloatSpinBox::~BapsFloatSpinBox()
{
}


void
BapsFloatSpinBox::setDecimals(unsigned int decimals)
{
    i_decimals = decimals;
    scaleRange();
}

unsigned int
BapsFloatSpinBox::decimals() const
{
    return i_decimals;
}


QString
BapsFloatSpinBox::mapValueToText(int value)
{
    double real_value = value / i_scale;
    return QString("%1").arg(real_value, 0, 'f', i_decimals);
}

int
BapsFloatSpinBox::mapTextToValue(bool *ok)
{
    double real_value = text().toFloat();
    int    value = int (real_value * i_scale + 0.5);
    *ok = true;
    return value;
}

void
BapsFloatSpinBox::scaleRange()
{
    i_scale = ::pow(10,i_decimals);
    double bottom = minValue()/i_scale;
    double top    = maxValue()/i_scale;
    i_double_validator.setRange(bottom,top,i_decimals);
    updateDisplay();
}
