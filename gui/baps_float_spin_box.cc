
#include "baps_float_spin_box.hh"

#ifndef CMATH_INCLUDED
# include <cmath>
# define CMATH_INCLUDED
#endif

BapsFloatSpinBox::BapsFloatSpinBox(QWidget *parent, const char *name)
    : QSpinBox(parent,name),
      _decimals(2),
      _double_validator(0,0,0,this)
{
    scaleRange();		// update scale factor
    updateDisplay();		// correctly format initial text
    setValidator(&_double_validator);
}

BapsFloatSpinBox::~BapsFloatSpinBox()
{
}


void
BapsFloatSpinBox::setDecimals(unsigned int decimals)
{
    _decimals = decimals;
    scaleRange();
}

unsigned int
BapsFloatSpinBox::decimals() const
{
    return _decimals;
}


QString
BapsFloatSpinBox::mapValueToText(int value)
{
    double real_value = value / _scale;
    return QString("%1").arg(real_value, 0, 'f', _decimals);
}

int
BapsFloatSpinBox::mapTextToValue(bool *ok)
{
    double real_value = text().toFloat();
    int    value = int (real_value * _scale + 0.5);
    *ok = true;
    return value;
}

void
BapsFloatSpinBox::scaleRange()
{
    _scale = ::pow(10,_decimals);
    double bottom = minValue()/_scale;
    double top    = maxValue()/_scale;
    _double_validator.setRange(bottom,top,_decimals);
    updateDisplay();
}
