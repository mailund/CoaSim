
#ifndef BAPS_FLOAT_SPIN_BOX_HH_INCLUDED
#define BAPS_FLOAT_SPIN_BOX_HH_INCLUDED

#ifndef QSPINBOX_H_INCLUDED
# include <qspinbox.h>
# define QSPINBOX_H_INCLUDED
#endif
#ifndef QVALIDATOR_H_INCLUDED
# include <qvalidator.h>
# define QVALIDATOR_H_INCLUDED
#endif

class QDoubleValidator;
class BapsFloatSpinBox : public ::QSpinBox {
    Q_OBJECT

    Q_PROPERTY( uint   decimals READ decimals WRITE setDecimals )

    uint   i_decimals;
    double i_scale;

    QDoubleValidator i_double_validator;

public:
    BapsFloatSpinBox(QWidget *parent = 0, const char *name = 0);
    ~BapsFloatSpinBox();

    // properties controling the spin box
    void   setDecimals(uint decimals);
    uint   decimals() const;

private:
    // overriding representation functions
    virtual QString mapValueToText(int value);
    virtual int     mapTextToValue(bool *ok);

    void scaleRange();
};

#endif
