
#ifndef MS_TABLE_HH_INCLUDED
#define MS_TABLE_HH_INCLUDED

#ifndef POSITION_CHECKER_HH_INCLUDED
# include "position_checker.hh"
#endif

#ifndef QTABLE_H_INCLUDED
# include "qtable.h"
# define QTABLE_H_INCLUDED
#endif

class MSTable : public ::QTable {
    Q_OBJECT

public:
    MSTable(QWidget *parent = 0, const char *name = 0);
    ~MSTable();

    // set checker -- steals the reference
    void setPositionChecker(PositionChecker *c) { checker = c; }

protected:
    QWidget *createEditor(int row, int col, bool initFromCell) const;
    void endEdit(int row, int col, bool accepted, bool wasReplacing);

private:
    PositionChecker *checker;
};

#endif
