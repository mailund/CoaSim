
#ifndef TRAIT_OR_SNP_TABLE_HH_INCLUDED
#define TRAIT_OR_SNP_TABLE_HH_INCLUDED

#ifndef POSITION_CHECKER_HH_INCLUDED
# include "position_checker.hh"
#endif

#ifndef QTABLE_H_INCLUDED
# include "qtable.h"
# define QTABLE_H_INCLUDED
#endif

class TraitOrSNPTable : public ::QTable {
    Q_OBJECT

public:
    TraitOrSNPTable(QWidget *parent = 0, const char *name = 0);
    ~TraitOrSNPTable();

    // set checker -- steals the reference
    void setPositionChecker(PositionChecker *c) { checker = c; }

protected:
    QWidget *createEditor(int row, int col, bool initFromCell) const;
    void endEdit(int row, int col, bool accepted, bool wasReplacing);

private:
    PositionChecker *checker;
};

#endif