
#ifndef TRAIT_OR_SNP_TABLE_HH_INCLUDED
#define TRAIT_OR_SNP_TABLE_HH_INCLUDED

#ifndef QTABLE_H_INCLUDED
# include "qtable.h"
# define QTABLE_H_INCLUDED
#endif

class TraitOrSNPTable : public ::QTable {
public:
    TraitOrSNPTable(QWidget *parent = 0, const char *name = 0);
    ~TraitOrSNPTable();
};

#endif
