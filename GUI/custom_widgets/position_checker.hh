
#ifndef POSITION_CHECKER_HH_INCLUDED
#define POSITION_CHECKER_HH_INCLUDED

// hook for tying the position check into the main application
struct PositionChecker {
    virtual bool check(int pos) = 0;
    virtual ~PositionChecker() {}
};

#endif
