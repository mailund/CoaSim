#ifndef RUN_SIMULATION_IMPL_H
#define RUN_SIMULATION_IMPL_H
#include "runsimulationform.h"

class QTable;

class RunSimulationImpl : public RunSimulationForm
{
  Q_OBJECT
  QTable *_marker_table;
  int     _no_leaves;
  double  _recomb_rate;
  double  _geneconv_rate;
  double  _geneconv_length;
  double  _growth;
  double  _mrate;

public:
  RunSimulationImpl(QTable *marker_table,
		    int no_leaves,
		    double recomb_rate,
		    double geneconv_rate, 
		    double geneconv_length,
		    double growth,
		    double mrate, 
		    QWidget* parent = 0, const char* name = 0, WFlags fl = 0 );
  ~RunSimulationImpl();

  virtual void set_out_file();
  virtual void run_simulation();

};

#endif
