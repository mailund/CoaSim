#ifndef RUN_SIMULATION_IMPL_H
#define RUN_SIMULATION_IMPL_H
#include "runsimulationform.h"

class QTable;
class Configuration;

class RunSimulationImpl : public RunSimulationForm
{
  Q_OBJECT
  QString &_output_file;
  bool    &_leaves_only;

public:
  RunSimulationImpl(QString &output_file,
		    bool    &leaves_only,
		    QWidget* parent = 0, const char* name = 0, WFlags fl = 0 );
  ~RunSimulationImpl();

  virtual void set_out_file();
  virtual void run_simulation();

};

#endif
