#ifndef RUN_SIMULATION_IMPL_HH_INCLUDED
#define RUN_SIMULATION_IMPL_HH_INCLUDED

#ifndef RUNSIMULATIONFORM_H_INCLUDED
# include "runsimulationform.h"
# define RUNSIMULATIONFORM_H_INCLUDED
#endif

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
