
#include "builder.hh"
#include "configuration.hh"
#include "node.hh"

#include "testing.hh"

/*
 * WARNING: THIS PROGRAM IS NOT REALLY TESTING BUILDER -- WE NEED TO
 * DO THAT STATISTICALLY SOMEHOW -- THE PROGRAM SIMPLY TRIES TO RUN
 * THE BUILDER (TO SEE THAT IT DOESN'T CRASH).  MOST OF THE REALY
 * FUNCTIONALITY IS IMPLEMENTED AND TESTED IN THE NODE MODULE ANYWAY.
 */


int main(int argc, const char *argv[])
{
  HANDLE_TEST_OPTIONS;

  try {

    const double positions[] = { 0.0, 0.2, 0.3, 0.4, 0.67, };
    const size_t no_positions = (sizeof positions)/sizeof(double);

    Configuration conf((const double*)positions, &positions[no_positions],
		       0.0, 0.0, 0.0, 0.0, 0.0);

    Builder b(conf);
    b.build(10);


  } catch (std::exception &ex) {
    std::cout << "EXCEPTION: " << ex.what() << std::endl;
  }

  REPORT_RESULTS;
}
