
#include "testing.hh"
#include "configuration.hh"



int main(int argc, const char *argv[])
{
  HANDLE_TEST_OPTIONS;

  const double positions[] = { 0.0, 0.2, 0.4, 0.3, 0.67, };
  const size_t no_positions = (sizeof positions)/sizeof(double);

  try {
    Configuration conf(0,
		       (const double*)positions, &positions[no_positions],
		       0.0, 0.0, 0.0, 0.0, 0);
    ERROR("Unsorted positions");
  } catch (Configuration::out_of_sequence&) {}


  try {

  const double positions[] = { 0.0, 0.2, 0.3, 0.4, 0.67, };
  const size_t no_positions = (sizeof positions)/sizeof(double);

  Configuration conf(0,
		     (const double*)positions, &positions[no_positions],
		     0.0, 0.0, 0.0, 0.0, 0);
  for (size_t i = 0; i < no_positions; ++i)
    CHECK(conf.position(i) == positions[i]);


  try {
    conf.set_marker(no_positions,0);
    ERROR("Cannot set types for markers beyond [0,1,...,no_pos-1]");
  } catch(std::out_of_range&) {}

  try {
    conf.set_marker(no_positions+1,0);
    ERROR("Cannot set types for markers beyond [0,1,...,no_pos-1]");
  } catch(std::out_of_range&) {}

  conf.set_marker(0,0);
  try {
    conf.first_marker(0);
    ERROR("Cannot access uninitialized marker.");
  } catch(Configuration::uninitialized_marker&) {}
  try {
    conf.plain_marker(0);
    ERROR("Cannot access uninitialized marker.");
  } catch(Configuration::uninitialized_marker&) {}

  conf.set_marker(0,(Marker*)0xdeadbeef,true);
  CHECK( conf.is_first_marker(0));
  CHECK(!conf.is_plain_marker(0));

  conf.set_marker(0,(Marker*)0xdeadbeef,false);
  CHECK(!conf.is_first_marker(0));
  CHECK( conf.is_plain_marker(0));

  
  } catch (std::exception &ex) {
    std::cout << "EXCEPTION: " << ex.what() << std::endl;
    return 2;
  }


  REPORT_RESULTS;
}
