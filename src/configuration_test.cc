
#include "testing.hh"
#include "configuration.hh"



int main(int argc, const char *argv[])
{
  HANDLE_TEST_OPTIONS;

  const double positions[] = { 0.0, 0.2, 0.4, 0.3, 0.67, };
  const size_t no_positions = (sizeof positions)/sizeof(double);

  try {
    Configuration conf((const double*)positions, &positions[no_positions]);
    ERROR("Unsorted positions");
  } catch (Configuration::out_of_sequence&) {}


  try {

  const double positions[] = { 0.0, 0.2, 0.3, 0.4, 0.67, };
  const size_t no_positions = (sizeof positions)/sizeof(double);

  Configuration conf((const double*)positions, &positions[no_positions]);
  for (size_t i = 0; i < no_positions; ++i)
    CHECK(conf.position(i) == positions[i]);

  // updating the positions shouldn't have changed the number of positions
  CHECK(conf.no_markers() == no_positions);



  for (size_t i = 0; i < no_positions; ++i)
    CHECK(conf.value_set(i).size() == 0);


  try {
    conf.set_marker_type(no_positions,Configuration::MT_SNP);
    ERROR("Cannot set types for markers beyond [0,1,...,no_pos-1]");
  } catch(std::out_of_range&) {}

  try {
    conf.set_marker_type(no_positions+1,Configuration::MT_SNP);
    ERROR("Cannot set types for markers beyond [0,1,...,no_pos-1]");
  } catch(std::out_of_range&) {}

  try {
    conf.value_set(0).add_value(0);
    ERROR("Cannot add value to uninitialized marker.");
  } catch(Configuration::ValueSet::uninitialized_marker_type&) {}


  conf.set_marker_type(0,Configuration::MT_SNP);

  try {
    conf.value_set(0).add_value(-1);
    ERROR("We cannot set a SNP value -- and not to -1");
  } catch(Configuration::ValueSet::illegal_value&) {}

  try {
    conf.value_set(0).add_value(0);
    ERROR("We cannot set a SNP value");
  } catch(Configuration::ValueSet::illegal_value&) {}

  try {
    conf.value_set(0).add_value(1);
    ERROR("We cannot set a SNP value");
  } catch(Configuration::ValueSet::illegal_value&) {}

  try {
    conf.value_set(0).add_value(2);
    ERROR("We cannot set a SNP value");
  } catch(Configuration::ValueSet::illegal_value&) {}

  CHECK(conf.value_set(0).size() == 2);
  CHECK(conf.value_set(0).value(0) == 0);
  CHECK(conf.value_set(0).value(1) == 1);

  try {
    conf.value_set(0).value(2);
    ERROR("Cannot access index 2");
  } catch(std::out_of_range&) {}




  conf.set_marker_type(1,Configuration::MT_TRAIT);
  try {
    conf.value_set(1).add_value(-1);
    ERROR("We cannot set a Trait value -- and not to -1");
  } catch(Configuration::ValueSet::illegal_value&) {}

  try {
    conf.value_set(1).add_value(0);
    ERROR("We cannot set a Trait value");
  } catch(Configuration::ValueSet::illegal_value&) {}

  try {
    conf.value_set(1).add_value(1);
    ERROR("We cannot set a Trait value");
  } catch(Configuration::ValueSet::illegal_value&) {}

  try {
    conf.value_set(1).add_value(2);
    ERROR("We cannot set a Trait value");
  } catch(Configuration::ValueSet::illegal_value&) {}

  CHECK(conf.value_set(1).size() == 2);
  CHECK(conf.value_set(1).value(0) == 0);
  CHECK(conf.value_set(1).value(1) == 1);

  try {
    conf.value_set(1).value(2);
    ERROR("Cannot access index 2");
  } catch(std::out_of_range&) {}



  conf.set_marker_type(2,Configuration::MT_MICROSATELLITE);
  CHECK(conf.value_set(2).size() == 0);

  try {
    conf.value_set(2).add_value(-1);
    ERROR("We cannot set a micro satellite value to a negative value.");
  } catch(Configuration::ValueSet::illegal_value&) {}
  CHECK(conf.value_set(2).size() == 0);

  conf.value_set(2).add_value(0);
  conf.value_set(2).add_value(2);
  conf.value_set(2).add_value(3);
  CHECK(conf.value_set(2).size() == 3);
  CHECK(conf.value_set(2).value(0) == 0);
  CHECK(conf.value_set(2).value(1) == 2);
  CHECK(conf.value_set(2).value(2) == 3);

  try {
    conf.value_set(2).value(3);
    ERROR("Accessing out of range.");
  } catch(std::out_of_range&) {}
  
  } catch (std::exception &ex) {
    std::cout << "EXCEPTION: " << ex.what() << std::endl;
  }


  REPORT_RESULTS;
}
