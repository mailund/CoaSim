
#include "rc-parser.hh"
#include "testing.hh"

#include <sstream>

int main(int argc, const char *argv[])
{
  HANDLE_TEST_OPTIONS;

  try {

    std::istringstream is("growth: 0.99\n"
			  "#comment\n"
			  "no_leaves: 5\n"
			  "rho: 250.0 #comment\n"
			  " foo: 10.0\n"
			  "bar : 20.0\n"
			  "positions: 0.1 0.2 0.3\n"
			  "markers: trait ms snp\n"
			  "no_values: 0 4 0\n"
			  "true_value: 1\n"
			  "true_value_2: true\n"
			  "false_value: 0\n"
			  "false_value_2: false\n");
    std::map<std::string,std::string> m;
    
    RCParser rcp(is);
    
    CHECK(rcp.get_int("no_leaves") == 5);
    
    CHECK(rcp.get_double("growth") == 0.99);
    CHECK(rcp.get_double("rho") == 250.0);
    
    CHECK(rcp.get_double("foo") == 10.0);
    CHECK(rcp.get_double("bar") == 20.0);

    std::vector<double> positions = rcp.get_double_vector("positions");
    CHECK(positions.size() == 3);
    CHECK(positions[0] == 0.1);
    CHECK(positions[1] == 0.2);
    CHECK(positions[2] == 0.3);

    std::vector<std::string> markers = rcp.get_string_vector("markers");
    CHECK(markers.size() == 3);
    CHECK(markers[0] == "trait");
    CHECK(markers[1] == "ms");
    CHECK(markers[2] == "snp");

    std::vector<int> no_values = rcp.get_int_vector("no_values");
    CHECK(no_values.size() == 3);
    CHECK(no_values[0] == 0);
    CHECK(no_values[1] == 4);
    CHECK(no_values[2] == 0);

    CHECK(rcp.get_bool("true_value") == true);
    CHECK(rcp.get_bool("true_value_2") == true);
    CHECK(rcp.get_bool("false_value") == false);
    CHECK(rcp.get_bool("false_value_2") == false);

  } catch(std::exception &ex) {
    std::cerr << "Ex: " << ex.what() << std::endl;
    return 2;
  }

  REPORT_RESULTS;
}
