
add_library(Core 
		   all_markers.hh
		   builder.hh builder.cc
		   builder_events.hh builder_events.cc
		   compile_options.hh
		   configuration.hh configuration.cc
		   descender.hh descender.cc
		   dist_funcs.hh dist_funcs.cc
		   interval.hh interval.cc
		   marker.hh marker.cc
		   epochs.hh epochs.cc
		   micro_satellite_marker.hh micro_satellite_marker.cc
		   monitor.hh 
		   node.hh node.cc
		   retired_interval.hh retired_interval.cc
		   simulator.hh simulator.cc
		   snp_marker.hh snp_marker.cc
		   trait_marker.hh trait_marker.cc
		   )


add_executable( interval_test
			  interval.hh interval.cc
			  interval_test.cc)

add_executable(configuration_test
			  interval.hh interval.cc
			  retired_interval.hh retired_interval.cc
			  node.hh node.cc
			  marker.hh marker.cc
			  dist_funcs.hh dist_funcs.cc
			  snp_marker.hh snp_marker.cc
			  configuration.hh configuration.cc
			  configuration_test.cc
			  )

add_executable(node_test
			  node.hh node.cc
			  interval.hh interval.cc
			  retired_interval.hh retired_interval.cc
			  configuration.hh configuration.cc
			  marker.hh marker.cc
			  snp_marker.hh snp_marker.cc
			  dist_funcs.hh dist_funcs.cc
			  node_test.cc
			  )

add_executable(all_markers_test
			  testing.hh
			  interval.hh interval.cc
			  node.hh node.cc
			  retired_interval.hh retired_interval.cc
			  marker.hh marker.cc
			  all_markers.hh 
			  snp_marker.hh snp_marker.cc
			  micro_satellite_marker.hh micro_satellite_marker.cc
			  trait_marker.hh trait_marker.cc
			  dist_funcs.hh dist_funcs.cc
			  all_markers_test.cc
			  )

add_executable(mutation_test
			  interval.hh interval.cc
			  node.hh node.cc
			  retired_interval.hh retired_interval.cc
			  configuration.hh configuration.cc
			  marker.hh marker.cc
			  all_markers.hh
			  snp_marker.hh snp_marker.cc
			  micro_satellite_marker.hh micro_satellite_marker.cc
			  trait_marker.hh trait_marker.cc
			  dist_funcs.hh test_dist_funcs.hh test_dist_funcs.cc
			  mutation_test.cc
			  )

add_executable(builder_test
			  interval.hh interval.cc
			  retired_interval.hh retired_interval.cc
			  node.hh node.cc
			  configuration.hh configuration.cc
			  dist_funcs.hh dist_funcs.cc
			  marker.hh marker.cc
			  snp_marker.hh snp_marker.cc
			  builder.hh builder.cc
			  builder_events.hh builder_events.cc
			  epochs.hh epochs.cc
			  builder_test.cc
			  )

add_executable(descender_test
			  interval.hh interval.cc
			  retired_interval.hh retired_interval.cc
			  node.hh node.cc
			  configuration.hh configuration.cc
			  marker.hh marker.cc
			  snp_marker.hh snp_marker.cc
			  trait_marker.hh trait_marker.cc
			  micro_satellite_marker.hh micro_satellite_marker.cc
			  dist_funcs.hh dist_funcs.cc
			  descender.cc
			  descender_test.cc
			  )

add_executable(simulator_test
			  interval.hh interval.cc
			  retired_interval.hh retired_interval.cc
			  node.hh node.cc
			  configuration.hh configuration.cc
			  builder.hh builder.cc
			  builder_events.hh builder_events.cc
			  epochs.hh epochs.cc
			  descender.hh descender.cc
			  marker.hh marker.cc
			  snp_marker.hh snp_marker.cc
			  trait_marker.hh trait_marker.cc
			  micro_satellite_marker.hh micro_satellite_marker.cc
			  dist_funcs.hh dist_funcs.cc
			  simulator.hh simulator.cc
			  simulator_test.cc
			  )

enable_testing()
add_test(IntervalTest interval_test)
add_test(ConfigurationTest configuration_test)
add_test(NodeTest node_test)
add_test(AllMarkersTest all_markers_test)
add_test(MutationTest mutation_test)
add_test(BuilderTest builder_test)
add_test(DescenderTest descender_test)
add_test(SimulatorTest simulator_test)
