#!/usr/bin/python

import sys, string, commands, time, random

sim_indicator = ""
number_disease_markers = 1
number_cases_with_mutation = 7
number_controls_with_mutation = 2
number_controls = 500
number_cases = 500
disease_positions = "0.25"
disease_alleles = "1"
low_freqs = ""
high_freqs = ""
write_low_freqs = ""
write_high_freqs = ""

growth = 0.0;
rho = 0.0
mutation = 0.0
gencon_length = 0.0
gencon_param = 0.0
numb_tree_leafs = 0
steps = 0
leafnodes = 0
number_of_markers = 0
alfabet_size = 0
pop_size = 10000;
min_disease_location = 0.01
max_disease_location =0.99
initial_disease_location = 0.3;

cases_dir = "$MAPPING_SIMULATIONS/xml/cases/"
controls_dir = "$MAPPING_SIMULATIONS/xml/controls/"

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-cases_dir':
        cases_dir = sys.argv[i+1]
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-controls_dir':
        controls_dir = sys.argv[i+1]
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-id':
        sim_indicator = sys.argv[i+1]
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-number_disease_markers':
        number = sys.argv[i+1]
        break
number_disease_markers = int(number)
disease_positions = ''
for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-disease_positions':
        for k in range(number_disease_markers):
            disease_positions = disease_positions + sys.argv[i+1+k] + ' '
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-disease_alleles':
        disease_alleles = sys.argv[i+1]
        break
low_freqs = ''
for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-low_freqs':
        for k in range(number_disease_markers):        
            low_freqs = low_freqs + sys.argv[i+1+k] + ' '
        break
high_freqs = ''
for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-high_freqs':
        for k in range(number_disease_markers):
            high_freqs = high_freqs + sys.argv[i+1] + ' '
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-number_cases_with_mutation':
        number_cases_with_mutation = sys.argv[i+1]
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-number_controls_with_mutation':
        number_controls_with_mutation = sys.argv[i+1]
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-number_controls':
        number_controls = sys.argv[i+1]
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-number_cases':
        number_cases = sys.argv[i+1]
        break

randomphase = ""
for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-randomphase':
        randomphase = "-randomphase"
        break

randomtree = ""
for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-random-tree':
        randomtree = "-random_tree"
        break
    
for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-rho':
        rho = float(sys.argv[i+1])
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-growth':
        growth = float(sys.argv[i+1])
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-mutation-rate':
        mutation= float(sys.argv[i+1])
        break


for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-gencon-length':
        gencon_length = float(sys.argv[i+1])
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-gencon-param':
        gencon_param = float(sys.argv[i+1])
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-tree-size':
        numb_tree_leafs = int(sys.argv[i+1])
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-steps':
        steps = int(sys.argv[i+1])
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-leaf-nodes':
        leafnodes = int(sys.argv[i+1])
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-number-of-markers':
        number_of_markers = int(sys.argv[i+1])
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-alfabet-size':
        alfabet_size = int(sys.argv[i+1])
        break    

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-pop-size':
        pop_size = int(sys.argv[i+1])
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-min-disease-location':
        min_disease_location = float(sys.argv[i+1])
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-max-disease-location':
        max_disease_location = float(sys.argv[i+1])
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-initial-disease-location':
        initial_disease_location = float(sys.argv[i+1])
        break

    
markers = number_of_markers + number_disease_markers

#loci marker_type low_freq high_freq (this is 0 and 1 for microsatelitelines)
#marker_pos = "0.01 0.13 0.25 0.38 0.5 0.63 0.75 0.87 0.99 " + disease_positions
marker_pos = "0.01 "
locus = 0.01
delta = 0.98/(number_of_markers-1)
for i in range(number_of_markers-1):
    locus = locus + delta
    marker_pos = marker_pos + str(locus) + " "
marker_pos = marker_pos + disease_positions

marker_type = "0"
for i in range(number_of_markers-1):
    marker_type = marker_type + " 0"
    
for i in range(number_disease_markers):
    marker_type = marker_type + " 2"

low_freq = "0.0 "
for i in range(number_of_markers-1):
    low_freq = low_freq + "0.0 "

low_freq = low_freq + low_freqs

high_freq = "1.0 "
for i in range(number_of_markers-1):
    high_freq = high_freq + "1.0 "

high_freq = high_freq + high_freqs

alphabets_sizes = str(alfabet_size)
for i in range(number_of_markers-1):
    alphabets_sizes = alphabets_sizes + " " + str(alfabet_size)
    
for i in range(number_disease_markers):
    alphabets_sizes  = alphabets_sizes + " 2"

tmp = "0"
for i in range(alfabet_size-1):
    tmp = tmp + " " + str(i+1)

    
alphabets=tmp
for i in range(number_of_markers-1):
    alphabets=alphabets + " " + tmp
    
for i in range(number_disease_markers):
    alphabets=alphabets + " " + "0 1"


command = "$MAPPING_PROGRAMS/src/coasim/python/build_simulation_xml.py $MAPPING_SIMULATIONS/xml/coasim_input/simulation_input.xml $MAPPING_SIMULATIONS/xml/coasim_output/simulation_output.xml " + str(leafnodes) + " " + str(rho) + " " + str(growth) + " " + str(mutation) + " " + str(gencon_length) + " " + str(gencon_param) + " " + str(markers) + " " + marker_pos +  " " + marker_type + " " + low_freq + " " + high_freq + " " + alphabets_sizes + " " + alphabets + " "
print command
print commands.getoutput(command)

command = "$MAPPING_PROGRAMS/src/coasim/python/exec_simulation_coasim.py $MAPPING_SIMULATIONS/xml/coasim_input/simulation_input.xml"
print command
print commands.getoutput(command)

command = "$MAPPING_PROGRAMS/src/coasim/python/build_markers.py -i $MAPPING_SIMULATIONS/xml/coasim_output/simulation_output.xml -disease_pos " + str(number_disease_markers) + " " + str(disease_positions) + " -odist $MAPPING_SIMULATIONS/xml/distances/dist_" + sim_indicator + " -dtd $MAPPING_PROGRAMS/xml/dtd/bioinformatics.dtd "
commands.getoutput(command)

command = "$MAPPING_PROGRAMS/src/coasim/python/simulation_extraction.py -i $MAPPING_SIMULATIONS/xml/coasim_output/simulation_output.xml -ocases " + str(cases_dir) + sim_indicator + "_cases -ocontrols " + str(controls_dir) + sim_indicator + "_controls -number_disease_markers " + str(number_disease_markers)  + " -disease_positions " + str(disease_positions) + " -disease_alleles " + str(disease_alleles) + " -number_cases_with_mutation " + str(number_cases_with_mutation) + " -number_controls_with_mutation " + str(number_controls_with_mutation) + " -number_controls " + str(number_controls) + " -number_cases " + str(number_cases)  + " -dtd $MAPPING_PROGRAMS/xml/dtd/bioinformatics.dtd " + randomphase + " -uvs_format "
print command
print commands.getoutput(command)
bla
command = "$MAPPING_PROGRAMS/src/parsing/to_value_set_combi.py -o /tmp/value_set" + sim_indicator + ".xml -first $MAPPING_SIMULATIONS/xml/cases/" + sim_indicator + "_cases -second $MAPPING_SIMULATIONS/xml/controls/" + sim_indicator + "_controls "
print command
print commands.getoutput(command)
command = "$MAPPING_PROGRAMS/src/parsing/to_marker.py -o /tmp/marker" + sim_indicator + ".xml -v /tmp/value_set" + sim_indicator + ".xml -d $MAPPING_SIMULATIONS/xml/distances/dist_" + sim_indicator
print command
print commands.getoutput(command)
command = "$MAPPING_PROGRAMS/src/parsing/merge_xml_files.py -o /tmp/pretree" + sim_indicator + ".xml /tmp/value_set" + sim_indicator + ".xml /tmp/marker" + sim_indicator + ".xml $MAPPING_SIMULATIONS/xml/cases/" + sim_indicator + "_cases" 
print command
print commands.getoutput(command)
command = "$MAPPING_PROGRAMS/src/generecon/src/to_tree -o /tmp/tree" + sim_indicator + ".xml -f /tmp/pretree" + sim_indicator + ".xml -d $MAPPING_PROGRAMS/xml/dtd/bioinformatics.dtd -numb_leafs " + str(numb_tree_leafs) + " " + randomtree
print command
print commands.getoutput(command)

command = "$MAPPING_PROGRAMS/src/parsing/point_composer.py -o $MAPPING_SIMULATIONS/xml/start_point/point_" + sim_indicator + " -m /tmp/marker" + sim_indicator + ".xml -v /tmp/value_set" + sim_indicator + ".xml -c $MAPPING_SIMULATIONS/xml/controls/" + sim_indicator + "_controls -t /tmp/tree" + sim_indicator + ".xml -dtd $MAPPING_PROGRAMS/xml/dtd/bioinformatics.dtd"
print command
print commands.getoutput(command)
command = "$MAPPING_PROGRAMS/src/coasim/python/build_generecon_execution_script.py  $MAPPING_SIMULATIONS/xml/execution_starters/exec_" + sim_indicator + " $MAPPING_SIMULATIONS/xml/start_point/point_" + sim_indicator + " " + str(steps) + " " + sim_indicator + " " + str(rho/pop_size) + " " + str(mutation/pop_size) + " " + str(min_disease_location) + " " + str(max_disease_location) + " " + str(initial_disease_location)
print command
print commands.getoutput(command)
command = 'nohup $MAPPING_PROGRAMS/src/generecon/python/exec_generecon.py $MAPPING_SIMULATIONS/xml/execution_starters/exec_' + sim_indicator + ' '
print command
print commands.getoutput(command)
