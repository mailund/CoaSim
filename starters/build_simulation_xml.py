#!/usr/bin/python

import sys, string


def comp(x,y):
    if float(y[0])<float(x[0]):
        return 1
    else:
        return -1



path = sys.argv[1]
name = sys.argv[2]
leafs = int(sys.argv[3])
rho = sys.argv[4]
growth = sys.argv[5]
mutation = sys.argv[6]
gencon_length = sys.argv[7]
gencon_param = sys.argv[8]

markers = int(sys.argv[9])

last_cnt = 9
marker_data = []
loci = []
## entering positions
for i in range(markers):
    loci = [sys.argv[last_cnt+i+1]]
    marker_data.append(loci)
## entering marker type
for i in range(markers):
    marker_data[i].append(sys.argv[last_cnt+i+markers+1])
## entering lower acceptes freq range
for i in range(markers):
    marker_data[i].append(sys.argv[last_cnt+i+2*markers+1])
## entering higher acceptes freq range
for i in range(markers):
    marker_data[i].append(sys.argv[last_cnt+i+3*markers+1])
## entering alphabets
alphabet_sizes = []
for i in range(markers):
    alphabet_sizes.append(int(sys.argv[last_cnt+i+4*markers+1]))

cnt = last_cnt+5*markers+1
for i in range(markers):
    alphabet = []
    for j in range(alphabet_sizes[i]):
        alphabet.append(sys.argv[cnt])
        cnt = cnt + 1
    marker_data[i].append(alphabet)
    

marker_data.sort(comp)


xml_file = open(path,"w")
    
xml_file.write('<coasim_parameters>\n')
xml_file.write('  <dtd>$MAPPING_PROGRAMS/xml/dtd/coasim.dtd</dtd>\n')
xml_file.write('  <xsl>$MAPPING_PROGRAMS/xml/xslt/coasim_to_dot.xsl</xsl>\n')
xml_file.write('  <log>coasim.log</log>\n')
xml_file.write('  <execution_dir>$MAPPING_PROGRAMS/src/coasim/src/</execution_dir>\n')
xml_file.write('  <output_dir>$MAPPING_SIMULATIONS/xml/</output_dir>\n')
xml_file.write('  <output_file>' + name + '</output_file>\n')
xml_file.write('  <geneconversion_length>' + gencon_length + '</geneconversion_length>\n')
xml_file.write('  <geneconversion_rate>' + gencon_param + '</geneconversion_rate>\n')
xml_file.write('  <recombination_rate>' + rho + '</recombination_rate>\n')
xml_file.write('  <growth>' + growth + '</growth>\n')
xml_file.write('  <mutation_rate>' + mutation + '</mutation_rate>\n')
xml_file.write('  <number_of_leafs>'+str(leafs)+'</number_of_leafs>\n')
xml_file.write('  <output mode="leaf_nodes_only"></output>\n')
xml_file.write('  <markers>\n')
for i in range(markers):
    xml_file.write('    <marker number="'+str(i)+'">\n')
    xml_file.write('      <position>' + str(marker_data[i][0]) + '</position>\n')
    xml_file.write('      <marker_type>' + str(marker_data[i][1]) + '</marker_type>\n')
    xml_file.write('      <low_freq>' + str(marker_data[i][2]) + '</low_freq>\n')
    xml_file.write('      <high_freq>' + str(marker_data[i][3]) + '</high_freq>\n')
    
    xml_file.write('      <alphabet>\n')
    for j in marker_data[i][4]:
        xml_file.write('        <value>' + j + '</value>\n')
    xml_file.write('      </alphabet>\n')
    xml_file.write('    </marker>\n')
xml_file.write('  </markers>\n')    
xml_file.write('  <use_random_seed>1</use_random_seed>\n')
xml_file.write('</coasim_parameters>\n')

xml_file.close()
