#!/usr/bin/python

import sys, string, commands, random
from xml.sax import make_parser
from xml.sax.handler import ContentHandler
from coasim_leaf_xml_to_python import *

def comp(x,y):
    if float(y[1])<float(x[1]):
        return 1
    else:
        return -1


for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-i':
        path = sys.argv[i+1]
        break

removemarkers = []
number_of_markers = 0
for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-marker':
        number_of_markers = int(sys.argv[i+1])
        for j in range(number_of_markers):
            removemarkers.append(int(sys.argv[i+1+j+1]))
        break


removemarkers_pos = []
for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-disease_pos':
        number_of_markers = int(sys.argv[i+1])
        for j in range(number_of_markers):
            removemarkers_pos.append(float(sys.argv[i+1+j+1]))
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-odist':
        dist_outfile = sys.argv[i+1]
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-dtd':
        dtd = sys.argv[i+1]
        break


xml_file = open(path,"r")
parser = make_parser()
haplotype = Haplotype_Handler()
parser.setContentHandler(haplotype)
parser.parse(xml_file)
xml_file.close()

marker = haplotype.getMarkers()
marker.sort(comp)

if len(removemarkers)==0:
    for i in range(number_of_markers):
        disease_marker_number = -1
        marker_pos = float(removemarkers_pos[i])
        dist = abs(float(marker[0][1])-marker_pos)
        for i in range(len(marker)):
            if (abs(float(marker[i][1])-marker_pos))<dist:
                disease_marker_number = i
                dist = (abs(float(marker[i][1])-marker_pos))
        removemarkers.append(int(disease_marker_number))


dist_file = open(dist_outfile,"w")
dist_file.write('<?xml version="1.0"?>\n')
dist_file.write('<!DOCTYPE markers SYSTEM "' + dtd + '"> \n')
dist_file.write('<markers>\n')

toenter = 1
for i in marker:
    toenter = 1
    for k in removemarkers:
        if marker[k][0]==i[0]:
            toenter = 0
    if toenter==1:        
        dist_file.write('  <marker id="' + i[0] + '">\n') 
        dist_file.write('    <position>' + i[1] + '</position> \n')
        dist_file.write('  </marker>\n') 
dist_file.write('</markers>\n')
dist_file.close()
