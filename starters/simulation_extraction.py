#!/usr/bin/python

import sys, string, commands, random
from xml.sax import make_parser
from xml.sax.handler import ContentHandler

rand = random.Random()
rand.seed()


for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-ocases':
        cases_outfile = sys.argv[i+1]
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-ocontrols':
        controls_outfile = sys.argv[i+1]
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-i':
        path = sys.argv[i+1]
        break

uvs_format = 0
for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-uvs_format':
        uvs_format = 1
        break

number_disease_markers = 0
for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-number_disease_markers':
        number_disease_markers = int(sys.argv[i+1])
        break

disease_markers = []
for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-disease_positions':
        for m in range(number_disease_markers):
            disease_markers.append([sys.argv[i+1+m]])
        break


for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-disease_alleles':
        for m in range(number_disease_markers):
            disease_markers[m].append(sys.argv[i+1+m])
        break


number_cases_with_mutation = 0.0
for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-number_cases_with_mutation':
        number_cases_with_mutation = float(sys.argv[i+1])
        break
    
number_controls_with_mutation = 0.0
for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-number_controls_with_mutation':
        number_controls_with_mutation = float(sys.argv[i+1])
        break

number_controls = 0.0
for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-number_controls':
        number_controls = float(sys.argv[i+1])
        break

number_cases = 0.0
for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-number_cases':
        number_cases = float(sys.argv[i+1])
        break

for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-dtd':
        dtd = sys.argv[i+1]
        break

randomphase = 0
for i in range(1,len(sys.argv)):
    if sys.argv[i] == '-randomphase':
        randomphase = 1
        break



def comp(x,y):
    if float(y[1])<float(x[1]):
        return 1
    else:
        return -1




def get_marker_ids(marker,disease_markers):
    lst = []
    for d in disease_markers:
        dist = []
        for m in range(len(marker)):
            dist.append([m,abs(float(d[0])-float(marker[m][1]))])
        dist.sort(comp)
        lst.append([marker[dist[0][0]][0],d[1]])
    return lst


class Character_append:
    def __init__(self):
        self.s = '';
        self.st = '';

    def clear(self):
        self.s = ''

    def get(self):
        self.st = self.s
        self.s = ''
        return self.st
    
    def add(self,txt):
        self.s = self.s + string.strip(str(txt))


class Haplotype_Extraction_Handler(ContentHandler):
    def __init__ (self,_haplotype_ids,_disease_markers):
        self.pc_data = Character_append();
        self.haplotype_ids = _haplotype_ids;
        self.disease_markers = _disease_markers;
        self.controls = []
        self.cases = []
        self.curid = ""
        self.curmarker_id = ""
        self.allele_indicator = 0
        self.checklist = [];
        self.haplotype_data = [];
    def startElement(self, name, attrs):
        self.pc_data.clear()
        if name == 'haplotype':
            self.haplotype_data = []
            self.curid = str(attrs.get('id',"no_id"))
            self.checklist = []
        if name == 'loci':
            self.curmarker_id = str(attrs.get('marker_ref',"no_ref"))
        if name == 'allele':
            self.allele_indicator = 1
                
        return


    def characters (self, ch):
        if self.allele_indicator == 1:
            self.pc_data.add(str(ch))
                

    def endElement(self, name):
        if name == 'haplotype':
            ok = 0
            for c in self.checklist:
                if c==1:
                    ok=1
                    break
            if ok==1:
                self.cases.append([self.curid,self.haplotype_data])
            else:
                self.controls.append([self.curid,self.haplotype_data])
        if name == 'loci':
            for m in self.disease_markers:
                if m[0]==self.curmarker_id:
                    if self.curallele == str(m[1]):
                        self.checklist.append(1)
                    else:
                        self.checklist.append(0)
            self.haplotype_data.append([self.curmarker_id,self.curallele])
        if name == 'allele':
            self.allele_indicator = 0
            self.curallele = self.pc_data.get()
        return


    def getCases(self):
        leaf_cases = []
        for h in self.cases:
            ok = 0
            for l in range(len(self.haplotype_ids)):
                if self.haplotype_ids[l]==h[0]:
                    ok=1
                    self.haplotype_ids.pop(l)
                    break
            if ok==1:
                leaf_cases.append(h)
        return leaf_cases


    def getControls(self):
        leaf_controls = []
        for h in self.controls:
            ok = 0
            for l in range(len(self.haplotype_ids)):
                if self.haplotype_ids[l]==h[0]:
                    ok=1
                    self.haplotype_ids.pop(l)
                    break
            if ok==1:
                leaf_controls.append(h)
        return leaf_controls


class Haplotype_Handler(ContentHandler):
    def __init__ (self):
        self.haplotypes = [];
        self.markers = []
        self.marker = []
        self.position_indicator = 0
        self.pc_data = Character_append();
        
    def startElement(self, name, attrs):
        self.pc_data.clear()
        if name == 'leaf':
            self.haplotypes.append(str(attrs.get('haplotype',"no_haplotype_found")))
        if name == 'marker':
            self.marker = [str(attrs.get('id',"no_marker_id"))]
        if name == 'position':
            self.position_indicator = 1
        return

    def characters (self, ch):
        if self.position_indicator == 1:
            self.pc_data.add(str(ch))
        
    def endElement(self, name):
        if name == 'position':
            self.marker.append(self.pc_data.get())   
            self.position_indicator = 0
        if name == 'marker':
            self.markers.append(self.marker)
            
        return



    def getHaplotypes(self):
        return self.haplotypes


    def getMarkers(self):
        return self.markers



xml_file = open(path,"r")
parser = make_parser()
haplotype = Haplotype_Handler()
parser.setContentHandler(haplotype)
parser.parse(xml_file)
xml_file.close()

marker = haplotype.getMarkers()
marker.sort(comp)

haplotype_ids = haplotype.getHaplotypes()

disease_marker_ids = get_marker_ids(marker,disease_markers)

xml_file_2 = open(path,"r")
parser_2 = make_parser()
extration_handler = Haplotype_Extraction_Handler(haplotype_ids,disease_marker_ids)
parser_2.setContentHandler(extration_handler)
parser_2.parse(xml_file_2)
xml_file_2.close()

cases = extration_handler.getCases()
controls = extration_handler.getControls()


tmp = []
for i in range(len(cases)):
    r = rand.randint(0,len(cases)-1)
    tmp.append(cases[r])
    cases.pop(r)

for i in range(len(tmp)):
    cases.append(tmp[i])

tmp = []
for i in range(len(controls)):
    r = rand.randint(0,len(controls)-1)
    tmp.append(controls[r])
    controls.pop(r)

for i in range(len(tmp)):
    controls.append(tmp[i])


diploid_case = []

if (number_cases_with_mutation + number_controls_with_mutation) > len(cases):
    print "Too few cases "
    exit

if (number_cases+number_controls-number_cases_with_mutation-number_controls_with_mutation)>len(controls):
    print "Too few controls "
    exit
    
for c in range(number_cases_with_mutation):
    diploid_case.append([cases[0],controls[0]])
    print "case with mutation: " + str(cases[0][0]) 
    cases.pop(0)
    controls.pop(0)
    
for c in range(number_cases-number_cases_with_mutation):
    diploid_case.append([controls[0],controls[1]])
    controls.pop(0)
    controls.pop(0)


tmp = []
for i in range(len(diploid_case)):
    r = rand.randint(0,len(diploid_case)-1)
    tmp.append(diploid_case[r])
    diploid_case.pop(r)

for i in range(len(tmp)):
    diploid_case.append(tmp[i])




diploid_controls = []
for c in range(number_controls_with_mutation):
    diploid_controls.append([cases[0],controls[0]])
    cases.pop(0)
    controls.pop(0)
    
for c in range(number_controls-number_controls_with_mutation):
    diploid_controls.append([controls[0],controls[1]])
    controls.pop(0)
    controls.pop(0)

tmp = []
for i in range(len(diploid_controls)):
    r = rand.randint(0,len(diploid_controls)-1)
    tmp.append(diploid_controls[r])
    diploid_controls.pop(r)

for i in range(len(tmp)):
    diploid_controls.append(tmp[i])

case_file = open(cases_outfile,"w")
if uvs_format == 0:
    case_file.write('<?xml version="1.0"?>\n')
    case_file.write('<!DOCTYPE diploids SYSTEM "' + dtd + '"> \n')
    case_file.write('<diploids>\n')


id = 0
 
for i in diploid_case:
    id = id + 1
    if uvs_format == 0:
        case_file.write('  <diploid id="diploid_cases_' + i[0][0] + '">\n')
    for j in range(len(i[0][1])):
        write = 1
        for m in disease_marker_ids:
            if m[0]==i[0][1][j][0]:
                write = 0
                break
        if write==1:
            first = 0
            if randomphase==1:
                first = rand.randint(0,1)
            second = 1 - first
            if uvs_format == 0:
                case_file.write('    <loci marker="' + i[0][1][j][0] + '"><allele fixed="true">' + i[first][1][j][1])
                case_file.write('</allele><allele fixed="true">' + i[second][1][j][1] + '</allele></loci>\n')
            else:
                case_file.write(i[first][1][j][1] + ' ' + i[second][1][j][1] + ' ')
                
    if uvs_format == 0:
        case_file.write('  </diploid>\n')
    else:
        case_file.write('\n')
     
if uvs_format == 0:
    case_file.write('</diploids>\n')
case_file.close()

control_file = open(controls_outfile,"w")
if uvs_format == 0:
    control_file.write('<?xml version="1.0"?>\n')
    control_file.write('<!DOCTYPE diploids SYSTEM "' + dtd + '"> \n')
    control_file.write('<diploids>\n')

id = 0
for i in diploid_controls:
    id = id + 1
    if uvs_format == 0:
        control_file.write('  <diploid id="diploid_controls_' + i[0][0] + '">\n')
    for j in range(len(i[0][1])):
        write = 1
        for m in disease_marker_ids:
            if m[0]==i[0][1][j][0]:
                write = 0
                break
        if write==1:
            first = 0
            if randomphase==1:
                first = rand.randint(0,1)
            second = 1 - first
            if uvs_format == 0:
                control_file.write('    <loci marker="' + i[0][1][j][0] + '"><allele fixed="true">' + i[first][1][j][1])
                control_file.write('</allele><allele fixed="true">' + i[second][1][j][1] + '</allele></loci>\n')
            else:
                control_file.write(i[first][1][j][1] + ' ' + i[second][1][j][1] + ' ')
    if uvs_format == 0:
        control_file.write('  </diploid>\n')
    else:
        control_file.write('\n')
if uvs_format == 0:
    control_file.write('</diploids>\n')
control_file.close()


