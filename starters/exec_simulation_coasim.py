#!/usr/bin/python

import sys, string, commands
from xml.sax import make_parser
from xml.sax.handler import ContentHandler

def comp(x,y):
    x_position = 0.0
    y_position = 0.0
    for p in x:
        if str(p)[:4]=="pos=":
            x_position = float(p[4:])
            break
    for p in y:
        if str(p)[:4]=="pos=":
            y_position = float(p[4:])
            break
    if y_position<x_position:
        return 1
    else:
        return -1


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

class Value_Handler(ContentHandler):
    def __init__ (self):
        self.pc_data = Character_append();
        self.execution_dir = "";  
        self.output_dir = "";  
        self.dtd = "";  
        self.xsl = "";  
        self.log = "";  
        self.output_file = "";
        self.genecon_length = "";
        self.genecon_rate = "";
        self.recomb_rate = "";
        self.growth = "";
        self.mutation_rate = "";
        self.number_leafs = "";
        self.set_rand_seed = "";
        self.mode = "";
        
        self.execution_dir_indicator = 0;  
        self.output_dir_indicator = 0;  
        self.dtd_indicator = 0;  
        self.xsl_indicator = 0;  
        self.log_indicator = 0;  
        self.output_file_indicator = 0;
        self.genecon_length_indicator = 0;
        self.genecon_rate_indicator = 0;
        self.recomb_rate_indicator = 0;
        self.growth_indicator = 0;
        self.mutation_rate_indicator = 0;
        self.number_leafs_indicator = 0;
        self.set_rand_seed_indicator = 0;
        self.marker_type_indicator = 0;
        self.low_freq_indicator = 0;
        self.high_freq_indicator = 0;
        self.position_indicator = 0;
        self.alphabet_indicator = 0;
        self.markers_indicator = 0;
        self.marker_indicator = 0;
        self.value_indicator = 0;
        self.output_indicator = 0;

        self.cur_marker_number = 0;
        self.cur_is_poly = 0;
        self.markers = []


    def get_poly_list(self):
        poly_list = ""
        self.markers.sort(comp)
        for e in self.markers:
            for p in e:
                if str(p)[:8]=="is_poly=":
                    poly_list = poly_list + " " + p[8:]
        poly_list = '"' + poly_list[1:] + '"'
        return poly_list
        

    def get_positions(self):
        positions = ""
        self.markers.sort(comp)
        for e in self.markers:
            for p in e:
                if str(p)[:4]=="pos=":
                    positions = positions + " " + p[4:]
        positions = '"' + positions[1:] + '"'
        return positions


        
    def get_marker_type(self):
        marker_type = ""
        self.markers.sort(comp)
        for e in self.markers:
            for p in e:
                if str(p)[:12]=="marker_type=":
                    marker_type = marker_type + " " + p[12:]
        marker_type = '"' + marker_type[1:] + '"'
        return marker_type


    def get_low_freq(self):
        low_freq = ""
        self.markers.sort(comp)
        for e in self.markers:
            for p in e:
                if str(p)[:9]=="low_freq=":
                    low_freq = low_freq + " " + p[9:]
        low_freq = '"' + low_freq[1:] + '"'
        return low_freq


    def get_high_freq(self):
        high_freq = ""
        self.markers.sort(comp)
        for e in self.markers:
            for p in e:
                if str(p)[:10]=="high_freq=":
                    high_freq = high_freq + " " + p[10:]
        high_freq = '"' + high_freq[1:] + '"'
        return high_freq


    def get_alphabets(self):
        alphabet = ""
        self.markers.sort(comp)
        for e in self.markers:
            alphabet = alphabet + '{'
            for p in e:
                if str(p)[:6]=="value=":
                    alphabet = alphabet + " " + string.strip(p[6:])
            alphabet = alphabet + ' } '
        alphabet = '"' + alphabet[:-1] + '"'
        return alphabet

      
    def startElement(self, name, attrs):
        self.pc_data.clear()
        if name == 'dtd':
            self.dtd_indicator = 1;
        if name == 'xsl':
            self.xsl_indicator = 1;
        if name == 'log':
            self.log_indicator = 1;
        if name == 'execution_dir':
            self.execution_dir_indicator = 1;
        if name == 'output_dir':
            self.output_dir_indicator = 1;
        if name == 'output_file':
            self.output_file_indicator = 1;
        if name == 'geneconversion_length':
            self.genecon_length_indicator = 1;
        if name == 'geneconversion_rate':
            self.genecon_rate_indicator = 1;
        if name == 'recombination_rate':
            self.recomb_rate_indicator = 1;
        if name == 'growth':
            self.growth_indicator = 1;
        if name == 'mutation_rate':
            self.mutation_rate_indicator = 1;
        if name == 'number_of_leafs':
            self.number_leafs_indicator = 1;
        if name == 'output':
            self.output_indicator = 1;
            self.mode = str(attrs.get('mode',"full_mode"))
        if name == 'markers':
            self.markers_indicator = 1;
        if name == 'marker':
            marker_number = attrs.get('number',"-1")
            self.cur_marker_number = int(marker_number);
            self.marker_indicator = 1;
            self.markers.append([self.cur_marker_number])
        if name == 'position':
            self.position_indicator = 1;
        if name == 'marker_type':
            self.marker_type_indicator = 1;
        if name=='low_freq':
            self.low_freq_indicator = 1;
        if name=='high_freq':
            self.high_freq_indicator = 1;
        if name=='alphabet':
            self.alphabet_indicator = 1;
        if name == 'value':
            self.value_indicator = 1;
        if name == 'use_random_seed':
            self.set_rand_seed_indicator = 1;
        return

    def characters (self, ch):
        if self.dtd_indicator == 1:
            self.pc_data.add(str(ch))
        if self.xsl_indicator == 1:
            self.pc_data.add(str(ch))
        if self.log_indicator == 1:
            self.pc_data.add(str(ch))
        if self.execution_dir_indicator == 1:
            self.pc_data.add(str(ch))
        if self.output_dir_indicator == 1:
            self.pc_data.add(str(ch))
        if self.output_file_indicator == 1:
            self.pc_data.add(str(ch))
        if self.genecon_length_indicator == 1:
            self.pc_data.add(str(ch))
        if self.genecon_rate_indicator == 1:
            self.pc_data.add(str(ch))
        if self.recomb_rate_indicator == 1:
            self.pc_data.add(str(ch))
        if self.growth_indicator == 1:
            self.pc_data.add(str(ch))
        if self.mutation_rate_indicator == 1:
            self.pc_data.add(str(ch))
        if self.number_leafs_indicator == 1:
            self.pc_data.add(str(ch))
        if self.markers_indicator == 1:
            self.marker_indicator = 0
        if self.marker_indicator == 1:
            self.marker_indicator = 0
        if self.position_indicator == 1:
            self.pc_data.add(str(ch))
        if self.marker_type_indicator == 1:
            self.pc_data.add(str(ch))
        if self.low_freq_indicator == 1:
            self.pc_data.add(str(ch))
        if self.high_freq_indicator == 1:
            self.pc_data.add(str(ch))
        if self.alphabet_indicator == 1:
            self.alphabet_indicator = 0
        if self.value_indicator == 1:
            self.pc_data.add(str(ch))
        if self.set_rand_seed_indicator == 1:
            self.pc_data.add(str(ch))


    def endElement(self, name):
        if name == 'dtd':
            self.dtd = self.pc_data.get()
            self.dtd_indicator = 0;
        if name == 'xsl':
            self.xsl = self.pc_data.get()   
            self.xsl_indicator = 0;
        if name == 'log':
            self.log = self.pc_data.get()   
            self.log_indicator = 0;
        if name == 'execution_dir':
            self.execution_dir_indicator = 0;
            self.execution_dir = self.pc_data.get()   
        if name == 'output_dir':
            self.output_dir = self.pc_data.get()   
            self.output_dir_indicator = 0;
        if name == 'output_file':
            self.output_file_indicator = 0;
            self.output_file = self.pc_data.get()
        if name == 'geneconversion_length':
            self.genecon_length_indicator = 0;
            self.genecon_length = self.pc_data.get()    
        if name == 'geneconversion_rate':
            self.genecon_rate_indicator = 0;
            self.genecon_rate = self.pc_data.get() 
        if name == 'recombination_rate':
            self.recomb_rate_indicator = 0;
            self.recomb_rate = self.pc_data.get()
        if name == 'growth':
            self.growth_indicator = 0;
            self.growth = self.pc_data.get()
        if name == 'mutation_rate':
            self.mutation_rate = self.pc_data.get()   
            self.mutation_rate_indicator = 0;
        if name == 'number_of_leafs':
            self.number_leafs_indicator = 0;
            self.number_leafs = self.pc_data.get() 
        if name == 'output':
            self.output_indicator = 0;
        if name == 'markers':
            self.markers_indicator = 0;
        if name == 'marker':
            self.marker_indicator = 0;
        if name == 'position':
            for i in range(len(self.markers)):
                if self.markers[i][0]==self.cur_marker_number:
                    self.markers[i].append("pos="+self.pc_data.get())
                    break
            self.position_indicator = 0;

        if name == 'marker_type':
            for i in range(len(self.markers)):
                if self.markers[i][0]==self.cur_marker_number:
                    self.markers[i].append("marker_type="+self.pc_data.get())
                    break
            self.marker_type_indicator = 0;

        if name == 'low_freq':
            for i in range(len(self.markers)):
                if self.markers[i][0]==self.cur_marker_number:
                    self.markers[i].append("low_freq="+self.pc_data.get())
                    break
            self.low_freq_indicator = 0;

        if name == 'high_freq':
            for i in range(len(self.markers)):
                if self.markers[i][0]==self.cur_marker_number:
                    self.markers[i].append("high_freq="+self.pc_data.get())
                    break
            self.high_freq_indicator = 0;

        if name=='alphabet':
            self.alphabet_indicator = 0;


        if name == 'value':
            self.value_indicator = 0;
            for i in range(len(self.markers)):
                if self.markers[i][0]==self.cur_marker_number:
                    self.markers[i].append("value=" + self.pc_data.get())
                    break

        if name == 'use_random_seed':
            self.set_rand_seed = self.pc_data.get() 
            self.set_rand_seed_indicator = 0;




print sys.argv[1]
path = sys.argv[1]
xml_file = open(path,"r")



parser = make_parser()
curH = Value_Handler()
parser.setContentHandler(curH)
parser.parse(xml_file)
xml_file.close()

commandline = curH.execution_dir + 'coasim -o ' + curH.output_file + ' -gl ' + curH.genecon_length + ' -gr '
commandline = commandline + curH.genecon_rate + ' -rr ' +  curH.recomb_rate + ' -exp ' + curH.growth + ' -mr '
commandline = commandline + curH.mutation_rate + ' -ln ' +  curH.number_leafs + ' -v ' +  curH.get_alphabets() + ' -rand ' +  curH.set_rand_seed + ' -marker_type '
commandline = commandline + curH.get_marker_type() + ' -pos ' + curH.get_positions() + ' -low_freq ' + curH.get_low_freq() + ' -high_freq ' + curH.get_high_freq() + ' -dtd '
commandline = commandline + curH.dtd + ' -log ' + curH.output_dir + curH.log + ' -mode ' + curH.mode
print "now executing " + commandline
commands.getoutput(commandline)


#commandline = "xsltproc -o " + curH.output_dir + "tmp.dot " + curH.xsl + " " + curH.output_file 
#print "now executing " + commandline
#print commands.getoutput(commandline)
#commandline = "dot -Tps -o " + curH.output_dir + "tmp.ps " + curH.output_dir + "tmp.dot " 
#print "now executing " + commandline
#print commands.getoutput(commandline)
#commandline = "ggv " + curH.output_dir + "tmp.ps"
#print "now executing " + commandline
#print commands.getoutput(commandline)
