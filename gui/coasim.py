#!/usr/bin/python
from qt import *
import time, math
import sys, string, commands
import os, thread

from coasim_gui import coasim_gui


def comp(x,y):
    if float(y[0])<float(x[0]):
        return 1
    else:
        return -1


class Input_data:
    def __init__(self,parent):
        self.parent = parent
        self.execution_dir = "";  
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
        self.mode = "leaf_nodes_only";
        self.markers = []

    def set(self,markers):
        self.markers = markers
        self.execution_dir = str(self.parent.exec_dir.text());  
        self.dtd = str(self.parent.dtd.text())  
        self.log = str(self.parent.log.text())    
        self.output_file = str(self.parent.o.text())  
        self.genecon_length = str(self.parent.gl.text())
        self.genecon_rate = str(self.parent.gr.text())
        self.recomb_rate = str(self.parent.rr.text());
        self.growth = str(self.parent.exp.text())
        self.mutation_rate = str(self.parent.mr.text());
        self.number_leafs = str(self.parent.ln.text())  
        self.set_rand_seed = str(self.parent.rand.isChecked())
        self.mode = str(self.parent.mode.isChecked()) 
        if int(self.parent.mode.isChecked())==0:
            self.mode = "all_nodes"
        else:
            self.mode = "leaf_nodes_only"



    def get_positions(self):
        positions = ""
        self.markers.sort(comp)
        for e in self.markers:
            positions = positions + " " + str(e[0])
        positions = '"' + positions[1:] + '"'
        return positions


        
    def get_marker_type(self):
        marker_type = ""
        self.markers.sort(comp)
        for e in self.markers:
            marker_type = marker_type + " " + str(e[3])
        marker_type = '"' + marker_type[1:] + '"'
        return marker_type


    def get_low_freq(self):
        low_freq = ""
        self.markers.sort(comp)
        for e in self.markers:
            low_freq = low_freq + " " + str(e[1])
        low_freq = '"' + low_freq[1:] + '"'
        return low_freq


    def get_high_freq(self):
        high_freq = ""
        self.markers.sort(comp)
        for e in self.markers:
            high_freq = high_freq + " " + str(e[2])
        high_freq = '"' + high_freq[1:] + '"'
        return high_freq


    def get_alphabets(self):
        alphabet = ""
        self.markers.sort(comp)
        for e in self.markers:
            alphabet = alphabet + '{'
            for i in range(int(e[4])):
                alphabet = alphabet + " " + str(i)
            alphabet = alphabet + ' } '
        alphabet = '"' + alphabet[:-1] + '"'
        return alphabet




class Tools:
    def __init__(self,parent):
        self.parent = parent
        self.filename = ""
        
    def findfile_and_set(self,field):
        field.setText(str(QFileDialog.getOpenFileName("$HOME", "*", self.parent, "FileDialog", "Choose a file")))


        
class build_command_line:
    def __init__(self,parent):
        self.parent = parent
        self.tools = Tools(parent)
        self.markers = []
        for i in range(10):
            self.markers.append([0.05+0.1*i,0.2,0.8,1,2])

        self.progress_log_file = commands.getoutput("echo " + str(self.parent.log.text())+"_progress.dat")
        updateTimer = QTimer(self.parent)
        self.parent.connect(updateTimer, SIGNAL("timeout()"), self.progress)
        updateTimer.start(1000)
        self.action = 0
        
    def progress(self):
        if self.action == 1:
            try:
                file = open(self.progress_log_file,"r")
                txt = ''
                left_top_nodes = 0
                init_top_nodes = 100
                old_left = 0
                for line in file.readlines():
                    left_top_nodes = int(string.split(line)[0])
                    init_top_nodes = int(string.split(line)[1])
                file.close()
                if old_left != left_top_nodes:
                    if left_top_nodes <= init_top_nodes:
                        self.parent.progressBar.setProgress(int(float(init_top_nodes-left_top_nodes)*100.0/float(init_top_nodes)))
                    else:
                        self.parent.progressBar.setProgress(0)
                    old_left = left_top_nodes
            except IOError:
                return
            #silently accept IOErrors
            
        
    def output_file_clicked(self):
        self.tools.findfile_and_set(self.parent.o)

    def dtd_clicked(self):
        self.tools.findfile_and_set(self.parent.dtd)

    def log_clicked(self):
        self.tools.findfile_and_set(self.parent.log)

    def exec_lines_clicked(self):
        line = "New Thread"
        thread.start_new_thread(self.exec_lines,(line,))        

    def exec_lines(self,line):
        command = ""
        if string.strip(str(self.parent.post1.text())) != "":
            command = string.strip(str(self.parent.post1.text()))
            print command
            print commands.getoutput(command)
        if string.strip(str(self.parent.post2.text())) != "":
            command = string.strip(str(self.parent.post2.text()))
            print command
            print commands.getoutput(command)
        if string.strip(str(self.parent.post3.text())) != "":
            command = string.strip(str(self.parent.post3.text()))
            print command
            print commands.getoutput(command)
        if string.strip(str(self.parent.post4.text())) != "":
            command = string.strip(str(self.parent.post4.text()))
            print command
            print commands.getoutput(command)


    def simulate_clicked(self):
        self.action = 1
        self.parent.progressBar.setProgress(0)
        self.parent.status_text.setText(str("Simulating"))
        curH = Input_data(self.parent)
        curH.set(self.markers)
        commandline = curH.execution_dir + 'coasim -o ' + curH.output_file + ' -gl ' + curH.genecon_length + ' -gr '
        commandline = commandline + curH.genecon_rate + ' -rr ' +  curH.recomb_rate + ' -exp ' + curH.growth + ' -mr '
        commandline = commandline + curH.mutation_rate  + ' -ln ' +  curH.number_leafs + ' -v ' + curH.get_alphabets() + ' -rand ' +  curH.set_rand_seed + ' -marker_type '
        commandline = commandline + curH.get_marker_type() + ' -pos ' + curH.get_positions() + ' -low_freq ' + curH.get_low_freq() + ' -high_freq ' + curH.get_high_freq() + ' -dtd '
        commandline = commandline + curH.dtd + ' -log ' + curH.log + ' -mode ' + curH.mode
        

        self.do_simulation(commandline)
#        thread.start_new_thread(self.do_simulation,(commandline,))


    def set_type(self,type):
        if type == 0:
            self.parent.msat.setChecked(1)
        if type == 1:
            self.parent.snp.setChecked(1)
        if type == 2:
            self.parent.dsnp.setChecked(1)

    def markers_changed(self):
        s = self.parent.marker_indicator.count()
        for i in range(s):
            self.parent.marker_indicator.removeItem(0)
        s = int(str(self.parent.markers.text()))
        self.markers = []
        for i in range(s):
            self.parent.marker_indicator.insertItem(str(i+1) + ". marker")
            self.markers.append([0.05+0.9/(s-1)*i,0.2,0.8,1,2])

        self.parent.marker_indicator.setCurrentItem(0)
        self.parent.position.setText(str(self.markers[0][0]))
        self.set_type(self.markers[0][3])


    def marker_indicator_activated(self):
        self.parent.position.setText(str(self.markers[int(str(self.parent.marker_indicator.currentItem()))][0]))
        self.parent.markerminfreq.setText(str(self.markers[int(str(self.parent.marker_indicator.currentItem()))][1]))
        self.parent.markermaxfreq.setText(str(self.markers[int(str(self.parent.marker_indicator.currentItem()))][2]))
        self.set_type(self.markers[int(str(self.parent.marker_indicator.currentItem()))][3])
        self.parent.alsize.setText(str(self.markers[int(str(self.parent.marker_indicator.currentItem()))][4]))

    def position_changed(self):
        index = int(str(self.parent.marker_indicator.currentItem()))
        self.markers[index][0] = str(self.parent.position.text())

    def alphabet_changed(self):
        index = int(str(self.parent.marker_indicator.currentItem()))
        self.markers[index][4] = str(self.parent.alsize.text())

    def type_changed(self):
        index = int(str(self.parent.marker_indicator.currentItem()))
        if self.parent.snp.isChecked()==1:
            self.markers[index][3] = 1
        if self.parent.dsnp.isChecked()==1:
            self.markers[index][3] = 2
        if self.parent.msat.isChecked()==1:
            self.markers[index][3] = 0

    def markerminfreq_changed(self):
        index = int(str(self.parent.marker_indicator.currentItem()))
        self.markers[index][1] = str(self.parent.markerminfreq.text())

    def markermaxfreq_changed(self):
        index = int(str(self.parent.marker_indicator.currentItem()))
        self.markers[index][2] = str(self.parent.markermaxfreq.text())


    def do_simulation(self,command):
        print command
        print commands.getoutput(command)
        self.parent.progressBar.setProgress(100)
        self.parent.status_text.setText(str("Done"))
        self.action = 0

class coasim(coasim_gui):
    def __init__(self, parent=None, name=None, fl=0):
        coasim_gui.__init__(self,parent,name,fl)
        self.build_commandline = build_command_line(self)
        self.initial_output_file = "$HOME/coasim/output.xml"
        self.initial_log_file = "$HOME/coasim/log"
        self.initial_dtd_file = "$HOME/coasim/coasim.dtd"
        self.initial_exec_dir = "$HOME/bin/"
        self.initial_post1 = "xsltproc $HOME/coasim/coasim_to_dot.xsl ~/coasim/output.xml > $HOME/coasim/tmp.dot"
        self.initial_post2 = "dot -Tps -o $HOME/coasim/tmp.ps $HOME/coasim/tmp.dot"
        self.initial_post3 = "ggv $HOME/coasim/tmp.ps"
        self.initial_post4 = ""
        self.q = 0
        self.init_file = commands.getoutput("echo $HOME/coasim/.coasim" )
        self.init_data = []
        try:
            file = open(self.init_file,"r")
            for line in file.readlines():
                self.init_data.append(line)
            file.close()

            self.initial_output_file = string.strip(self.init_data[0])
            self.initial_dtd_file = string.strip(self.init_data[1])
            self.initial_log_file = string.strip(self.init_data[2])
            self.initial_exec_dir = string.strip(self.init_data[3])            
            self.initial_post1 = string.strip(self.init_data[4])
            self.initial_post2 = string.strip(self.init_data[5])
            self.initial_post3 = string.strip(self.init_data[6])
            self.initial_post4 = string.strip(self.init_data[7])
        except IOError:
            self.q = 2
            # do nothing
        self.o.setText(self.initial_output_file)
        self.dtd.setText(self.initial_dtd_file)
        self.log.setText(self.initial_log_file)
        self.exec_dir.setText(self.initial_exec_dir)
        self.post1.setText(str(self.initial_post1))
        self.post2.setText(str(self.initial_post2))
        self.post3.setText(str(self.initial_post3))
        self.post4.setText(str(self.initial_post4))
        
        self.connect(
            self.simulate, SIGNAL('clicked()'),
            self.build_commandline.simulate_clicked
            )

        self.connect(
            self.exec_lines, SIGNAL('clicked()'),
            self.build_commandline.exec_lines_clicked
            )

        self.connect(
            self.log_btn, SIGNAL('clicked()'),
            self.build_commandline.log_clicked
            )


        self.connect(
            self.output_file_btn, SIGNAL('clicked()'),
            self.build_commandline.output_file_clicked
            )

        self.connect(
            self.dtd_btn, SIGNAL('clicked()'),
            self.build_commandline.dtd_clicked
            )


        self.connect(
            self.markers, SIGNAL('lostFocus()'),
            self.build_commandline.markers_changed
        )        


        self.connect(
            self.alsize, SIGNAL('lostFocus()'),
            self.build_commandline.alphabet_changed
        )        


        self.connect(
            self.snp, SIGNAL('clicked()'),
            self.build_commandline.type_changed
        )        

        self.connect(
            self.dsnp, SIGNAL('clicked()'),
            self.build_commandline.type_changed
        )        

        self.connect(
            self.msat, SIGNAL('clicked()'),
            self.build_commandline.type_changed
        )        

        self.connect(
            self.marker_indicator, SIGNAL('activated(int)'),
            self.build_commandline.marker_indicator_activated
        )

        self.connect(
            self.position, SIGNAL('lostFocus()'),
            self.build_commandline.position_changed
        )        

        self.connect(
            self.markerminfreq, SIGNAL('lostFocus()'),
            self.build_commandline.markerminfreq_changed
        )        

        self.connect(
            self.markermaxfreq, SIGNAL('lostFocus()'),
            self.build_commandline.markermaxfreq_changed
        )        

#        self.o.setText(str("$HOME/coasim-output/coasim_" + str(now)))


    def closing(self):
        try:
            file = open(self.init_file,"w")
            file.write(str(self.o.text()) + "\n")
            file.write(str(self.dtd.text()) + "\n")
            file.write(str(self.log.text()) + "\n")
            file.write(str(self.exec_dir.text()) + "\n")
            file.write(str(self.post1.text()) + "\n")
            file.write(str(self.post2.text()) + "\n")
            file.write(str(self.post3.text()) + "\n")
            file.write(str(self.post4.text()) + "\n")
            file.close()
        except IOError:
            self.q = 2
            # do nothing




now = time.time()
#program_dir = commands.getoutput("echo $MAPPING_PROGRAMS")
#simulation_dir = commands.getoutput("echo $MAPPING_SIMULATIONS")
if __name__ == "__main__":
    a = QApplication(sys.argv)
    QObject.connect(a,SIGNAL("lastWindowClosed()"),a,SLOT("quit()"))
    w = coasim()
    a.setMainWidget(w)
    w.show()
    a.exec_loop()
    w.closing()


