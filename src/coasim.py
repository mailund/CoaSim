'''
Wrapper for coasim.

Copyright (C) 2004, Bioinformatics ApS <http://www.bioinformatics.dk>
'''

_coasim_cmd = 'coasim' # name or path to coasim tool

class Configuration(object):
    def __init__(self,
                 no_leaves,
                 gene_conversion_rate,
                 gene_conversion_length,
                 recombination_rate,
                 growth,
                 positions,
                 markers,
                 low_freq, high_freq,
                 no_values, mutation_rates):
        '''Configuration of a simulation.

        Sets options for the simulation.  The options are:

        no_leaves -- the number of leaves in the arc.

        gene_conversion_rate/gene_conversion_length -- the gene
        conversion parameters.

        recombination_rate -- recombination rate.

        growth -- growth parameter.

        positions -- a list of the marker positions (as floats).

        markers -- a llist of marker types, matching the list of
        positions.  The types are given as strings from the set
        {"trait", "snp", "ms"}, where "trait" denotes a trait marker,
        "snp" a SNP marker, and "ms" a micro-satellite marker.

        low_freq/high_freq -- bounds of the frequencies for mutations
        for traits and values; should be lists with a value for each
        marker, but the values are ignored for ms-markers.

        no_values -- the number of possible values for
        micro-satellite; should be a list of integers with a value for
        each marker, but the values are ignored for snp- and
        trait-markers.

        mutation_rates -- mutation rate per marker (only used for
        micro-satellite, ignored for snp- and trait-markers.)

        '''
        
        self.no_leaves = no_leaves
        self.gene_conversion_rate = gene_conversion_rate
        self.gene_conversion_length = gene_conversion_length
        self.recombination_rate = recombination_rate
        self.growth = growth

        # FIXME: should probably do some type-checking here...
        self.positions = ' '.join([str(p) for p in positions])
        self.markers = ' '.join(markers)
        self.low_freq = ' '.join([str(f) for f in low_freq])
        self.high_freq = ' '.join([str(f) for f in high_freq])
        self.no_values = ' '.join([str(n) for n in no_values])
        self.mutation_rates = ' '.join([str(r) for r in mutation_rates])
        

    def run_commands(self):
        'Create the run-command options'
        template = '''
no_leaves: %(no_leaves)d

gene_conversion_rate:   %(gene_conversion_rate)f
gene_conversion_length: %(gene_conversion_length)f

recombination_rate: %(recombination_rate)f

growth: %(growth)f

positions: %(positions)s
markers:   %(markers)s
low_freq:  %(low_freq)s
high_freq: %(high_freq)s
no_values: %(no_values)s
mutation_rates: %(mutation_rates)s
        '''
        return template % vars(self)


def coasim(conf):
    '''Run coasim simulation.

    Run a simulation, writing the output to outfile.

    Returns a list of haplotypes if successful, None if a failure occurs.'''

    import tempfile, os
    rcfile = tempfile.mktemp()
    outfile = tempfile.mktemp()
    try:
        f = open(rcfile,'w')
        print >> f, conf.run_commands()
        f.close()

        res = os.system(_coasim_cmd+' -r '+rcfile+' -o '+outfile)
        if res != 0: return None

        haplotypes = []
        f = open(outfile)
        for line in f:
            marker_vals = map(int, line.split())
            haplotypes.append(marker_vals)
        return haplotypes

    finally:
        os.unlink(rcfile)
        os.unlink(outfile)
    

if __name__ == '__main__':
    c = Configuration(no_leaves = 5,
                      gene_conversion_rate = 0.0,
                      gene_conversion_length = 0.0,
                      recombination_rate = 0.5,
                      growth = 0.5,

                      positions = [0.1,     0.2,   0.3,  0.5],
                      markers =   ['trait', 'snp', 'ms', 'snp'],
                      low_freq =  [0.1,     0.1,   0.0,  0.1],
                      high_freq = [0.2,     0.3,   0.0,  0.2],
                      no_values = [0,       0,     4,    0],
                      mutation_rates = [0.0,0.0,1.0,0.0])
    print c.run_commands()

    print coasim(c)
                 
