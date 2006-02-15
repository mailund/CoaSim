__doc__ = '''
CoaSim/Python -- Python bindings for Coasim
Copyright (C) 2006 by Thomas Mailund <mailund@mailund.dk>

This module contains the functionality for dealing with population
structure.  '''

class _PopStructure(object):
    '''Abstract class for population structures.  Used only for
    type-checking purposes.'''
    def acceptVisitor(self,v,PO=True):
        '''Part of the visitor design pattern.  The PO parameter
        determines if the traversal should be post order (children
        visited before parents) or pre order.'''
        raise NotImplementedError('acceptVisitor')
    

class Sample(_PopStructure):
    '''Representation of a present-day sample.'''
    def __init__(self,sampleSize):
        '''Builds a sample of sampleSize sequences.'''
        if sampleSize <= 0:
            raise ValueError('Sample size must be positive.')
        self.sampleSize = sampleSize
        self.endTime = 0

    def acceptVisitor(self,v,PO=True):
        '''Part of the visitor design pattern.  The PO parameter
        determines if the traversal should be post order (children
        visited before parents) or pre order (parents before
        children).'''
        v.visitSample(self)

    def __str__(self):
        return 'Sample(%d)' % self.sampleSize
    __repr__ = __str__


class Population(_PopStructure):
    '''Representation of a population (including any sub-structure).'''
    def __init__(self, relativeSize, subStructure, epochs=None, name=None):
        '''Builds a population with size relativeSize*N (where N is an
        assumed global population size used to define the time unit,
        1=2N).  Any sub-structure is given as the subStructure
        argument, and any epochs (bottlenecks, exponential growth) are
        given as a list in the optional epochs argument.  '''
        if relativeSize <= 0:
            raise ValueError('Relative size must be positive.')
        if not isinstance(subStructure,_PopStructure):
            raise TypeError('Sub-structure most be a population structure.')

        if epochs is not None:
            for e in epochs:
                if not isinstance(e,_Epoch):
                    raise TypeError('Epochs list must only include epochs.')
                if e.startTime < subStructure.endTime:
                    raise ValueError('Epoch starting before population.')

        if name is not None and not isinstance(name,str):
            raise TypeError('Population name must be a string.')

        # FIXME: check that overlapping epochs are also nested!!!

        self.relativeSize = relativeSize
        self.subStructure = subStructure
        self.epochs = epochs
        self.name = name

        epochEndTime = 0
        if epochs is not None:
            endTimes = [e.endTime for e in epochs if e.endTime is not None]
            if endTimes != []: epochEndTime = max(endTimes)
        self.endTime = max(epochEndTime,subStructure.endTime)

    def acceptVisitor(self,v,PO=True):
        '''Part of the visitor design pattern.  The PO parameter
        determines if the traversal should be post order (children
        visited before parents) or pre order.'''
        if PO:
            self.subStructure.acceptVisitor(v)
            v.visitPopulation(self)
        else:
            v.visitPopulation(self)
            self.subStructure.acceptVisitor(v)

    def __str__(self):
        if self.epochs is not None and self.epochs != []:
            epochStr = ',epochs=%s' % str(self.epochs)
        else:
            epochStr = ''
        if self.name is not None:
            nameStr = ',name="%s"' % self.name
        else:
            nameStr = ''
        return 'Population(%g,%s%s%s)' % \
               (self.relativeSize,str(self.subStructure),epochStr,nameStr)
    __repr__ = __str__


class Merge(_PopStructure):
    '''Representation of a merging point for two or more populations.'''
    def __init__(self,mergeTime,populations):
        '''Builds a merging point at time mergeTime, merging the
        populations give as a sequence of Population objects.'''

        if mergeTime <= 0:
            raise ValueError('Merge time must be positive.')
        if len(populations) < 2:
            raise ValueError('At least two populations must be merged.')
        for p in populations:
            if not isinstance(p,Population):
                raise TypeError('Sub-populations must be populations.')
        if mergeTime < max([p.endTime for p in populations]):
            raise ValueError('Merge time is sooner than the sub-structures allow.')

        self.mergeTime = mergeTime
        self.populations = populations
        self.endTime = mergeTime

        # update sub-populations' end time
        for p in self.populations:
            p.endTime = mergeTime

    def acceptVisitor(self,v,PO=True):
        '''Part of the visitor design pattern.  The PO parameter
        determines if the traversal should be post order (children
        visited before parents) or pre order.'''
        if PO:
            for p in self.populations:
                p.acceptVisitor(v)
            v.visitMerge(self)
        else:
            v.visitMerge(self)
            for p in self.populations:
                p.acceptVisitor(v)

    def __str__(self):
        return 'Merge(%g,%s)' % (self.mergeTime,str(self.populations))
    __repr__ = __str__


class _Epoch(object):
    '''Abstract class for epochs.  Used only for type-checking purposes.'''
    def acceptVisitor(self,v):
        '''Part of the visitor design pattern. '''
        raise NotImplementedError('acceptVisitor')


class Bottleneck(_Epoch):
    '''A bottleneck epoch.'''
    def __init__(self, relativeSize, startTime, endTime = None):
        '''Build a bottleneck of size relativeSize*Np where Np is the
        population size of the local surrounding population.  The
        bottleneck starts at time startTime and ends at time endTime,
        or at the end of the existence of the local population if
        endTime is None.'''
        if relativeSize <= 0:
            raise ValueError('Relative size must be positive.')
        if not (isinstance(startTime,float) or isinstance(startTime,int)):
            raise TypeError('Start time must be a number.')
        if startTime < 0:
            raise ValueError('Start time must be non-negative.')
        if not (endTime is None or isinstance(endTime,float) \
                or isinstance(endTime,int)):
            raise TypeError('End time must be a number.')
        if endTime is not None and endTime <= startTime:
            raise ValueError('End time must come after start time.')

        self.relativeSize = relativeSize
        self.startTime = startTime
        self.endTime = endTime

    def acceptVisitor(self,v):
        '''Part of the visitor design pattern. '''
        v.visitBottleneck(self)

    def __str__(self):
        if self.endTime is not None:
            return 'Bottleneck(%g,%g,%g)' % \
                   (self.relativeSize, self.startTime, self.endTime)
        else:
            return 'Bottleneck(%g,%g)' % (self.relativeSize, self.startTime)
    __repr__ = __str__
            

class Growth(_Epoch):
    '''A bottleneck epoch.'''
    def __init__(self, beta, startTime, endTime = None):
        '''Build a period of exponential growth with growth parameter
        beta.  The bottleneck starts at time startTime and ends at
        time endTime, or at the end of the existence of the local
        population if endTime is None.'''
        if beta <= 0:
            raise ValueError('Growth factor beta must be positive.')
        if not (isinstance(startTime,float) or isinstance(startTime,int)):
            raise TypeError('Start time must be a number.')
        if startTime < 0:
            raise ValueError('Start time must be non-negative.')
        if not (endTime is None or isinstance(endTime,float) \
                or isinstance(endTime,int)):
            raise TypeError('End time must be a number.')
        if endTime is not None and endTime <= startTime:
            raise ValueError('End time must come after start time.')

        self.beta = beta
        self.startTime = startTime
        self.endTime = endTime

    def acceptVisitor(self,v):
        '''Part of the visitor design pattern. '''
        v.visitGrowth(self)

    def __str__(self):
        if self.endTime is not None:
            return 'Growth(%g,%g,%g)' % (self.beta, self.startTime, self.endTime)
        else:
            return 'Growth(%g,%g)' % (self.beta, self.startTime)
    __repr__ = __str__


class Migration(object):
    '''A period of migration.'''
    def __init__(self,src,dst,r,startTime=None,endTime=None):
        '''Build a period of migration with migration (backwards in
        time) from src to dst (so dst to src forward in time), with
        scaled migration rate r, starting at time startTime (or
        max(src.startTime,dst.startTime) if None) and ending at time
        endTime (or min(src.endTime,dst.endTime) if None.

        The populations src and dst can be given either as their names
        or references to their populations.'''

        def checkPop(pop,whichPop):
            if isinstance(pop,str): return
            if isinstance(pop,Population): return
            raise TypeError('%s population is not of a supported type.' % whichPop)
        checkPop(src,'Source')
        checkPop(dst,'Destination')
        if src is dst: raise ValueError('Populations must be distinct.')
        if r <= 0: raise ValueError('Migration rate must be positive.')

        self.src = src
        self.dst = dst
        self.r = r
        self.startTime = startTime
        self.endTime = endTime

    def __str__(self):
        def pop2str(pop):
            if isinstance(pop,str): return '"%s"' % pop
            if isinstance(pop,Population):
                if pop.name is not None: return '"%s"' % pop.name
                else: return str(pop)
            return '<illegal type>'
        srcStr = pop2str(self.src)
        dstStr = pop2str(self.dst)
        startTimeStr = endTimeStr = ''
        if self.startTime is not None:
            startTimeStr = ',startTime=%g' % self.startTime
        if self.endTime is not None:
            endTimeStr = ',endTime=%g' % self.endTime
        return 'Migration(%s,%s,%g%s%s)' % \
              (srcStr,dstStr,self.r,startTimeStr,endTimeStr)


class PopVisitor(object):
    '''Abstract visitor for population structures.'''
    def visitSample(self,sample):
        '''Invoked on visited samples.'''
        pass
    def visitPopulation(self,population):
        '''Invoked on visited populations.'''
        pass
    def visitMerge(self,merge):
        '''Invoked on visited merges.'''
        pass

class EpochVisitor(object):
    '''Abstract visitor for epochs.'''
    def visitBottleneck(self,bn):
        '''Invoked on visited bottlenecks.'''
        pass
    def visitGrowth(self,g):
        '''Invoked on visited growth epochs.'''
        pass


def _numberPopulations(popStruct):
    '''Assign numbers to populations in popStruct and return a mapping
    from population names to their numbers, start-, and end-time.'''
    nameDict = dict()
    class V(PopVisitor):
        def __init__(self):
            self.count = 0
        def visitSample(self,sample):
            assert not hasattr(sample,'number')
            sample.number = self.count
            self.count += 1
        def visitPopulation(self,pop):
            assert not hasattr(pop,'number')
            pop.number = pop.subStructure.number
            if pop.name is not None:
                nameDict[pop.name] = (pop.number,pop.subStructure.endTime,pop.endTime)
        def visitMerge(self,merge):
            assert not hasattr(merge,'number')
            merge.number = min([p.number for p in merge.populations])
    popStruct.acceptVisitor(V())
    return nameDict

def _collectSampleSizes(popStruct):
    '''Collect list of sample sizes.'''
    sampleSizes = []
    class V(PopVisitor):
        def visitSample(self,s):
            sampleSizes.append(s.sampleSize)
    popStruct.acceptVisitor(V())
    return sampleSizes

def _collectEvents(popStruct):
    '''Collect the events represented by the population structure.'''
    events = []

    class EV(EpochVisitor):
        def __init__(self,pop):
            self.pop = pop
        def visitBottleneck(self,bn):
            if bn.endTime is None:
                bn.endTime = self.pop.endTime
            e = ('bottleneck', self.pop.number, float(bn.relativeSize),
                 float(bn.startTime), float(bn.endTime))
            events.append(e)
        def visitGrowth(self,g):
            if g.endTime is None:
                g.endTime = self.pop.endTime
            e = ('growth', self.pop.number, float(g.beta),
                 float(g.startTime), float(g.endTime))
            events.append(e)

    class PV(PopVisitor):
        def visitPopulation(self,pop):
            if pop.relativeSize != 1:
                e = ('bottleneck', pop.number, float(pop.relativeSize),
                     float(pop.subStructure.endTime), float(pop.endTime))
                events.append(e)
            if pop.epochs is not None:
                ev = EV(pop)
                for e in pop.epochs:
                    e.acceptVisitor(ev)
                
        def visitMerge(self,merge):
            e = ('merge', float(merge.mergeTime),
                 [p.number for p in merge.populations])
            events.append(e)

    popStruct.acceptVisitor(PV())

    return events

def _collectMigrationEvents(migrations,popNameMap):
    '''Collect the migration events in the form accepted by the simulator.'''

    def popInfo(pop):
        if isinstance(pop,str): return popNameMap[pop]
        else: return pop.number, pop.subStructure.endTime, pop.endTime
    
    migEvents = []
    for m in migrations:
        srcNum,srcStart,srcEnd = popInfo(m.src)
        dstNum,dstStart,dstEnd = popInfo(m.dst)
        if srcEnd is None: srcEnd = float('inf')
        if dstEnd is None: dstEnd = float('inf')
        earliestStart = max(srcStart,dstStart)
        latestEnd = min(srcEnd,dstEnd)
        if m.startTime is None: m.startTime = earliestStart
        if m.endTime is None: m.endTime = latestEnd

        if m.startTime < earliestStart:
            errMsg = '"%s" has as a start time earlier than its populations.' % str(m)
            raise ValueError(errMsg)
        if m.endTime > latestEnd:
            errMsg = '"%s" has as an end time later than its populations.' % str(m)
            raise ValueError(errMsg)

        e = ('migration',srcNum,dstNum,float(m.r),float(m.startTime),float(m.endTime))
        migEvents.append(e)

    return migEvents

def compile(popStruct, migrationList = []):
    '''Compile a population structure into the sample-size list and
    event-list needed for the simulator.'''
    if not isinstance(popStruct,Population):
        raise TypeError('The root of a population structure must be a population.')
    popStruct.endTime = float('inf')
    nameDict = _numberPopulations(popStruct)
    structEvents = _collectEvents(popStruct)
    migEvents = _collectMigrationEvents(migrationList,nameDict)
    events = structEvents + migEvents
    sampleSizes = _collectSampleSizes(popStruct)
    return events, sampleSizes


if __name__ == '__main__':
    # testing sample
    try:
        Sample(-1)
        assert False
    except ValueError, e:
        assert str(e) == 'Sample size must be positive.'
    try:
        Sample(0)
        assert False
    except ValueError, e:
        assert str(e) == 'Sample size must be positive.'
    assert str(Sample(10)) == 'Sample(10)'

    # testing population
    try:
        Population(-1,None)
        assert False
    except ValueError, e:
        assert str(e) == 'Relative size must be positive.'
    try:
        Population(0,None)
        assert False
    except ValueError, e:
        assert str(e) == 'Relative size must be positive.'

    try:
        Population(1,None)
        assert False
    except TypeError, e:
        assert str(e) == 'Sub-structure most be a population structure.'

    try:
        Population(1,Sample(10),[None])
        assert False
    except TypeError, e:
        assert str(e) == 'Epochs list must only include epochs.'

    try:
        Population(1,Sample(10),name=2)
    except TypeError, e:
        assert str(e) == 'Population name must be a string.'

    assert str(Population(10,Sample(10))) == 'Population(10,Sample(10))'
    assert str(Population(10,Sample(10),name="N")) == \
           'Population(10,Sample(10),name="N")'

    # testing merge
    try:
        Merge(-1,None)
        assert False
    except ValueError, e:
        assert str(e) == 'Merge time must be positive.'
    try:
        Merge(0,None)
        assert False
    except ValueError, e:
        assert str(e) == 'Merge time must be positive.'
    try:
        Merge(1,None)
        assert False
    except TypeError, e:
        assert str(e) == 'len() of unsized object'
    try:
        Merge(1,[1,2])
        assert False
    except TypeError, e:
        assert str(e) == 'Sub-populations must be populations.'

    m = Merge(3,[Population(1,Sample(1)),Population(2,Sample(2))])
    assert str(m) == 'Merge(3,[Population(1,Sample(1)), Population(2,Sample(2))])'
    assert m.endTime == 3
    

    # testing bottleneck
    try:
        Bottleneck(-1,None)
        assert False
    except ValueError, e:
        assert str(e) == 'Relative size must be positive.'
    try:
        Bottleneck(0,None)
        assert False
    except ValueError, e:
        assert str(e) == 'Relative size must be positive.'

    try:
        Bottleneck(1,None)
        assert False
    except TypeError, e:
        assert str(e) == 'Start time must be a number.'
    assert str(Bottleneck(1,0)) == 'Bottleneck(1,0)'
    assert str(Bottleneck(1,0.0)) == 'Bottleneck(1,0)'
    assert str(Bottleneck(1,0,None)) == 'Bottleneck(1,0)'

    try:
        Bottleneck(1,0,[])
        assert False
    except TypeError, e:
        assert str(e) == 'End time must be a number.'
    try:
        Bottleneck(1,0,0)
        assert False
    except ValueError, e:
        assert str(e) == 'End time must come after start time.'
    try:
        Bottleneck(1,1,0)
        assert False
    except ValueError, e:
        assert str(e) == 'End time must come after start time.'
    
    assert str(Bottleneck(1,0,1.5)) == 'Bottleneck(1,0,1.5)'


    # testing growth
    try:
        Growth(-1,None)
        assert False
    except ValueError, e:
        assert str(e) == 'Growth factor beta must be positive.'
    try:
        Growth(0,None)
        assert False
    except ValueError, e:
        assert str(e) == 'Growth factor beta must be positive.'

    try:
        Growth(1,None)
        assert False
    except TypeError, e:
        assert str(e) == 'Start time must be a number.'
    assert str(Growth(1,0)) == 'Growth(1,0)'
    assert str(Growth(1,0.0)) == 'Growth(1,0)'
    assert str(Growth(1,0,None)) == 'Growth(1,0)'

    try:
        Growth(1,0,[])
        assert False
    except TypeError, e:
        assert str(e) == 'End time must be a number.'
    try:
        Growth(1,0,0)
        assert False
    except ValueError, e:
        assert str(e) == 'End time must come after start time.'
    try:
        Growth(1,1,0)
        assert False
    except ValueError, e:
        assert str(e) == 'End time must come after start time.'
    
    assert str(Growth(1,0,1.5)) == 'Growth(1,0,1.5)'


    # testing epochs in populations...
    s = Sample(1)
    assert Population(1,s,epochs=[]).endTime == 0
    b = Bottleneck(1,0,0.5)
    assert Population(1,s,epochs=[b]).endTime == 0.5
    g = Growth(1,0,1.5)
    assert Population(1,s,epochs=[g]).endTime == 1.5
    assert Population(1,s,epochs=[b,g]).endTime == 1.5
    assert Population(1,s,epochs=[g,b]).endTime == 1.5

    assert Merge(2, [Population(1,s,epochs=[g]),
                     Population(1,s,epochs=[b])]).endTime == 2
    assert Merge(1.5, [Population(1,s,epochs=[g]),
                       Population(1,s,epochs=[b])]).endTime == 1.5

    try:
        Merge(1, [Population(1,s,epochs=[g]), Population(1,s,epochs=[b])])
        assert False
    except ValueError, e:
        assert str(e) == 'Merge time is sooner than the sub-structures allow.'



    P = Population
    M = Merge
    S = Sample
    B = Bottleneck
    G = Growth

    try:
        P(1, M(1,[P(1,S(1)),P(1,S(1))]), epochs=[B(1,0.5,1.5)])
        assert False
    except ValueError, e:
        assert str(e) == 'Epoch starting before population.'
    P(1, M(1,[P(1,S(1)),P(1,S(1))]), epochs=[B(1,1,1.5)])

    # visiting
    popStruct = P(1, epochs=[B(0.2, 1, 2)],
                  subStructure=M(1, [P(2,S(10),epochs=[G(10,0)]),
                                     P(3,S(20),epochs=[B(0.1,0.1,0.5)])]))

    class KindCountVisitor(PopVisitor):
        def __init__(self):
            self.counts = {'S':0, 'P':0, 'M':0}
        def visitSample(self,_):
            self.counts['S'] += 1
        def visitPopulation(self,_):
            self.counts['P'] += 1
        def visitMerge(self,_):
            self.counts['M'] += 1
    v = KindCountVisitor()
    popStruct.acceptVisitor(v)
    assert v.counts == {'P':3,'S':2,'M':1}

    # numbering populations
    assert _numberPopulations(popStruct) == {}
    namedStruct = P(1, epochs=[B(0.2, 1, 2)], name='P1',
                    subStructure=M(1,[P(2,S(10),epochs=[G(10,0)], name='P2'),
                                      P(3,S(20),epochs=[B(0.1,0.1,0.5)], name='P3')]))
    namedStruct.endTime = float('inf')
    assert _numberPopulations(namedStruct) == {'P1':(0,1,float('inf')),
                                               'P2':(0,0,1),'P3':(1,0,1)}

    namedStruct = P(1, epochs=[B(0.2, 1, 2)], name='P1',
                    subStructure=M(1,[P(2,S(10),epochs=[G(10,0)], name='P2'),
                                      P(3,S(20),epochs=[B(0.1,0.1,0.5)], name='P3'),
                                      P(3,S(20),epochs=[B(0.1,0.1,0.5)], name='P4')]))
    namedStruct.endTime = float('inf')
    assert _numberPopulations(namedStruct) == {'P1':(0,1,float('inf')),
                                               'P2':(0,0,1), 'P3':(1,0,1),
                                               'P4':(2,0,1)}

    namedStruct = P(1, name='P1',
                    subStructure=M(2,[P(1,name='P2',
                                        subStructure=M(1, [P(2,S(1),name='P3'),
                                                           P(1,S(2),name='P4')])),
                                      P(1,name='P5',
                                        subStructure=M(1, [P(2,S(3),name='P6'),
                                                           P(1,S(4),name='P7')]))]))
    namedStruct.endTime = float('inf')
    assert _numberPopulations(namedStruct) == {'P1':(0,2,float('inf')),
                                               'P2':(0,1,2),
                                               'P3':(0,0,1),'P4':(1,0,1),
                                               'P5':(2,1,2),
                                               'P6':(2,0,1),'P7':(3,0,1)}

    assert _collectSampleSizes(namedStruct) == [1,2,3,4]


    # collecting events
    popStruct = P(4, epochs=[B(0.25, 1, 2), B(25, 10)],
                  subStructure=M(1, [P(1,S(10),epochs=[G(10,0)]),
                                     P(3,S(20),epochs=[B(0.5,0.25,0.5)])]))
    popStruct.endTime = float('inf')
    _numberPopulations(popStruct)
    assert _collectEvents(popStruct) == \
           [('growth', 0, 10, 0, 1),
            ('bottleneck', 1, 3, 0, 1), ('bottleneck', 1, 0.5, 0.25, 0.5),
            ('merge', 1, [0, 1]),
            ('bottleneck', 0, 4, 1, float('inf')),
            ('bottleneck', 0, 0.25, 1, 2),
            ('bottleneck', 0, 25, 10, float('inf'))]
    
    # migrations
    assert str(Migration('foo','bar',0.1)) == 'Migration("foo","bar",0.1)'
    foo = P(1,S(1))
    bar = P(1,S(1))
    assert str(Migration(foo,bar,0.1)) == \
           'Migration(Population(1,Sample(1)),Population(1,Sample(1)),0.1)'
    foo = P(1,S(1),name='foo')
    bar = P(1,S(1),name='bar')
    assert str(Migration(foo,bar,0.1)) == 'Migration("foo","bar",0.1)'

    assert str(Migration(foo,bar,0.1,startTime=3.4)) == \
           'Migration("foo","bar",0.1,startTime=3.4)'
    assert str(Migration(foo,bar,0.1,endTime=3.4)) == \
           'Migration("foo","bar",0.1,endTime=3.4)'

    try:
        Migration(None,1,0.1)
        assert False
    except TypeError, e:
        assert str(e) == 'Source population is not of a supported type.'
    try:
        Migration(foo,None,0.1)
        assert False
    except TypeError, e:
        assert str(e) == 'Destination population is not of a supported type.'

    try:
        Migration(foo,foo,0.1)
        assert False
    except ValueError, e:
        assert str(e) == 'Populations must be distinct.'

    try:
        Migration(foo,bar,-0.1)
        assert False
    except ValueError, e:
        assert str(e) == 'Migration rate must be positive.'
    try:
        Migration(foo,bar,0)
        assert False
    except ValueError, e:
        assert str(e) == 'Migration rate must be positive.'



    popStruct = P(4, M(1, [P(1,S(10), name='foo'), P(3,S(20), name='bar')]),
                  epochs=[B(0.25, 1, 2), B(25, 10)])
    popStruct.endTime = float('inf')
    nameDict = _numberPopulations(popStruct)
    migrations = [Migration('foo','bar',0.1)]
    assert _collectMigrationEvents(migrations,nameDict) == \
           [('migration',0,1,0.1,0,1)]
    migrations = [Migration('foo','bar',0.1,0.2)]
    assert _collectMigrationEvents(migrations,nameDict) == \
           [('migration',0,1,0.1,0.2,1)]
    migrations = [Migration('foo','bar',0.1,endTime=0.2)]
    assert _collectMigrationEvents(migrations,nameDict) == \
           [('migration',0,1,0.1,0,0.2)]
    
