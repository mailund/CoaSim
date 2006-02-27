#!/bin/bash

coreversion=4.1.2
version=1.0
release=1

(cd Core; make dist)
(cd Python; python setup.py sdist)
mkdir coasim-python-${version}
cd coasim-python-${version}

tar zxvf ../Core/coasim-core-${coreversion}.tar.gz
mv ../Core/coasim-core-${coreversion}.tar.gz ..
tar zxvf ../Python/dist/coasim-python-${version}.tar.gz
mv ../Python/dist/coasim-python-${version}.tar.gz ..
mv coasim-core-${coreversion} Core
mv coasim-python-${version} Python

cd ..
tar zcvf coasim-python-${version}.tar.gz coasim-python-${version}
rm -rf coasim-python-${version}

#mv coasim-guile-${version}.tar.gz ~/rpm/SOURCES
#rpmbuild -bb coasim-guile.spec
#mv ~/rpm/RPMS/i386/coasim-guile-${version}-${release}.i386.rpm .
#mv ~/rpm/SOURCES/coasim-guile-${version}.tar.gz .
