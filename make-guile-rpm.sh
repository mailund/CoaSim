#!/bin/bash

version=3.0.0
release=3

(cd Core; make dist)
(cd Guile; make dist)
mkdir coasim-guile-${version}
cd coasim-guile-${version}

tar zxvf ../Core/coasim-core-${version}.tar.gz
rm ../Core/coasim-core-${version}.tar.gz
tar zxvf ../Guile/coasim-guile-${version}.tar.gz
rm ../Guile/coasim-guile-${version}.tar.gz
mv coasim-core-${version} Core
mv coasim-guile-${version} Guile

cd ..
tar zcvf coasim-guile-${version}.tar.gz coasim-guile-${version}
rm -rf coasim-guile-${version}

mv coasim-guile-${version}.tar.gz ~/rpm/SOURCES
rpmbuild -bb coasim-guile.spec
rm ~/rpm/SOURCES/coasim-guile-${version}.tar.gz
mv ~/rpm/RPMS/i386/coasim-guile-${version}-${release}.i386.rpm .
