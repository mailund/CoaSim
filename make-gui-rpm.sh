#!/bin/bash

core_version=3.0.1
version=3.0.2
release=1

(cd Core; make dist)
(cd GUI;  make dist)
mkdir coasim-gui-${version}
cd coasim-gui-${version}

tar zxvf ../Core/coasim-core-${core_version}.tar.gz
rm ../Core/coasim-core-${core_version}.tar.gz
tar zxvf ../GUI/coasim_gui.tar.gz
rm ../GUI/coasim_gui.tar.gz
mv coasim-core-${core_version} Core
mv coasim_gui GUI

cp ../GUI/coasim_gui .
cp ../GUI/coasim-icon.png .
cp ../GUI/bioinformatics-icon.png .
cp ../GUI/coasim.desktop .
cp ../GUI/BioinformaticsApS.directory .
cp ../GUI/bioinformatics-aps.menu .

cd ..
tar zcvf coasim-gui-${version}.tar.gz coasim-gui-${version}
rm -rf coasim-gui-${version}

mv coasim-gui-${version}.tar.gz ~/rpm/SOURCES
rpmbuild -bb --rmsource coasim-gui.spec
mv ~/rpm/RPMS/i386/coasim-gui-${version}-${release}.i386.rpm .
rm ~/rpm/RPMS/i386/coasim-gui-debuginfo-${version}-${release}.i386.rpm

