#!/bin/bash

version=3.0.0
release=4

(cd Core; make)
(cd GUI; make)
mkdir coasim-gui-${version}
cp GUI/coasim_gui coasim-gui-${version}
cp GUI/coasim-icon.png coasim-gui-${version}
cp GUI/bioinformatics-icon.png coasim-gui-${version}
cp GUI/coasim.desktop coasim-gui-${version}
cp GUI/BioinformaticsApS.directory coasim-gui-${version}
cp GUI/bioinformatics-aps.menu coasim-gui-${version}

tar zcvf coasim-gui-${version}.tar.gz coasim-gui-${version}
rm -rf coasim-gui-${version}

mv coasim-gui-${version}.tar.gz ~/rpm/SOURCES
rpmbuild -bb coasim-gui.spec
rm ~/rpm/SOURCES/coasim-gui-${version}.tar.gz
mv ~/rpm/RPMS/i386/coasim-gui-${version}-${release}.i386.rpm .
