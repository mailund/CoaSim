Summary: CoaSim -- A coalescence process simulator
Name: coasim-gui
Version: 3.0.0
Release: 3
License: GPL
Group: Applications/Bioinformatics
URL: http://www.birc.dk/Software/Coasim
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root

%description
CoaSim -- A coalescence process simulator

%prep
%setup -q

%build

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/local/bin
mkdir -p $RPM_BUILD_ROOT/usr/local/share/pixmaps
mkdir -p $RPM_BUILD_ROOT/usr/local/share/applications
mkdir -p $RPM_BUILD_ROOT/usr/local/share/desktop-directories
mkdir -p $RPM_BUILD_ROOT/etc/xdg/menus/applications-merged

cp coasim_gui      $RPM_BUILD_ROOT/usr/local/bin/

cp coasim-icon.png         $RPM_BUILD_ROOT/usr/local/share/pixmaps
cp bioinformatics-icon.png $RPM_BUILD_ROOT/usr/local/share/pixmaps

cp coasim.desktop  $RPM_BUILD_ROOT/usr/local/share/applications

cp BioinformaticsApS.directory \
   $RPM_BUILD_ROOT/usr/local/share/desktop-directories
cp bioinformatics-aps.menu \
   $RPM_BUILD_ROOT/etc/xdg/menus/applications-merged

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
/usr/local/bin/coasim_gui

/usr/local/share/pixmaps/coasim-icon.png
/usr/local/share/pixmaps/bioinformatics-icon.png
/usr/local/share/applications/coasim.desktop
/usr/local/share/desktop-directories/BioinformaticsApS.directory
/etc/xdg/menus/applications-merged/bioinformatics-aps.menu



%changelog
* Mon Oct 11 2004 Thomas Mailund <mailund@mailund.dk>
- Initial build.

