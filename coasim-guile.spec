Summary: CoaSim -- A coalescence process simulator
Name: coasim-guile
Version: 3.1.0
Release: 1
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
(cd Core && ./configure && make)
(cd Guile && ./configure && make)

%install
rm -rf $RPM_BUILD_ROOT
(cd Guile && make DESTDIR=$RPM_BUILD_ROOT install)

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
/usr/local/bin/coasim_guile

%doc /usr/local/share/coasim/doc/*.html
%doc /usr/local/share/coasim/doc/*.css
%doc /usr/local/share/coasim/test-input/*.scm

/usr/local/share/coasim/scheme/coasim/*.scm
/usr/local/share/coasim/scheme/coasim/SNP/*.scm



%changelog
* Mon Oct 11 2004 Thomas Mailund <mailund@mailund.dk>
- Initial build.

