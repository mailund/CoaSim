
cpp_sources = $(wildcard ../*.cc)
scheme_sources = $(shell find ../scheme -name *.scm)
guile_sources = $(cpp_sources) $(scheme_sources)
tools = extract_guile_comments.py 

all: bindings.html

bindings.html: bindings.xml guile-bindings.xsl
	xsltproc bindings.xml > bindings.html

bindings.xml: $(guile_sources) $(tools)
	./extract_guile_comments.py $(guile_sources) > bindings.xml
