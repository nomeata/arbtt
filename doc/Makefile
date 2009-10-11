all: html man

XSLTPROC=xsltproc
XSLTPROC_HTML_OUTDIR=users_guide/
XSLTPROC_HTML_CSS=fptools.css
XSLTPROC_HTML_PARAMS=\
	--stringparam use.id.as.filename 1 \
	--stringparam base.dir $(XSLTPROC_HTML_OUTDIR) \
	--stringparam html.stylesheet $(XSLTPROC_HTML_CSS)
XSLTPROC_HTML_STYLESHEET=/usr/share/xml/docbook/stylesheet/nwalsh/xhtml/profile-chunk.xsl
XSLTPROC_MAN_STYLESHEET=/usr/share/xml/docbook/stylesheet/nwalsh/manpages/profile-docbook.xsl
XSLTPROC_MAN_OUTDIR=man/
XSLTPROC_MAN_OPTIONS=--stringparam man.output.in.separate.dir 1 --stringparam man.output.base.dir

# build targets
#
.PHONY: man html pdf
man: 
	$(XSLTPROC) --xinclude --stringparam profile.condition man $(XSLTPROC_MAN_OPTIONS) $(XSLTPROC_MAN_OUTDIR) $(XSLTPROC_MAN_STYLESHEET) arbtt.xml

html:
	$(XSLTPROC) --xinclude --stringparam profile.condition html $(XSLTPROC_HTML_PARAMS) $(XSLTPROC_HTML_STYLESHEET) arbtt.xml
	cp $(XSLTPROC_HTML_CSS) $(XSLTPROC_HTML_OUTDIR)

pdf:
	#dblatex -tpdf arbtt.xml
	fop -xml arbtt.xml -xsl /usr/share/xml/docbook/stylesheet/docbook-xsl/fo/docbook.xsl -pdf arbtt.pdf

# auxiliary targets
#
.PHONY: clean
clean:
	-rm -rf users_guide/ man/ arbtt.pdf
