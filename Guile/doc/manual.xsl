<?xml version="1.0" encoding="ISO-8859-1" ?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:include href="htmltags.xsl"/>
  
  <xsl:output method="xml" encodig="iso-8856-1"
	      doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN"
              doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"/>
  
  <xsl:template match="/">
    <html>
      <head>
       <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
       <title>GeneRecon Scheme</title>
       <link rel="stylesheet" type="text/css" href="guile-bindings.css" />
      </head>
      <body>

      <xsl:for-each select="manual">
        <xsl:apply-templates select="."/>
      </xsl:for-each>

      </body>
    </html>
  </xsl:template>
  
  <xsl:template match="section">
    <h1><xsl:apply-templates/></h1>
  </xsl:template>

  <xsl:template match="subsection">
    <h2><xsl:apply-templates/></h2>
  </xsl:template>

  <xsl:template match="example">
    <pre class="example"><xsl:apply-templates/></pre>
  </xsl:template>

</xsl:stylesheet>

<!-- Keep this comment at the end of the file
Local variables:
mode: xml
sgml-omittag:t
sgml-shorttag:t
sgml-namecase-general:nil
sgml-general-insert-case:lower
sgml-minimize-attributes:nil
sgml-always-quote-attributes:t
sgml-indent-step:2
sgml-indent-data:t
sgml-parent-document:nil
sgml-exposed-tags:nil
sgml-local-catalogs:nil
sgml-local-ecat-files:nil
End:
-->
