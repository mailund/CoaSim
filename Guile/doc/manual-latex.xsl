<?xml version="1.0" encoding="ISO-8859-1" ?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:output method="text" encodig="iso-8856-1"/>
  
  <xsl:template match="/">
    <xsl:for-each select="manual">
      <xsl:apply-templates select="."/>
    </xsl:for-each>
  </xsl:template>
  
  <xsl:template match="section">
    \section{<xsl:apply-templates/>}
  </xsl:template>

  <xsl:template match="subsection">
    \subsection{<xsl:apply-templates/>}
  </xsl:template>

  <xsl:template match="example">
    \begin{verbatim}
      <xsl:apply-templates/>
    \end{verbatim}
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
