<?xml version="1.0" encoding="ISO-8859-1" ?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:include href="htmltags.xsl"/>
  
  <xsl:output method="xml" encodig="iso-8856-1"
	      doctype-public="-//W3C//DTD XHTML 1.0 Strict//EN"
              doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"/>
  
  <xsl:template match="/">
    <html>
      <head>
	<title>CoaSim Scheme Bindings</title>
	<link rel="stylesheet" type="text/css" href="guile-bindings.css"/>
      </head>
      <body>

       <h1>CoaSim specific scheme functions</h1>
       <div class="header">
         <h2>Main (coasim) module</h2>
       	<ul>
       	  <xsl:for-each select="guile-bindings/method">
       	    <xsl:sort select="@name" data-type="text"/>
       	    <li><a>
       	      <xsl:attribute name="href">#<xsl:value-of select="@name"/>
       	      </xsl:attribute>
       	      <xsl:value-of select="@name"/>
       	    </a></li>
       	  </xsl:for-each>
       	</ul>
         <h2>Extra modules</h2>
         <xsl:for-each select="guile-bindings/module">
           <xsl:sort select="@name" data-type="text"/>
             <p><b><xsl:value-of select="@name"/></b></p>
             <xsl:apply-templates select="brief"/>
             <ul>
               <xsl:for-each select="./method">
                 <xsl:sort select="@name" data-type="text"/>
                 <li><a>
                   <xsl:attribute name="href">#<xsl:value-of select="../@name"/><xsl:value-of select="@name"/>
                   </xsl:attribute>
                   <xsl:value-of select="@name"/>
                 </a></li>
               </xsl:for-each>
             </ul>
         </xsl:for-each>
       </div>

        <h1>Main (coasim) Module</h1>
        <xsl:for-each select="guile-bindings/method">
	  <xsl:sort select="@name" data-type="text"/>
	  <xsl:apply-templates select="."/>
	</xsl:for-each>

        <xsl:for-each select="guile-bindings/module">
          <xsl:sort select="@name" data-type="text"/>
          <h1><xsl:value-of select="@name"/></h1>
          <xsl:apply-templates select="brief"/>
          <xsl:for-each select="./method">
	    <xsl:sort select="@name" data-type="text"/>

              <!-- FIXME: manual inlining of method template -->
              <!-- put here to get the ref prefix correct -->
              <h2>
               <a>
                 <xsl:attribute name="name"><xsl:value-of select="../@name"/><xsl:value-of select="@name"/>
                 </xsl:attribute>
                 <xsl:value-of select="@name"/>
               </a>
              </h2>
              <xsl:apply-templates select="brief"/>
              <xsl:apply-templates select="prototype"/>
              <xsl:apply-templates select="example"/>
              <xsl:apply-templates select="description"/>

	    </xsl:for-each>
	</xsl:for-each>

      </body>
    </html>
    
  </xsl:template>
  
  <xsl:template match="method">
    <h2>
     <a>
       <xsl:attribute name="name"><xsl:value-of select="@name"/>
       </xsl:attribute>
       <xsl:value-of select="@name"/>
     </a>
    </h2>
    <xsl:apply-templates select="brief"/>
    <xsl:apply-templates select="prototype"/>
    <xsl:apply-templates select="example"/>
    <xsl:apply-templates select="description"/>
  </xsl:template>

  <xsl:template match="brief">
    <p class="brief"><em><xsl:apply-templates/></em></p>
  </xsl:template>

  <xsl:template match="prototype">
    <h4>Prototype</h4>
    <pre class="prototype"><xsl:apply-templates/></pre>
  </xsl:template>

  <xsl:template match="example">
    <h4>Example</h4>
    <pre class="example"><xsl:apply-templates/></pre>
  </xsl:template>

  <xsl:template match="description">
    <h3>Details</h3>
    <div class="description"><xsl:apply-templates/></div>
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
