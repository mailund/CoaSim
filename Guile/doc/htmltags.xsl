<?xml version="1.0" encoding="ISO-8859-1" ?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0">

  <!-- XSL Stylesheet med standard HTML tags -->

  <xsl:template match="h1">
    <h1><xsl:apply-templates/></h1>
  </xsl:template>

  <xsl:template match="h2">
    <h2><xsl:apply-templates/></h2>
  </xsl:template>

  <xsl:template match="h3">
    <h3><xsl:apply-templates/></h3>
  </xsl:template>

  <xsl:template match="h4">
    <h4><xsl:apply-templates/></h4>
  </xsl:template>

  <xsl:template match="em">
    <em><xsl:apply-templates/></em>
  </xsl:template>
  
  <xsl:template match="b">
    <b><xsl:apply-templates/></b>
  </xsl:template>

  <xsl:template match="i">
    <i><xsl:apply-templates/></i>
  </xsl:template>

  <xsl:template match="center">
    <center> <xsl:apply-templates/> </center>
  </xsl:template>
  
  <xsl:template match="strong">
    <strong> <xsl:apply-templates/> </strong>
  </xsl:template>
  
  <xsl:template match="p">
    <p> <xsl:apply-templates/> </p>
  </xsl:template>
  
  <xsl:template match="blockquote">
    <blockquote> <xsl:apply-templates/> </blockquote>
  </xsl:template>
  
  <xsl:template match="br">
    <br> <xsl:apply-templates/> </br>
  </xsl:template>
  
  <xsl:template match="img">
    <img><xsl:attribute name="src"><xsl:value-of select="@src"/></xsl:attribute>
    </img>
  </xsl:template>
  
  <xsl:template match="a">
    <a><xsl:attribute name="href"><xsl:value-of select="@href"/></xsl:attribute>
      <xsl:apply-templates/>
    </a>
  </xsl:template>
  
  <xsl:template match="applet">
    <applet><xsl:attribute name="code"><xsl:value-of select="@code"/></xsl:attribute><xsl:attribute name="archive"><xsl:value-of select="@archive"/></xsl:attribute><xsl:attribute name="width"><xsl:value-of select="@width"/></xsl:attribute><xsl:attribute name="height"><xsl:value-of select="@height"/></xsl:attribute>
      <xsl:apply-templates/>
    </applet>
  </xsl:template>
  
  <xsl:template match="table">
    <table><xsl:attribute name="border"><xsl:value-of select="@border"/></xsl:attribute><xsl:attribute name="cellspacing"><xsl:value-of select="@cellspacing"/></xsl:attribute><xsl:attribute name="cellpadding"><xsl:value-of select="@cellpadding"/></xsl:attribute>
      <xsl:apply-templates/>
    </table>
  </xsl:template>
  
  <xsl:template match="tr">
    <tr> <xsl:apply-templates/> </tr>
  </xsl:template>
  <xsl:template match="th">
    <th> <xsl:apply-templates/> </th>
  </xsl:template>
  <xsl:template match="td">
    <td> <xsl:apply-templates/> </td>
  </xsl:template>
  
  
  <xsl:template match="dl">
    <dl> <xsl:apply-templates/> </dl>
  </xsl:template>

  <xsl:template match="dt">
    <dt> <xsl:apply-templates/> </dt>
  </xsl:template>

  <xsl:template match="dd">
    <dd> <xsl:apply-templates/> </dd>
  </xsl:template>

  <xsl:template match="ul">
    <ul> <xsl:apply-templates/> </ul>
  </xsl:template>
  
  <xsl:template match="ol">
    <ol> <xsl:apply-templates/> </ol>
  </xsl:template>
  
  <xsl:template match="li">
    <li> <xsl:apply-templates/> </li>
  </xsl:template>
  
  <xsl:template match="pre">
    <pre> <xsl:apply-templates/> </pre>
  </xsl:template>
  
  <xsl:template match="tt">
    <tt> <xsl:apply-templates/> </tt>
  </xsl:template>
  
  <xsl:template match="sub">
    <sub> <xsl:apply-templates/> </sub>
  </xsl:template>
  
  <xsl:template match="sup">
    <sup> <xsl:apply-templates/> </sup>
  </xsl:template>
  
  <xsl:template match="code">
    <code> <xsl:apply-templates/> </code>
  </xsl:template>
  
</xsl:stylesheet>
<!-- keep this comment at the end of the file
local variables:
mode: xml
sgml-omittag:nil
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
end:
-->
