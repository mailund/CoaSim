<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text"/>
<xsl:template match="/">
  <xsl:text> digraph G { 
</xsl:text>
<xsl:text>  size="8.5,8.5"; 
</xsl:text>
<xsl:text>  orientation=landscape;  
</xsl:text>
<xsl:text>   compound=true; 
</xsl:text>
 
<xsl:apply-templates select="//child"/>
<xsl:apply-templates select="coasim/genconversion|coasim/recombination|coasim/coalescent|coasim/interval_node"/>
<xsl:text>  { rank = same; 
</xsl:text>
<xsl:apply-templates select="coasim/leaf"/>
<xsl:text>  }; 
</xsl:text>

<xsl:apply-templates select="//mutation" mode="labels"/>
  <xsl:text> } 
</xsl:text>
</xsl:template>

<xsl:template match="haplotype">
<xsl:apply-templates select="loci"/>
</xsl:template>

<xsl:template match="loci">
  <xsl:if test=".='-1'">
    <xsl:text>?</xsl:text>   
  </xsl:if>
  <xsl:if test=".!='-1'">
    <xsl:value-of select="."/>   
  </xsl:if>
</xsl:template>

<xsl:template match="interval">
    <xsl:variable name="start" select="@start"/>
    <xsl:variable name="end" select="@end"/>
<xsl:text>(</xsl:text>
    <xsl:apply-templates select="//marker[$start&lt;position and position&lt;=$end]"/> 
<xsl:text>) </xsl:text>  
  <xsl:value-of select="@start"/> 
  <xsl:text> -> </xsl:text>
  <xsl:value-of select="@end"/>
  <xsl:text> \n </xsl:text>
</xsl:template>


<xsl:template match="interval_node">
  <xsl:value-of select="@id"/> 
  <xsl:text> [shape=polygon,sides=5,label="</xsl:text>
  <xsl:apply-templates select="interval"/>   <xsl:text> "]
</xsl:text>
</xsl:template>


<xsl:template match="genconversion">
  <xsl:value-of select="@id"/> 
  <xsl:text> [label="G( </xsl:text>
    <xsl:if test="@is_inside='1'">
      <xsl:variable name="start" select="@conversion_start"/>
      <xsl:variable name="end" select="@conversion_end"/>
      <xsl:text>...|</xsl:text>
      <xsl:apply-templates select="//marker[position&lt;$end and $start&lt;position]"/>      
      <xsl:text>|...</xsl:text>
    </xsl:if>
    <xsl:if test="@is_inside!='1'">
      <xsl:variable name="start" select="@conversion_start"/>
      <xsl:variable name="end" select="@conversion_end"/>
      <xsl:apply-templates select="//marker[position&lt;$start]"/>         
      <xsl:text>|...|</xsl:text>
      <xsl:apply-templates select="//marker[$end&lt;position]"/>         
    </xsl:if>
    <xsl:text>) time = </xsl:text><xsl:value-of select="@time"/><xsl:text>, start = </xsl:text> 
    <xsl:value-of select="@conversion_start"/> <xsl:text> stop = </xsl:text> 
    <xsl:value-of select="@conversion_end"/> <xsl:text>\n</xsl:text>
    <xsl:apply-templates select="id(@haplotype)"/>   <xsl:text> ", color=yellow, style=filled]
  </xsl:text>
</xsl:template>

<xsl:template match="marker">
  <xsl:number expr="position()"/>
</xsl:template>



<xsl:template match="recombination">
  <xsl:value-of select="@id"/> 
  <xsl:text> [label="R(</xsl:text>
  <xsl:if test="@is_left='1'">
    <xsl:variable name="cross_over" select="@crossover"/>
    <xsl:apply-templates select="//marker[position&lt;$cross_over]"/>   
    <xsl:text>|...</xsl:text>
  </xsl:if>
  <xsl:if test="@is_left!='1'"> 
      <xsl:text>...|</xsl:text>
   <xsl:variable name="cross_over" select="@crossover"/>
    <xsl:apply-templates select="//marker[$cross_over&lt;position]"/>   
  </xsl:if>
  <xsl:text>)  time = </xsl:text><xsl:value-of select="@time"/> <xsl:text>,  crossover = </xsl:text>
  <xsl:value-of select="@crossover"/> <xsl:text>\n</xsl:text>
  <xsl:apply-templates select="id(@haplotype)"/>   <xsl:text> ", color=green, style=filled]
</xsl:text>
</xsl:template>

<xsl:template match="coalescent">
  <xsl:value-of select="@id"/> 
  <xsl:text> [label="C  time = </xsl:text><xsl:value-of select="@time"/><xsl:text>\n</xsl:text>
  <xsl:apply-templates select="id(@haplotype)"/>   <xsl:text> ", color=red, style=filled]
</xsl:text>
</xsl:template>

<xsl:template match="leaf">
  <xsl:value-of select="@id"/> 
  <xsl:text> [shape=polygon,sides=4,label="Leaf  time = 0.0 \n</xsl:text>
  <xsl:apply-templates select="id(@haplotype)"/>   <xsl:text> ", color=light_blue, style=filled]
</xsl:text>
</xsl:template>

<xsl:template match="mutation" mode="labels">
  <xsl:value-of select="@parent_ref"/> 
  <xsl:text>_</xsl:text>
  <xsl:apply-templates select="id(@marker_ref)"/> 
  <xsl:text>_</xsl:text>
  <xsl:value-of select="@child_ref"/> 
  <xsl:text> [label="M (</xsl:text>
  <xsl:apply-templates select="id(@marker_ref)"/>   
  <xsl:text>)" color=gray, style=filled];
</xsl:text>
</xsl:template>

<xsl:template match="mutation" mode="connections">
  <xsl:value-of select="@parent_ref"/> 
  <xsl:text>_</xsl:text>
  <xsl:apply-templates select="id(@marker_ref)"/> 
  <xsl:text>_</xsl:text>
  <xsl:value-of select="@child_ref"/> 
  <xsl:text> -> </xsl:text>
</xsl:template>

<xsl:template match="child">
  <xsl:value-of select="../@id"/> 
  <xsl:text> -> </xsl:text>
  <xsl:variable name="parent" select="../@id"/>
  <xsl:variable name="child" select="@ref"/>
  <xsl:apply-templates select="//mutation[$parent=@parent_ref and $child=@child_ref]" mode="connections"/>
  <xsl:value-of select="@ref"/>
  <xsl:text>;
</xsl:text>
</xsl:template>

</xsl:stylesheet>
