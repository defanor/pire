<?xml version="1.0" encoding="utf-8"?>





<xsl:stylesheet version="2.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		
		xmlns:d="http://docbook.org/ns/docbook"
		>


  <!-- 
  -->


  <xsl:template match="node()|@*">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()" />
    </xsl:copy>
  </xsl:template>
  

  <xsl:template match="d:section/@xml:base" />
  <xsl:template match="d:section/@xml:lang" />
  <xsl:template match="d:section/@version" />




</xsl:stylesheet>
