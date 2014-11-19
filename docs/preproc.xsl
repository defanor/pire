<?xml version="1.0" encoding="utf-8"?>


<!-- 
exclude-result-prefixes="d"
-->


<xsl:stylesheet version="2.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		
		
		xmlns:d="http://docbook.org/ns/docbook"

>


<!-- http://www.ibiblio.org/xml/books/bible2/chapters/ch17.html -->

<!-- The default rule for elements
     d: von mir
-->
<!-- <xsl:template match="d:*|/"> -->
<!--   <xsl:apply-templates/> -->
<!-- </xsl:template> -->


<!-- The default rule for text nodes and attributes
     ...
-->


<!-- default element rule is identity transform -->
<xsl:template match="d:*">
  <xsl:copy>
    <xsl:copy-of select="@*"/>
    <!-- <xsl:copy-of select="@*|comment()"/> -->
    <xsl:apply-templates/>
  </xsl:copy>
</xsl:template>


<!-- 
xslt2 functions
http://www.dpawson.co.uk/xsl/rev2/functions2.html#d15232e292  
Insert a character every nth character

gpSz is the group size
ich mußte aber noch anpassen: idiv $gpSz
-->


<xsl:variable name="gpSz" select="1"/>

<!-- habe hier einen 200b char verwendet - s wrapping
     urspr mit ucs-insert, weil er aber quasi nicht sichtbar war
     dann explizit
-->
<xsl:template match="d:programlisting[@role='mywrap']/text()">
  <!-- <xsl:value-of select="."/> -->
  <xsl:value-of select="string-join(
			for $i in 0 to (string-length(.) idiv $gpSz)
                        return substring(., $i*$gpSz + 1, $gpSz), '&#x200b;')"/>


</xsl:template>




</xsl:stylesheet>




