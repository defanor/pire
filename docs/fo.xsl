<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet 
    version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    
    xmlns:xi="http://www.w3.org/2001/XInclude"
    
    >

  <!-- need the ns stylesheets here, for we use docbook5 -->
  <!-- install the debian docbook-xsl-ns pkg, then the paths should be fine -->

  <xsl:import href="/usr/share/xml/docbook/stylesheet/docbook-xsl-ns/fo/docbook.xsl" />

  
  <xsl:attribute-set name="section.title.level1.properties">
    <xsl:attribute name="font-size">20.736pt</xsl:attribute>
  </xsl:attribute-set>
  <xsl:attribute-set name="section.title.level2.properties">
    <xsl:attribute name="font-size">17.28pt</xsl:attribute>
  </xsl:attribute-set>
  <xsl:attribute-set name="section.title.level2.properties">
    <xsl:attribute name="font-size">14.4pt</xsl:attribute>
  </xsl:attribute-set>


  <!-- default 2 
       but show also deeper section titles in the toc
  -->
  <xsl:param name="toc.section.depth">3</xsl:param>


  
  <!-- jan2010 wohl für article nicht relevant? - nochmals ansehen -->
  <!-- xsl:include href="mytitlepages.xsl" / -->


  
  <xsl:output encoding="utf-8"
	      indent="yes"
      />

  
  <!-- 
       I used to need that, otherwise I'd get
       ...Background image not available: http://docbook.sourceforge.net/release/images/draft.png
       cf.
       
       "you don't need a catalog"
       http://www.mail-archive.com/docbook-apps@lists.oasis-open.org/msg10955.html
       http://docbook.sourceforge.net/release/xsl/current/doc/fo/draft.watermark.image.html
       
       but seems unnecessary now
       anyway, the img is in the -ns pkg
  -->
  
  <!-- <xsl:param name="draft.watermark.image">/usr/share/xml/docbook/stylesheet/docbook-xsl-ns/images/draft.png</xsl:param> -->




  <xsl:param name="body.start.indent">0pt</xsl:param>
  <xsl:param name="paper.type">A4</xsl:param>

  <!-- default: 1in/1in
       
       outer maybe smaller than als inner 
       1cm/2cm
       2cm/3cm
       1in/1.5in
       0.5in/0.75in
       0.66in/1in
  -->
  <!-- <xsl:param name="page.margin.outer">0.66in</xsl:param> -->
  <!-- <xsl:param name="page.margin.inner">1in</xsl:param> -->
  <xsl:param name="page.margin.outer">0.66in</xsl:param>
  <xsl:param name="page.margin.inner">0.66in</xsl:param>



  <!--
      otherwise page numbers are not readable on my printouts

       default 0.5in (?)
       - cf .../fo/param.xsl
       
       0.6 maybe enough
  -->
  <xsl:param name="page.margin.bottom">0.66in</xsl:param>

  <!-- 11 -->
  <xsl:param name="body.font.master">11</xsl:param>


  <!-- seems not to work -->
  <!-- xsl:param name="footnote.font.size" select="'9'"/ -->

  <xsl:param name="body.font.family">Palatino</xsl:param>


  <xsl:attribute-set name="root.properties">
    <xsl:attribute name="text-align">left</xsl:attribute>
  </xsl:attribute-set>
  <xsl:attribute-set name="footnote.properties">
    <xsl:attribute name="text-align">left</xsl:attribute>
  </xsl:attribute-set>
  




  <!-- programlisting etc -->
  <xsl:attribute-set name="monospace.verbatim.properties">
    <xsl:attribute name="font-family">LucidaTypewriter</xsl:attribute>
    <!-- 10 -->
    <xsl:attribute name="font-size">9pt</xsl:attribute>


    <!-- wrapping -->
    <!-- seems unsupported
	 ie. no special wrapping char at the line of a line (?)

    -->
    <!-- xsl:param name="hyphenate.verbatim" select="1" / -->
    <xsl:attribute name="wrap-option">wrap</xsl:attribute>
    <xsl:attribute name="keep-together.within-column">always</xsl:attribute>

    <!-- 21a9 -->
    <!-- xsl:attribute name="hyphenation-character">↩</xsl:attribute -->
    <!-- xsl:attribute name="hyphenation-character">*</xsl:attribute -->

  </xsl:attribute-set>





  <xsl:param name="shade.verbatim" select="1" />

  <xsl:attribute-set name="shade.verbatim.style">

    <xsl:attribute name="background-color">#f0f0f0</xsl:attribute>
    <xsl:attribute name="padding">3pt</xsl:attribute>

    <xsl:attribute name="padding-right">0pt</xsl:attribute>
    <xsl:attribute name="margin-left">11pt</xsl:attribute>
  </xsl:attribute-set>


  
  <xsl:include href="./header" />

  <xsl:include href="./footer" />

  <xsl:include href="./fo-bullets" />


  <!-- syntax highlighting -->
  <xsl:param name="highlight.source" select="1"/>

  
</xsl:stylesheet>
