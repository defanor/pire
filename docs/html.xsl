<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet 
    version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    >


  
  <!-- <xsl:import href="../xhtml/docbook.xsl" /> -->

  <!-- evt xhtml-1_1 -->
  <xsl:import href="/usr/share/xml/docbook/stylesheet/docbook-xsl-ns/xhtml/docbook.xsl" />


    

  <!-- keine Wirkung ? -->
  <xsl:param name="chapter.autolabel" select="1" />
  

  <!-- <xsl:output encoding="utf-8" -->
  <!-- 	      indent="yes" -->
  <!--     /> -->

  
  <xsl:param name="html.stylesheet" select="'my.css'"/>



  <!-- xsl:param name="draft.watermark.image">/usr/share/xml/docbook/stylesheet/docbook-xsl-ns/images/draft.png</xsl:param -->




  <!-- <xsl:param name="body.start.indent">0pt</xsl:param> -->
  <!-- <xsl:param name="paper.type">A4</xsl:param> -->


  <!-- prgramlisting -->

  
  <!-- default 2 
       but show also deeper section titles in the toc
  -->
  <xsl:param name="toc.section.depth">3</xsl:param>




  
  
</xsl:stylesheet>
