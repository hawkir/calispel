<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:ng="http://docbook.org/docbook-ng"
                xmlns:db="http://docbook.org/ns/docbook"
                xmlns:exsl="http://exslt.org/common"
                xmlns:exslt="http://exslt.org/common"
                exclude-result-prefixes="db ng exsl exslt"
                version='1.0'>
  <xsl:import href="file:///usr/share/xml/docbook/stylesheet/nwalsh/html/docbook.xsl"/>
  
  <!-- Unfortunately, docbook-xsl emits sloppy HTML, so we can't use
       Strict. -->
  <xsl:output method="html"
              encoding="UTF-8"
              indent="no"
	      doctype-public="-//W3C//DTD HTML 4.01 Transitional//EN"
	      doctype-system="http://www.w3.org/TR/html4/loose.dtd"/>
  
  <xsl:template match="replaceable">
    <xsl:text>&lt;</xsl:text>
    <xsl:call-template name="inline.italicseq"/>
    <xsl:text>&gt;</xsl:text>
  </xsl:template>
  
  <xsl:template match="type">
    <xsl:call-template name="inline.monoseq"/>
  </xsl:template>
</xsl:stylesheet>
