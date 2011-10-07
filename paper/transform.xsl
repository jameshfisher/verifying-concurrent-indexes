<xsl:stylesheet
   version="1.0"
   xmlns="http://www.w3.org/1999/xhtml"
   xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
   xmlns:xi="http://www.w3.org/2001/XInclude"
   xmlns:e="http://eegg.github.com/htmlx"	 
   xmlns:m="http://eegg.github.com/macro">

  <xsl:strip-space elements="e:exists e:fst e:snd e:pred"/>

  <xsl:template match="e:displaycode">
    <pre class="prettyprint"><xsl:apply-templates /></pre>
  </xsl:template>

  <xsl:template match="e:indent">
    <div class="indent"><xsl:apply-templates /></div>
  </xsl:template>  

  <xsl:template match="e:note|e:cite">
    <span class="note">
      <xsl:apply-templates select="@*|node()"/>
    </span>
  </xsl:template>

  <xsl:template match="e:descriptionlist">
    <ul>
      <xsl:apply-templates select="e:li" />
    </ul>
  </xsl:template>

  <xsl:template match="e:math"><span class="math"><xsl:apply-templates /></span></xsl:template>

  <xsl:template match="e:constant"><span class="constant"><xsl:apply-templates /></span></xsl:template>
  <xsl:template match="e:const"><span class="constant"><xsl:value-of select="@n" /></span></xsl:template>
  <xsl:template match="e:var"><span class="var"><xsl:value-of select="@n" /></span></xsl:template>
  <xsl:template match="e:st"><span class="st"><xsl:value-of select="@n" /></span></xsl:template> <!-- a set -->

  <xsl:template name="op">
    <xsl:param name="sym"/>
    <xsl:variable name="l" select="@type"/>
    <xsl:variable name="p" select="@parens"/>
    <xsl:if test="$p='yes'">(</xsl:if><xsl:for-each select="*">
      <xsl:apply-templates select="." />
      <xsl:if test="following-sibling::*">&#160;<xsl:value-of select="$sym"/>&#160;<xsl:if test="$l='lines'"><br/></xsl:if></xsl:if>
    </xsl:for-each><xsl:if test="$p='yes'">)</xsl:if>
  </xsl:template>

  <xsl:template name="opNoSpaces">
    <xsl:param name="sym"/>
    <xsl:variable name="l" select="@type"/>
    <xsl:variable name="p" select="@parens"/>
    <xsl:if test="$p='yes'">(</xsl:if><xsl:for-each select="*">
      <xsl:apply-templates select="." />
      <xsl:if test="following-sibling::*"><xsl:value-of select="$sym"/><xsl:if test="$l='lines'"><br/></xsl:if></xsl:if>
    </xsl:for-each><xsl:if test="$p='yes'">)</xsl:if>
  </xsl:template>

  <xsl:template match="e:fpfun"><xsl:apply-templates select="e:fst" />&#160;⇀<sub>fin</sub>&#160;<xsl:apply-templates select="e:snd" /></xsl:template>

  <xsl:template match="e:fupd"><xsl:apply-templates select="e:fst" />&#160;[&#160;<xsl:apply-templates select="e:snd" />&#160;↦&#160;<xsl:apply-templates select="e:thd" />]</xsl:template>

  <xsl:template match="e:size">|<xsl:apply-templates />|</xsl:template>

  <xsl:template match="e:define"><xsl:call-template name="op"><xsl:with-param name="sym">≝</xsl:with-param></xsl:call-template></xsl:template>

  <xsl:template match="e:plus"><xsl:call-template name="opNoSpaces"><xsl:with-param name="sym">+</xsl:with-param></xsl:call-template></xsl:template>
  <xsl:template match="e:minus"><xsl:call-template name="opNoSpaces"><xsl:with-param name="sym">&#x2212;</xsl:with-param></xsl:call-template></xsl:template>

  <xsl:template match="e:setminus"><xsl:call-template name="opNoSpaces"><xsl:with-param name="sym">∖</xsl:with-param></xsl:call-template></xsl:template>
  <xsl:template match="e:notin"><xsl:call-template name="op"><xsl:with-param name="sym">∉</xsl:with-param></xsl:call-template></xsl:template>
  <xsl:template match="e:in"><xsl:call-template name="op"><xsl:with-param name="sym">∈</xsl:with-param></xsl:call-template></xsl:template>
  <xsl:template match="e:subset"><xsl:call-template name="op"><xsl:with-param name="sym">⊆</xsl:with-param></xsl:call-template></xsl:template>
  <xsl:template match="e:notsubset"><xsl:call-template name="op"><xsl:with-param name="sym">⊈</xsl:with-param></xsl:call-template></xsl:template>
  <xsl:template match="e:union"><xsl:call-template name="op"><xsl:with-param name="sym">∪</xsl:with-param></xsl:call-template></xsl:template>
  <xsl:template match="e:intersection"><xsl:call-template name="op"><xsl:with-param name="sym">∩</xsl:with-param></xsl:call-template></xsl:template>

  <xsl:template match="e:sep"><xsl:call-template name="op"><xsl:with-param name="sym">∗</xsl:with-param></xsl:call-template></xsl:template>
  <xsl:template match="e:fcell"><xsl:call-template name="op"><xsl:with-param name="sym">↦</xsl:with-param></xsl:call-template></xsl:template>


  <xsl:template match="e:and"><xsl:call-template name="op"><xsl:with-param name="sym">∧</xsl:with-param></xsl:call-template></xsl:template>
  <xsl:template match="e:or"><xsl:call-template name="op"><xsl:with-param name="sym">∨</xsl:with-param></xsl:call-template></xsl:template>
  <xsl:template match="e:eq"><xsl:call-template name="op"><xsl:with-param name="sym">=</xsl:with-param></xsl:call-template></xsl:template>
  <xsl:template match="e:noteq"><xsl:call-template name="op"><xsl:with-param name="sym">≠</xsl:with-param></xsl:call-template></xsl:template>
  <xsl:template match="e:lt"><xsl:call-template name="op"><xsl:with-param name="sym">&lt;</xsl:with-param></xsl:call-template></xsl:template>
  <xsl:template match="e:leq"><xsl:call-template name="op"><xsl:with-param name="sym">≤</xsl:with-param></xsl:call-template></xsl:template>
  <xsl:template match="e:gt"><xsl:call-template name="op"><xsl:with-param name="sym">&gt;</xsl:with-param></xsl:call-template></xsl:template>
  <xsl:template match="e:geq"><xsl:call-template name="op"><xsl:with-param name="sym">≥</xsl:with-param></xsl:call-template></xsl:template>
  <xsl:template match="e:impl"><xsl:call-template name="op"><xsl:with-param name="sym">→</xsl:with-param></xsl:call-template></xsl:template>
  <xsl:template match="e:doubleimpl"><xsl:call-template name="op"><xsl:with-param name="sym">↔</xsl:with-param></xsl:call-template></xsl:template>

  <xsl:template match="e:forall">∀<xsl:apply-templates select="e:vars" />.&#160;<xsl:apply-templates select="e:expr" /></xsl:template>
  <xsl:template match="e:exists">∃<xsl:apply-templates select="e:vars" />.&#160;<xsl:apply-templates select="e:expr" /></xsl:template>
  <xsl:template match="e:vars"><xsl:call-template name="opNoSpaces"><xsl:with-param name="sym">, </xsl:with-param></xsl:call-template></xsl:template>

  <xsl:template match="e:logimpl"><xsl:call-template name="op"><xsl:with-param name="sym">⇒</xsl:with-param></xsl:call-template></xsl:template>

  <xsl:template match="e:fst"><xsl:apply-templates select="@*|node()" /></xsl:template>
  <xsl:template match="e:snd"><xsl:apply-templates select="@*|node()" /></xsl:template>

  <xsl:template match="e:keysof"><span class="keysof">Keys</span>(<xsl:apply-templates />)</xsl:template>
  <xsl:template match="e:tuple">(<xsl:apply-templates />)</xsl:template>

  <xsl:template match="e:bigo">O(<xsl:apply-templates />)</xsl:template>
  <xsl:template match="e:bigomega">Ω(<xsl:apply-templates />)</xsl:template>
  <xsl:template match="e:bigtheta">Θ(<xsl:apply-templates />)</xsl:template>

  <xsl:template match="e:triple">
    <span class="triple">
      <xsl:apply-templates select="e:pre" />
      <xsl:apply-templates select="e:command" />
      <xsl:apply-templates select="e:post" />
    </span>
  </xsl:template>

  <xsl:template match="e:pre|e:post|e:cond"><div class="cond"><xsl:apply-templates /></div></xsl:template>
  <xsl:template match="e:set">{<xsl:apply-templates select="@*|node()" />}</xsl:template>
  <xsl:template match="e:setb"><span class="set math"><xsl:apply-templates select="e:fst" />&#160;:&#160;<xsl:apply-templates select="e:snd" /></span></xsl:template>
  <xsl:template match="e:command"><pre class="prettyprint"><xsl:apply-templates /></pre></xsl:template>

  <xsl:template match="e:li">
    <li>
      <xsl:apply-templates select="e:item" />
      <xsl:apply-templates select="e:description" />
    </li>
  </xsl:template>

  <xsl:template match="e:item">
    <strong class="item"><xsl:apply-templates /></strong>
  </xsl:template>

  <xsl:template match="e:description">
    <xsl:apply-templates />
  </xsl:template>

  <xsl:template match="e:pred"><a class="pred_name"><xsl:attribute name="href">#def_<xsl:value-of select="@name"/></xsl:attribute><xsl:value-of select="@name" /></a>(<xsl:call-template name="opNoSpaces"><xsl:with-param name="sym">, </xsl:with-param></xsl:call-template>)</xsl:template>
  <xsl:template match="e:func"><span class="func_name"><xsl:value-of select="@n" /></span>(<xsl:call-template name="opNoSpaces"><xsl:with-param name="sym">, </xsl:with-param></xsl:call-template>)</xsl:template>
  <xsl:template match="e:predicate"><span class="pred_name"><xsl:apply-templates /></span></xsl:template>

  <!-- This should only emphasize on first use -->
  <xsl:template match="e:jargon"><em><xsl:apply-templates /></em></xsl:template>
  
  <xsl:template match="e:derivation">
    <table class="derivation displaymath">
      <xsl:apply-templates select="@*|node()"/>
    </table>
  </xsl:template>
  <xsl:template match="e:step">
    <tr>
      <td><span class="step"><xsl:number /></span>.</td>
      <td>
	<xsl:attribute name="class"><xsl:value-of select="@indent" /></xsl:attribute>
	<xsl:apply-templates select="e:derive"/>
      </td>
      <xsl:apply-templates select="e:by"/>
    </tr>
  </xsl:template>
  <xsl:template match="e:derive"><xsl:apply-templates select="@*|node()" /></xsl:template>
  <xsl:template match="e:by"><td><xsl:apply-templates select="@*|node()" /></td></xsl:template>

  <xsl:template match="e:from[@name]"><!-- http://stackoverflow.com/questions/7351574/xslt-find-number-of-given-node -->
    <span class="step">
      <xsl:variable name="vReferred" select="ancestor::e:derivation[1]/e:step[@name = current()/@name]"/>
      <xsl:for-each select="$vReferred"><xsl:number count="e:step" /></xsl:for-each>
    </span>
  </xsl:template>

  <xsl:template match="e:listing">
    <figure class="listing">
      <figcaption>
        <span class="figure_number">Listing <xsl:number count="e:listing" level="multiple"/>.</span><br />
	<xsl:apply-templates select="e:caption"/>
      </figcaption>
      <xsl:apply-templates select="e:contents"/>
    </figure>
  </xsl:template>

 <xsl:template match="e:diagram">
    <figure class="diagram">
      <figcaption>
        <span class="figure_number">Diagram <xsl:number count="e:diagram" level="any"/>.</span><br />
	<xsl:apply-templates select="e:caption"/>
      </figcaption>
      <xsl:apply-templates select="e:contents"/>
    </figure>
  </xsl:template>

 <xsl:template match="e:definition">
    <figure class="definition">
      <xsl:attribute name="id">def_<xsl:value-of select="@name"/></xsl:attribute>
      <figcaption><span class="figure_number">Definition <xsl:number count="e:definition" level="any"/>.</span></figcaption>
      <div><xsl:apply-templates /></div>
    </figure>
  </xsl:template>

 <xsl:template match="e:lemma">
    <figure class="lemma">
      <figcaption>
        <span class="figure_number">Lemma <xsl:number count="e:lemma" level="any"/>.</span><br />
	<xsl:apply-templates select="e:caption"/>
      </figcaption>
      <xsl:apply-templates select="e:contents"/>
    </figure>
  </xsl:template>

  <xsl:template match="e:specification">
    <figure class="specification">
      <figcaption>
        <span class="figure_number">Specification <xsl:number count="e:specification" level="any"/>.</span><br />
	<xsl:apply-templates select="e:caption"/>
      </figcaption>
      <div><xsl:apply-templates select="e:contents"/></div>
    </figure>
  </xsl:template>

  <xsl:template match="e:annotation">
    <figure class="annotation">
      <figcaption>
        <span class="figure_number">Annotation <xsl:number count="e:annotation" level="any"/>.</span><br />
	<xsl:apply-templates select="e:caption"/>
      </figcaption>
      <div><xsl:apply-templates select="e:contents"/></div>
    </figure>
  </xsl:template>

  <xsl:template match="e:h">
    <xsl:variable name="depth" select="count(ancestor::*[name()='section'])"/>
    <xsl:choose>
      <xsl:when test="$depth=1">
	<h1><xsl:apply-templates/></h1>
      </xsl:when>
      <xsl:when test="$depth=2">
	<h2><xsl:apply-templates/></h2>
      </xsl:when>
      <xsl:when test="$depth=3">
	<h3><xsl:apply-templates/></h3>
      </xsl:when>
      <xsl:when test="$depth=4">
	<h4><xsl:apply-templates/></h4>
      </xsl:when>
      <xsl:when test="$depth=5">
	<h5><xsl:apply-templates/></h5>
      </xsl:when>
      <xsl:otherwise>
	<h6><xsl:apply-templates/></h6>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <!-- passthrough -->
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>
