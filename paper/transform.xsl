<xsl:stylesheet
   version="1.0"
   xmlns="http://www.w3.org/1999/xhtml"
   xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
   xmlns:xi="http://www.w3.org/2001/XInclude"
   xmlns:e="http://eegg.github.com/htmlx"	 
   xmlns:m="http://eegg.github.com/macro">

  <xsl:strip-space elements="e:exists"/> <!-- seems to do fuckall -->

  <xsl:template match="e:displaycode">
    <pre class="prettyprint"><xsl:apply-templates /></pre>
  </xsl:template>

  <xsl:template match="e:indent">
    <div class="indent"><xsl:apply-templates /></div>
  </xsl:template>  

  <!-- Macros -->
  <xsl:template match="m:api"><abbr title="Application Programming Interface" class="smallcaps">API</abbr></xsl:template>
  <xsl:template match="m:url"><abbr title="Uniform Resource Locator" class="smallcaps">URL</abbr></xsl:template>
  <xsl:template match="m:adt"><abbr title="Abstract Data Type" class="smallcaps">ADT</abbr></xsl:template>
  <xsl:template match="m:ll"><abbr title="Linked List" class="smallcaps">LL</abbr></xsl:template>
  <xsl:template match="m:bst"><abbr title="Binary Search Tree" class="smallcaps">BST</abbr></xsl:template>
  <xsl:template match="m:rbt"><abbr title="Red-Black Tree" class="smallcaps">RBT</abbr></xsl:template>

  <xsl:template match="m:eg"><i>e.g.</i></xsl:template>
  <xsl:template match="m:ie"><i>i.e.</i></xsl:template>
  <xsl:template match="m:vs"><i>vs.</i></xsl:template>

  <xsl:template match="m:empty">∅</xsl:template>

  <xsl:template match="m:orElim">∨E</xsl:template>
  <xsl:template match="m:orIntro">∨I</xsl:template>
  <xsl:template match="m:andElim">∧E</xsl:template>
  <xsl:template match="m:andIntro">∧I</xsl:template>
  <xsl:template match="m:impliesElim">→E</xsl:template>
  <xsl:template match="m:impliesIntro">→I</xsl:template>
  <xsl:template match="m:doubleImplElim">↔E</xsl:template>
  <xsl:template match="m:doubleImplIntro">↔I</xsl:template>
  <xsl:template match="m:existsIntro">∃I</xsl:template>
  <xsl:template match="m:existsElim">∃E</xsl:template>

  <xsl:template match="m:lacuna">&#160;[…]&#160;</xsl:template>

  <xsl:template match="m:hemp"><b>emp</b></xsl:template>
  <xsl:template match="m:scemp">∅</xsl:template>

  <xsl:template match="m:red"><span class="red">●</span></xsl:template>
  <xsl:template match="m:black"><span class="black">●</span></xsl:template>
  
  <xsl:template match="e:note|e:cite">
    <span class="note">
      <xsl:apply-templates select="@*|node()"/>
    </span>
  </xsl:template>

  <xsl:template match="e:descriptionlist">
    <ul>
      <xsl:apply-templates match="e:li" />
    </ul>
  </xsl:template>

  <xsl:template match="e:math"><span class="math"><xsl:apply-templates /></span></xsl:template>

  <xsl:template match="e:constant"><span class="constant"><xsl:apply-templates /></span></xsl:template>
  <xsl:template match="e:const"><span class="constant"><xsl:value-of select="@n" /></span></xsl:template>
  <xsl:template match="e:var"><span class="var"><xsl:value-of select="@n" /></span></xsl:template>
  <xsl:template match="e:st"><span class="st"><xsl:value-of select="@n" /></span></xsl:template> <!-- a set -->

  <xsl:template match="e:fpfun"><xsl:apply-templates select="e:fst" />&#160;⇀<sub>fin</sub>&#160;<xsl:apply-templates select="e:snd" /></xsl:template>

  <xsl:template match="e:fupd"><xsl:apply-templates select="e:fst" />&#160;[&#160;<xsl:apply-templates select="e:snd" />&#160;↦&#160;<xsl:apply-templates select="e:thd" />]</xsl:template>

  <xsl:template match="e:size">|<xsl:apply-templates />|</xsl:template>

  <xsl:template match="e:define"><xsl:apply-templates select="e:fst" />&#160;≝&#160;<xsl:apply-templates select="e:snd" /></xsl:template>

  <xsl:template match="e:plus"><xsl:apply-templates select="e:fst" />+<xsl:apply-templates select="e:snd" /></xsl:template>
  <xsl:template match="e:minus"><xsl:apply-templates select="e:fst" />&#x2212;<xsl:apply-templates select="e:snd" /></xsl:template>

  <xsl:template match="e:setminus"><xsl:apply-templates select="e:fst" />∖<xsl:apply-templates select="e:snd" /></xsl:template>
  <xsl:template match="e:notin"><xsl:apply-templates select="e:fst" />&#160;∉&#160;<xsl:apply-templates select="e:snd" /></xsl:template>
  <xsl:template match="e:in"><xsl:apply-templates select="e:fst" />&#160;∈&#160;<xsl:apply-templates select="e:snd" /></xsl:template>
  <xsl:template match="e:subset"><xsl:apply-templates select="e:fst" />&#160;⊆&#160;<xsl:apply-templates select="e:snd" /></xsl:template>
  <xsl:template match="e:sep"><xsl:apply-templates select="e:fst" />&#160;∗&#160;<xsl:apply-templates select="e:snd" /></xsl:template>
  <xsl:template match="e:fcell"><xsl:apply-templates select="e:fst" />&#160;↦&#160;<xsl:apply-templates select="e:snd" /></xsl:template>
  <xsl:template match="e:union"><xsl:apply-templates select="e:fst" />&#160;∪&#160;<xsl:apply-templates select="e:snd" /></xsl:template>
  <xsl:template match="e:intersection"><xsl:apply-templates select="e:fst" />&#160;∩&#160;<xsl:apply-templates select="e:snd" /></xsl:template>

  <xsl:template match="e:and"><xsl:apply-templates select="e:fst" />&#160;∧&#160;<xsl:apply-templates select="e:snd" /></xsl:template>
  <xsl:template match="e:or"><xsl:apply-templates select="e:fst" />&#160;∨&#160;<xsl:apply-templates select="e:snd" /></xsl:template>

  <xsl:template match="e:eq"><xsl:apply-templates select="e:fst" />&#160;=&#160;<xsl:apply-templates select="e:snd" /></xsl:template>
  <xsl:template match="e:noteq"><xsl:apply-templates select="e:fst" />&#160;≠&#160;<xsl:apply-templates select="e:snd" /></xsl:template>
  <xsl:template match="e:lt"><xsl:apply-templates select="e:fst" />&#160;&lt;&#160;<xsl:apply-templates select="e:snd" /></xsl:template>
  <xsl:template match="e:leq"><xsl:apply-templates select="e:fst" />&#160;≤&#160;<xsl:apply-templates select="e:snd" /></xsl:template>
  <xsl:template match="e:gt"><xsl:apply-templates select="e:fst" />&#160;&gt;&#160;<xsl:apply-templates select="e:snd" /></xsl:template>
  <xsl:template match="e:geq"><xsl:apply-templates select="e:fst" />&#160;≥&#160;<xsl:apply-templates select="e:snd" /></xsl:template>

  <xsl:template match="e:forall">∀<xsl:apply-templates select="e:fst" />.&#160;<xsl:apply-templates select="e:snd" /></xsl:template>
  <xsl:template match="e:exists">∃<xsl:apply-templates select="e:fst" />.&#160;<xsl:apply-templates select="e:snd" /></xsl:template>

  <xsl:template match="e:impl"><xsl:apply-templates select="e:fst" />&#160;→&#160;<xsl:apply-templates select="e:snd" /></xsl:template>
  <xsl:template match="e:doubleimpl"><xsl:apply-templates select="e:fst" />&#160;↔&#160;<xsl:apply-templates select="e:snd" /></xsl:template>

  <xsl:template match="e:logimpl"><xsl:apply-templates select="e:fst" />&#160;⇒&#160;<xsl:apply-templates select="e:snd" /></xsl:template>

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

  <xsl:template match="e:pre|e:post|e:cond"><span class="cond math"><xsl:apply-templates /></span></xsl:template>
  <xsl:template match="e:set">{<xsl:apply-templates select="@*|node()" />}</xsl:template>
  <xsl:template match="e:setb"><span class="set math"><xsl:apply-templates select="e:fst" />&#160;:&#160;<xsl:apply-templates select="e:snd" /></span></xsl:template>
  <xsl:template match="e:command"><span class="command code"><xsl:apply-templates /></span></xsl:template>

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

  <xsl:template match="e:pred"><span class="pred_name"><xsl:value-of select="@name" /></span>(<xsl:apply-templates />)</xsl:template>
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
      <td><xsl:number />.</td>
      <td>
	<xsl:attribute name="class"><xsl:value-of select="@indent" /></xsl:attribute>
	<xsl:apply-templates select="e:derive"/>
      </td>
      <xsl:apply-templates select="e:by"/>
    </tr>
  </xsl:template>
  <xsl:template match="e:derive"><xsl:apply-templates select="@*|node()" /></xsl:template>
  <xsl:template match="e:by"><td><xsl:apply-templates select="@*|node()" /></td></xsl:template>
  <xsl:template match="e:from"><xsl:value-of select="@name"/></xsl:template> <!-- ancestor::e:derivation/child::e:step[name=@name] -->


  <!-- passthrough -->
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>
