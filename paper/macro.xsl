<xsl:stylesheet
   version="1.0"
   xmlns="http://www.w3.org/1999/xhtml"
   xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
   xmlns:xi="http://www.w3.org/2001/XInclude"
   xmlns:e="http://eegg.github.com/htmlx"
   xmlns:m="http://eegg.github.com/macro">

  <xsl:template match="m:api"><abbr title="Application Programming Interface" class="smallcaps">API</abbr></xsl:template>
  <xsl:template match="m:url"><abbr title="Uniform Resource Locator" class="smallcaps">URL</abbr></xsl:template>
  <xsl:template match="m:adt"><abbr title="Abstract Data Type" class="smallcaps">ADT</abbr></xsl:template>
  <xsl:template match="m:ll"><abbr title="Linked List" class="smallcaps">LL</abbr></xsl:template>
  <xsl:template match="m:llrb"><abbr title="Left-Leaning Red-Black Tree" class="smallcaps">LLRB</abbr></xsl:template>
  <xsl:template match="m:bst"><abbr title="Binary Search Tree" class="smallcaps">BST</abbr></xsl:template>
  <xsl:template match="m:rbt"><abbr title="Red-Black Tree" class="smallcaps">RBT</abbr></xsl:template>

  <xsl:template match="m:inf"><span>∞</span></xsl:template>
  <xsl:template match="m:neginf"><e:negative><span>∞</span></e:negative></xsl:template>

  <xsl:template match="m:null"><span>␀</span></xsl:template>

  <xsl:template match="m:eg"><i>e.g.</i></xsl:template>
  <xsl:template match="m:ie"><i>i.e.</i></xsl:template>
  <xsl:template match="m:vs"><i>vs.</i></xsl:template>

  <xsl:template match="m:empty"><span>∅</span></xsl:template>

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

  <xsl:template match="m:red"><span class="red">●</span></xsl:template>
  <xsl:template match="m:black"><span class="black">●</span></xsl:template>

  <xsl:template match="m:trueOfSubsetMembersTrueOfSetMembers">
      <e:logimpl>
	<e:fst><e:and>
	    <e:fst><e:forall>
		<e:vars><e:in>
		    <e:fst><e:var n="x"/></e:fst>
		    <e:snd><e:st n="A"/></e:snd>
		</e:in></e:vars>
		<e:expr><e:pred name="P"><e:var n="x"/></e:pred></e:expr>
	    </e:forall></e:fst>
	    <e:snd><e:forall>
		<e:vars><e:in>
		    <e:fst><e:var n="x"/></e:fst>
		    <e:snd><e:st n="B"/></e:snd>
		</e:in></e:vars>
		<e:expr><e:pred name="P"><e:var n="x"/></e:pred></e:expr>
	    </e:forall></e:snd>
	</e:and></e:fst>
	<e:snd><e:forall>
	    <e:vars><e:in>
		<e:fst><e:var n="x"/></e:fst>
		<e:snd>(<e:union>
		    <e:fst><e:st n="A"/></e:fst>
		    <e:snd><e:st n="B"/></e:snd>
		  </e:union>)</e:snd>
	    </e:in></e:vars>
	    <e:expr><e:pred name="P"><e:var n="x"/></e:pred></e:expr>
	</e:forall></e:snd>
      </e:logimpl>
  </xsl:template>

  <xsl:template match="m:AllMembersThenAllMembersOfSubset">
    <e:logimpl>
      <e:and>
	<e:forall>
	  <e:vars><e:in><e:var n="x"/><e:st n="A"/></e:in></e:vars>
	  <e:expr><e:pred name="P"><e:var n="x"/></e:pred></e:expr>
	</e:forall>
	<e:subset><e:st n="B"/><e:st n="A"/></e:subset>
      </e:and>
      <e:forall>
	<e:vars><e:in><e:var n="x"/><e:st n="B"/></e:in></e:vars>
	<e:expr><e:pred name="P"><e:var n="x"/></e:pred></e:expr>
      </e:forall>      
    </e:logimpl>
  </xsl:template>

  <xsl:template match="m:unionWithEmptyIsSet">
    <e:eq>
      <e:fst><e:union><e:fst><e:st n="A"/></e:fst><e:snd>∅</e:snd></e:union></e:fst>
      <e:snd><e:st n="A"/></e:snd>
    </e:eq>
  </xsl:template>

  <xsl:template match="m:sameOp">
    <e:logimpl>
      <e:fst><e:eq>
	  <e:fst><e:var n="a"/></e:fst>
	  <e:snd><e:var n="b"/></e:snd>
      </e:eq></e:fst>
      <e:snd><e:eq>
	  <e:fst><e:func n="f"><e:var n="a"/></e:func></e:fst>
	  <e:snd><e:func n="f"><e:var n="b"/></e:func></e:snd>
      </e:eq></e:snd>
    </e:logimpl>
  </xsl:template>

  <xsl:template match="m:distribSetminusUnion">
    <e:eq>
      <e:fst><e:setminus>
	  <e:fst>(<e:union>
	      <e:fst><e:st n="A"/></e:fst>
	      <e:snd><e:st n="B"/></e:snd>
	    </e:union>)</e:fst>
	  <e:snd><e:st n="C"/></e:snd>
      </e:setminus></e:fst>
      <e:snd><e:union>
	  <e:fst>(<e:setminus>
	      <e:fst><e:st n="A"/></e:fst>
	      <e:snd><e:st n="C"/></e:snd>
	  </e:setminus>)</e:fst>
	  <e:snd>(<e:setminus>
	      <e:fst><e:st n="B"/></e:fst>
	      <e:snd><e:st n="C"/></e:snd>
	  </e:setminus>)</e:snd>
      </e:union></e:snd>
    </e:eq>
  </xsl:template>

  <xsl:template match="m:SetMinusSetIsEmpty">
    <e:eq>
      <e:fst><e:setminus>
	  <e:fst><e:st n="A"/></e:fst>
	  <e:snd><e:st n="A"/></e:snd>
      </e:setminus></e:fst>
      <e:snd>∅</e:snd>
    </e:eq>
  </xsl:template>

  <!-- passthrough -->
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>
