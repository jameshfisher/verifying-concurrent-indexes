module bst.remove.removeMax.recursive;


import bst.node;
import bst.remove.removeMax.RemoveMaxRet;

import std.stdio;

RemoveMaxRet removeMax(Node* root) {
  assert(root != null);

  int max;
  Node* newRoot;

  // Function precondition.
  /// <e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred>

  // Open <e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred>.
  /// <e:exists><e:var n="v"/>
  ///   <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>
  /// </e:exists>

  // Open <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>.
  /// <e:exists>
  ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
  ///   <e:expr><e:indent>
  ///     <e:sep>
  ///       <e:fcell>
  ///         <code>root</code>
  ///         <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
  ///       </e:fcell>
  ///       <e:sep>
  ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
  ///             <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
  ///       </e:sep>
  ///     </e:sep> ∧<br />
  ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  ///   </e:indent></e:expr>
  /// </e:exists>

  auto r = root.c[1];
  // <m:existsElim/> on <e:var n="r"/> as <code>r</code>.
  /// <e:exists>
  ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
  ///   <e:expr><e:indent>
  ///     <e:sep>
  ///       <e:fcell>
  ///         <code>root</code>
  ///         <e:list><e:var n="v"/>,<e:var n="l"/>,<code>r</code></e:list>
  ///       </e:fcell>
  ///       <e:sep>
  ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
  ///             <e:pred name="Tree"><code>r</code>, <e:st n="R"/></e:pred>
  ///       </e:sep>
  ///     </e:sep> ∧<br />
  ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  ///   </e:indent></e:expr>
  /// </e:exists>

  if (r == null) {
    // Assert if-condition.  Substitution.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///         <code>root</code>
    ///         <e:list><e:var n="v"/>,<e:var n="l"/>,<code>null</code></e:list>
    ///       </e:fcell>
    ///       <e:sep>
    ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
    ///             <e:pred name="Tree"><code>null</code>, <e:st n="R"/></e:pred>
    ///       </e:sep>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>

    // Lemma: <e:logimpl>
    //   <e:pred name="Tree"><code>null</code>, <e:st n="R"/></e:pred>
    //   <e:pred name="EmptyTree"><code>null</code>, <e:st n="R"/></e:pred>
    // </e:logimpl>
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///         <code>root</code>
    ///         <e:list><e:var n="v"/>,<e:var n="l"/>,<code>null</code></e:list>
    ///       </e:fcell>
    ///       <e:sep>
    ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
    ///             <e:pred name="EmptyTree"><code>null</code>, <e:st n="R"/></e:pred>
    ///       </e:sep>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>

    // Lemma: <e:logimpl>
    //   <e:pred name="EmptyTree"><e:var n="r"/>, <e:st n="R"/></e:pred>
    //   <e:eq><e:st n="R"/><m:scemp/></e:eq>
    // </e:logimpl>.  Substitution.  Discard <e:eq><e:st n="R"/><m:scemp/></e:eq>.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///         <code>root</code>
    ///         <e:list><e:var n="v"/>,<e:var n="l"/>,<code>null</code></e:list>
    ///       </e:fcell>
    ///       <e:sep>
    ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
    ///             <e:pred name="EmptyTree"><code>null</code>, <m:scemp/></e:pred>
    ///       </e:sep>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <m:scemp/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>

    // Open <e:pred name="EmptyTree"><code>null</code>, <m:scemp/></e:pred>.  Discard <e:eq><code>null</code><code>null</code></e:eq>
    // and <e:eq><m:scemp/><m:scemp/></e:eq>.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///         <code>root</code>
    ///         <e:list><e:var n="v"/>,<e:var n="l"/>,<code>null</code></e:list>
    ///       </e:fcell>
    ///       <e:sep>
    ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
    ///             <m:hemp/>
    ///       </e:sep>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <m:scemp/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>
    
    // <e:sep><e:predicate>X</e:predicate><m:hemp/></e:sep> = <e:predicate>X</e:predicate>.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///         <code>root</code>
    ///         <e:list><e:var n="v"/>,<e:var n="l"/>,<code>null</code></e:list>
    ///       </e:fcell>
    ///       <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <m:scemp/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>

    // Lemma: <e:logimpl>
    //   <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <m:scemp/>, <e:st n="S"/></e:pred>
    //   <e:and>
    //     <e:pred name="Max"><e:var n="v"/>, <e:st n="S"/></e:pred>
    //     <e:eq>
    //       <e:st n="L"/>
    //       <e:setminus>
    //         <e:st n="S"/>
    //         <e:set><e:var n="v"/></e:set>
    //       </e:setminus>
    //     </e:eq>
    //   </e:and>
    // </e:logimpl>.<br />
    // Discard <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <m:scemp/>, <e:st n="S"/></e:pred>.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="l"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///         <code>root</code>
    ///         <e:list><e:var n="v"/>,<e:var n="l"/>,<code>null</code></e:list>
    ///       </e:fcell>
    ///       <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="Max"><e:var n="v"/>, <e:st n="S"/></e:pred> ∧
    ///     <e:eq>
    ///       <e:st n="L"/>
    ///       <e:setminus>
    ///         <e:st n="S"/>
    ///         <e:set><e:var n="v"/></e:set>
    ///       </e:setminus>
    ///     </e:eq>
    ///   </e:indent></e:expr>
    /// </e:exists>

    // Substitution.  Discard <e:eq>
    //       <e:st n="L"/>
    //       <e:setminus>
    //         <e:st n="S"/>
    //         <e:set><e:var n="v"/></e:set>
    //       </e:setminus>
    //     </e:eq>.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="l"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///         <code>root</code>
    ///         <e:list><e:var n="v"/>,<e:var n="l"/>,<code>null</code></e:list>
    ///       </e:fcell>
    ///       <e:pred name="Tree"><e:var n="l"/>, <e:setminus><e:st n="S"/><e:set><e:var n="v"/></e:set></e:setminus></e:pred>
    ///     </e:sep> ∧
    ///     <e:pred name="Max"><e:var n="v"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>

    max = root.value;
    newRoot = root.c[0];
    // Assignment (twice).
    /// <e:sep>
    ///   <e:fcell>
    ///     <code>root</code>
    ///     <e:list><code>max</code>,<code>newRoot</code>,<code>null</code></e:list>
    ///   </e:fcell>
    ///   <e:pred name="Tree"><code>newRoot</code>, <e:setminus><e:st n="S"/><e:set><code>max</code></e:set></e:setminus></e:pred>
    /// </e:sep> ∧
    /// <e:pred name="Max"><code>max</code>, <e:st n="S"/></e:pred>

    delete root;
    // Free heap chunk.
    /// <e:pred name="Tree"><code>newRoot</code>, <e:setminus><e:st n="S"/><e:set><code>max</code></e:set></e:setminus></e:pred> ∧
    /// <e:pred name="Max"><code>max</code>, <e:st n="S"/></e:pred>
  }
  else {
    // Deny if-condition.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///             <code>root</code>
    ///             <e:list><e:var n="v"/>,<e:var n="l"/>,<code>r</code></e:list>
    ///           </e:fcell>
    ///       <e:sep>
    ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
    ///             <e:pred name="Tree"><code>r</code>, <e:st n="R"/></e:pred>
    ///       </e:sep>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
    ///     <e:noteq><code>r</code><code>null</code></e:noteq>
    ///   </e:indent></e:expr>
    /// </e:exists>

    // Lemma: <e:logimpl>
    //   <e:and>
    //     <e:pred name="Tree"><code>r</code>, <e:st n="R"/></e:pred>
    //     <e:noteq><code>r</code><code>null</code></e:noteq>
    //   </e:and>
    //   <e:pred name="NonEmptyTree"><code>r</code>, <e:st n="R"/></e:pred>
    // </e:logimpl>.
    // Discard <e:noteq><e:var n="r"/><code>null</code></e:noteq>.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///         <code>root</code>
    ///         <e:list><e:var n="v"/>,<e:var n="l"/>,<code>r</code></e:list>
    ///       </e:fcell>
    ///       <e:sep>
    ///         <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
    ///         <e:pred name="NonEmptyTree"><code>r</code>, <e:st n="R"/></e:pred>
    ///       </e:sep>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>

    auto d = removeMax(r); auto rightMax = d.max; auto rightRoot = d.root;
    // Specification for <code>removeMax</code>.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///         <code>root</code>
    ///         <e:list><e:var n="v"/>,<e:var n="l"/>,<code>r</code></e:list>
    ///       </e:fcell>
    ///       <e:sep>
    ///         <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
    ///         <e:pred name="Tree"><code>rightRoot</code>, <e:setminus><e:st n="R"/><e:set><code>rightMax</code></e:set></e:setminus></e:pred>
    ///       </e:sep>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
    ///     <e:pred name="Max"><code>rightMax</code>, <e:st n="R"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>

    root.c[1] = rightRoot;
    // Assignment.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///         <code>root</code>
    ///         <e:list><e:var n="v"/>,<e:var n="l"/>,<code>rightRoot</code></e:list>
    ///       </e:fcell>
    ///       <e:sep>
    ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
    ///             <e:pred name="Tree"><code>rightRoot</code>, <e:setminus><e:st n="R"/><e:set><code>rightMax</code></e:set></e:setminus></e:pred>
    ///       </e:sep>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
    ///     <e:pred name="Max"><code>rightMax</code>, <e:st n="R"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>

    // <m:existsIntro/> on <code>rightRoot</code> as <e:var n="r"/>.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///         <code>root</code>
    ///         <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
    ///       </e:fcell>
    ///       <e:sep>
    ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
    ///             <e:pred name="Tree"><e:var n="r"/>, <e:setminus><e:st n="R"/><e:set><code>rightMax</code></e:set></e:setminus></e:pred>
    ///       </e:sep>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
    ///     <e:pred name="Max"><code>rightMax</code>, <e:st n="R"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>

    // Lemma: <e:logimpl>
    //   <e:and>
    //     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    //     <e:pred name="Max"><e:var n="r"/>, <e:st n="R"/></e:pred>
    //   </e:and>
    //   <e:and>
    //     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:setminus><e:st n="R"/><e:set><e:var n="r"/></e:set></e:setminus>, <e:setminus><e:st n="S"/><e:set><e:var n="r"/></e:set></e:setminus></e:pred>
    //     <e:pred name="Max"><e:var n="r"/>, <e:st n="S"/></e:pred>
    //   </e:and>
    // </e:logimpl>.<br />
    // Discard <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> and <e:pred name="Max"><code>rightMax</code>, <e:st n="R"/></e:pred>.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///         <code>root</code>
    ///         <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
    ///       </e:fcell>
    ///       <e:sep>
    ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
    ///             <e:pred name="Tree"><e:var n="r"/>, <e:setminus><e:st n="R"/><e:set><code>rightMax</code></e:set></e:setminus></e:pred>
    ///       </e:sep>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:setminus><e:st n="R"/><e:set><code>rightMax</code></e:set></e:setminus>, <e:setminus><e:st n="S"/><e:set><code>rightMax</code></e:set></e:setminus></e:pred> ∧
    ///     <e:pred name="Max"><code>rightMax</code>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>

    // <m:existsIntro/> on <e:setminus><e:st n="R"/><e:set><code>rightMax</code></e:set></e:setminus> as <e:st n="R"/>.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
    ///   <e:expr><e:indent>
    ///     <e:sep>
    ///       <e:fcell>
    ///         <code>root</code>
    ///         <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
    ///       </e:fcell>
    ///       <e:sep>
    ///         <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
    ///         <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
    ///       </e:sep>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:setminus><e:st n="S"/><e:set><code>rightMax</code></e:set></e:setminus></e:pred> ∧
    ///     <e:pred name="Max"><code>rightMax</code>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>

    // Close <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:setminus><e:st n="S"/><e:set><code>rightMax</code></e:set></e:setminus></e:pred>.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/></e:vars>
    ///   <e:expr>
    ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:setminus><e:st n="S"/><e:set><code>rightMax</code></e:set></e:setminus></e:pred> ∧
    ///     <e:pred name="Max"><code>rightMax</code>, <e:st n="S"/></e:pred>
    ///   </e:expr>
    /// </e:exists>

    // Close <e:pred name="NonEmptyTree"><code>root</code>, <e:setminus><e:st n="S"/><e:set><code>rightMax</code></e:set></e:setminus></e:pred>.
    /// <e:pred name="NonEmptyTree"><code>root</code>, <e:setminus><e:st n="S"/><e:set><code>rightMax</code></e:set></e:setminus></e:pred> ∧
    /// <e:pred name="Max"><code>rightMax</code>, <e:st n="S"/></e:pred>

    // Weaken
    /// <e:pred name="Tree"><code>root</code>, <e:setminus><e:st n="S"/><e:set><code>rightMax</code></e:set></e:setminus></e:pred> ∧
    /// <e:pred name="Max"><code>rightMax</code>, <e:st n="S"/></e:pred>

    max = rightMax;
    newRoot = root;
    // Assignment.
    /// <e:pred name="Tree"><code>newRoot</code>, <e:setminus><e:st n="S"/><e:set><code>max</code></e:set></e:setminus></e:pred> ∧
    /// <e:pred name="Max"><code>max</code>, <e:st n="S"/></e:pred>
  }

  // If-rule.
  /// <e:pred name="Tree"><code>newRoot</code>, <e:setminus><e:st n="S"/><e:set><code>max</code></e:set></e:setminus></e:pred> ∧
  /// <e:pred name="Max"><code>max</code>, <e:st n="S"/></e:pred>

  RemoveMaxRet o = {max: max, root: newRoot};
  return o;
}