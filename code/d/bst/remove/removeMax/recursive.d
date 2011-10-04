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
  /// <e:exists><e:fst><e:var n="v"/></e:fst>
  ///   <e:snd><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:snd>
  /// </e:exists>

  // Open <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>.
  /// <e:exists>
  ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
  ///   <e:snd><e:indent>
  ///     <e:sep>
  ///       <e:fst><e:fcell>
  ///             <e:fst><code>root</code></e:fst>
  ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
  ///           </e:fcell></e:fst>
  ///       <e:snd><e:sep>
  ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
  ///             <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
  ///       </e:sep></e:snd>
  ///     </e:sep> ∧<br />
  ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  ///   </e:indent></e:snd>
  /// </e:exists>

  auto r = root.c[1];
  // <m:existsElim/> on <e:var n="r"/> as <code>r</code>.
  /// <e:exists>
  ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
  ///   <e:snd><e:indent>
  ///     <e:sep>
  ///       <e:fst><e:fcell>
  ///             <e:fst><code>root</code></e:fst>
  ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<code>r</code></e:snd>
  ///           </e:fcell></e:fst>
  ///       <e:snd><e:sep>
  ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
  ///             <e:snd><e:pred name="Tree"><code>r</code>, <e:st n="R"/></e:pred></e:snd>
  ///       </e:sep></e:snd>
  ///     </e:sep> ∧<br />
  ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
  ///   </e:indent></e:snd>
  /// </e:exists>

  if (r == null) {
    // Assert if-condition.  Substitution.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///             <e:fst><code>root</code></e:fst>
    ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<code>null</code></e:snd>
    ///           </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
    ///             <e:snd><e:pred name="Tree"><code>null</code>, <e:st n="R"/></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>

    // Lemma: <e:logimpl>
    //   <e:fst><e:pred name="Tree"><code>null</code>, <e:st n="R"/></e:pred></e:fst>
    //   <e:snd><e:pred name="EmptyTree"><code>null</code>, <e:st n="R"/></e:pred></e:snd>
    // </e:logimpl>
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///             <e:fst><code>root</code></e:fst>
    ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<code>null</code></e:snd>
    ///           </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
    ///             <e:snd><e:pred name="EmptyTree"><code>null</code>, <e:st n="R"/></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>

    // Lemma: <e:logimpl>
    //   <e:fst><e:pred name="EmptyTree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:fst>
    //   <e:snd><e:eq><e:fst><e:st n="R"/></e:fst><e:snd><m:scemp/></e:snd></e:eq></e:snd>
    // </e:logimpl>.  Substitution.  Discard <e:eq><e:fst><e:st n="R"/></e:fst><e:snd><m:scemp/></e:snd></e:eq>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///             <e:fst><code>root</code></e:fst>
    ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<code>null</code></e:snd>
    ///           </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
    ///             <e:snd><e:pred name="EmptyTree"><code>null</code>, <m:scemp/></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <m:scemp/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>

    // Open <e:pred name="EmptyTree"><code>null</code>, <m:scemp/></e:pred>.  Discard <e:eq><e:fst><code>null</code></e:fst><e:snd><code>null</code></e:snd></e:eq>
    // and <e:eq><e:fst><m:scemp/></e:fst><e:snd><m:scemp/></e:snd></e:eq>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///             <e:fst><code>root</code></e:fst>
    ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<code>null</code></e:snd>
    ///           </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
    ///             <e:snd><m:hemp/></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <m:scemp/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>
    
    // <e:sep><e:fst><e:predicate>X</e:predicate></e:fst><e:snd><m:hemp/></e:snd></e:sep> = <e:predicate>X</e:predicate>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///             <e:fst><code>root</code></e:fst>
    ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<code>null</code></e:snd>
    ///           </e:fcell></e:fst>
    ///       <e:snd><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <m:scemp/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>

    // Lemma: <e:logimpl>
    //   <e:fst><e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <m:scemp/>, <e:st n="S"/></e:pred></e:fst>
    //   <e:snd><e:and>
    //     <e:fst><e:pred name="Max"><e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
    //     <e:snd><e:eq>
    //       <e:fst><e:st n="L"/></e:fst>
    //       <e:snd><e:setminus>
    //         <e:fst><e:st n="S"/></e:fst>
    //         <e:snd><e:set><e:var n="v"/></e:set></e:snd>
    //       </e:setminus></e:snd>
    //     </e:eq></e:snd>
    //   </e:and></e:snd>
    // </e:logimpl>.<br />
    // Discard <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <m:scemp/>, <e:st n="S"/></e:pred>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///         <e:fst><code>root</code></e:fst>
    ///         <e:snd><e:var n="v"/>,<e:var n="l"/>,<code>null</code></e:snd>
    ///       </e:fcell></e:fst>
    ///       <e:snd><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="Max"><e:var n="v"/>, <e:st n="S"/></e:pred> ∧
    ///     <e:eq>
    ///       <e:fst><e:st n="L"/></e:fst>
    ///       <e:snd><e:setminus>
    ///         <e:fst><e:st n="S"/></e:fst>
    ///         <e:snd><e:set><e:var n="v"/></e:set></e:snd>
    ///       </e:setminus></e:snd>
    ///     </e:eq>
    ///   </e:indent></e:snd>
    /// </e:exists>

    // Substitution.  Discard <e:eq>
    //       <e:fst><e:st n="L"/></e:fst>
    //       <e:snd><e:setminus>
    //         <e:fst><e:st n="S"/></e:fst>
    //         <e:snd><e:set><e:var n="v"/></e:set></e:snd>
    //       </e:setminus></e:snd>
    //     </e:eq>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///         <e:fst><code>root</code></e:fst>
    ///         <e:snd><e:var n="v"/>,<e:var n="l"/>,<code>null</code></e:snd>
    ///       </e:fcell></e:fst>
    ///       <e:snd><e:pred name="Tree"><e:var n="l"/>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:pred></e:snd>
    ///     </e:sep> ∧
    ///     <e:pred name="Max"><e:var n="v"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>

    max = root.value;
    newRoot = root.c[0];
    // Assignment (twice).
    /// <e:sep>
    ///   <e:fst><e:fcell>
    ///     <e:fst><code>root</code></e:fst>
    ///     <e:snd><code>max</code>,<code>newRoot</code>,<code>null</code></e:snd>
    ///   </e:fcell></e:fst>
    ///   <e:snd><e:pred name="Tree"><code>newRoot</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>max</code></e:set></e:snd></e:setminus></e:pred></e:snd>
    /// </e:sep> ∧
    /// <e:pred name="Max"><code>max</code>, <e:st n="S"/></e:pred>

    delete root;
    // Free heap chunk.
    /// <e:pred name="Tree"><code>newRoot</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>max</code></e:set></e:snd></e:setminus></e:pred> ∧
    /// <e:pred name="Max"><code>max</code>, <e:st n="S"/></e:pred>
  }
  else {
    // Deny if-condition.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///             <e:fst><code>root</code></e:fst>
    ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<code>r</code></e:snd>
    ///           </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
    ///             <e:snd><e:pred name="Tree"><code>r</code>, <e:st n="R"/></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
    ///     <e:noteq><e:fst><code>r</code></e:fst><e:snd><code>null</code></e:snd></e:noteq>
    ///   </e:indent></e:snd>
    /// </e:exists>

    // Lemma: <e:logimpl>
    //   <e:fst><e:and>
    //     <e:fst><e:pred name="Tree"><code>r</code>, <e:st n="R"/></e:pred></e:fst>
    //     <e:snd><e:noteq><e:fst><code>r</code></e:fst><e:snd><code>null</code></e:snd></e:noteq></e:snd>
    //   </e:and></e:fst>
    //   <e:snd><e:pred name="NonEmptyTree"><code>r</code>, <e:st n="R"/></e:pred></e:snd>
    // </e:logimpl>.
    // Discard <e:noteq><e:fst><e:var n="r"/></e:fst><e:snd><code>null</code></e:snd></e:noteq>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///             <e:fst><code>root</code></e:fst>
    ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<code>r</code></e:snd>
    ///           </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
    ///             <e:snd><e:pred name="NonEmptyTree"><code>r</code>, <e:st n="R"/></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>

    auto d = removeMax(r); auto rightMax = d.max; auto rightRoot = d.root;
    // Specification for <code>removeMax</code>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///             <e:fst><code>root</code></e:fst>
    ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<code>r</code></e:snd>
    ///           </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
    ///             <e:snd><e:pred name="Tree"><code>rightRoot</code>, <e:setminus><e:fst><e:st n="R"/></e:fst><e:snd><e:set><code>rightMax</code></e:set></e:snd></e:setminus></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
    ///     <e:pred name="Max"><code>rightMax</code>, <e:st n="R"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>

    root.c[1] = rightRoot;
    // Assignment.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///             <e:fst><code>root</code></e:fst>
    ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<code>rightRoot</code></e:snd>
    ///           </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
    ///             <e:snd><e:pred name="Tree"><code>rightRoot</code>, <e:setminus><e:fst><e:st n="R"/></e:fst><e:snd><e:set><code>rightMax</code></e:set></e:snd></e:setminus></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
    ///     <e:pred name="Max"><code>rightMax</code>, <e:st n="R"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>

    // <m:existsIntro/> on <code>rightRoot</code> as <e:var n="r"/>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///             <e:fst><code>root</code></e:fst>
    ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
    ///           </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
    ///             <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:setminus><e:fst><e:st n="R"/></e:fst><e:snd><e:set><code>rightMax</code></e:set></e:snd></e:setminus></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
    ///     <e:pred name="Max"><code>rightMax</code>, <e:st n="R"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>

    // Lemma: <e:logimpl>
    //   <e:fst><e:and>
    //     <e:fst><e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred></e:fst>
    //     <e:snd><e:pred name="Max"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
    //   </e:and></e:fst>
    //   <e:snd><e:and>
    //     <e:fst><e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:setminus><e:fst><e:st n="R"/></e:fst><e:snd><e:set><e:var n="r"/></e:set></e:snd></e:setminus>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="r"/></e:set></e:snd></e:setminus></e:pred></e:fst>
    //     <e:snd><e:pred name="Max"><e:var n="r"/>, <e:st n="S"/></e:pred></e:snd>
    //   </e:and></e:snd>
    // </e:logimpl>.<br />
    // Discard <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> and <e:pred name="Max"><code>rightMax</code>, <e:st n="R"/></e:pred>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///             <e:fst><code>root</code></e:fst>
    ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
    ///           </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
    ///             <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:setminus><e:fst><e:st n="R"/></e:fst><e:snd><e:set><code>rightMax</code></e:set></e:snd></e:setminus></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:setminus><e:fst><e:st n="R"/></e:fst><e:snd><e:set><code>rightMax</code></e:set></e:snd></e:setminus>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>rightMax</code></e:set></e:snd></e:setminus></e:pred> ∧
    ///     <e:pred name="Max"><code>rightMax</code>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>

    // <m:existsIntro/> on <e:setminus><e:fst><e:st n="R"/></e:fst><e:snd><e:set><code>rightMax</code></e:set></e:snd></e:setminus> as <e:st n="R"/>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///             <e:fst><code>root</code></e:fst>
    ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
    ///           </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
    ///             <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>rightMax</code></e:set></e:snd></e:setminus></e:pred> ∧
    ///     <e:pred name="Max"><code>rightMax</code>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>

    // Close <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>rightMax</code></e:set></e:snd></e:setminus></e:pred>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/></e:fst>
    ///   <e:snd>
    ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>rightMax</code></e:set></e:snd></e:setminus></e:pred> ∧
    ///     <e:pred name="Max"><code>rightMax</code>, <e:st n="S"/></e:pred>
    ///   </e:snd>
    /// </e:exists>

    // Close <e:pred name="NonEmptyTree"><code>root</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>rightMax</code></e:set></e:snd></e:setminus></e:pred>.
    /// <e:pred name="NonEmptyTree"><code>root</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>rightMax</code></e:set></e:snd></e:setminus></e:pred> ∧
    /// <e:pred name="Max"><code>rightMax</code>, <e:st n="S"/></e:pred>

    // Weaken
    /// <e:pred name="Tree"><code>root</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>rightMax</code></e:set></e:snd></e:setminus></e:pred> ∧
    /// <e:pred name="Max"><code>rightMax</code>, <e:st n="S"/></e:pred>

    max = rightMax;
    newRoot = root;
    // Assignment.
    /// <e:pred name="Tree"><code>newRoot</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>max</code></e:set></e:snd></e:setminus></e:pred> ∧
    /// <e:pred name="Max"><code>max</code>, <e:st n="S"/></e:pred>
  }

  // If-rule.
  /// <e:pred name="Tree"><code>newRoot</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>max</code></e:set></e:snd></e:setminus></e:pred> ∧
  /// <e:pred name="Max"><code>max</code>, <e:st n="S"/></e:pred>

  RemoveMaxRet o = {max: max, root: newRoot};
  return o;
}