module bst.removeRoot;

import bst.node;
import bst.remove.removeMax.removeMax;
import bst.remove.removeMax.RemoveMaxRet;


Node* removeRoot(Node* root) {
  assert(root != null); // [snip]
  Node* o;
  // Function precondition.
  /// <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>

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

  if (root.c[0] == null) {
    // Assert if-condition.  Substitution.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///             <e:fst><code>root</code></e:fst>
    ///             <e:snd><e:var n="v"/>,<code>null</code>,<e:var n="r"/></e:snd>
    ///           </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///             <e:fst><e:pred name="Tree"><code>null</code>, <e:st n="L"/></e:pred></e:fst>
    ///             <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>

    // Lemma: <e:logimpl>
    //   <e:fst><e:pred name="Tree"><code>null</code>, <e:st n="S"/></e:pred></e:fst>
    //   <e:snd><e:pred name="EmptyTree"><code>null</code>, <e:st n="S"/></e:pred></e:snd>
    // </e:logimpl>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///             <e:fst><code>root</code></e:fst>
    ///             <e:snd><e:var n="v"/>,<code>null</code>,<e:var n="r"/></e:snd>
    ///           </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///             <e:fst><e:pred name="EmptyTree"><code>null</code>, <e:st n="L"/></e:pred></e:fst>
    ///             <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>

    // Open <e:pred name="EmptyTree"><code>null</code>, <e:st n="L"/></e:pred>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///             <e:fst><code>root</code></e:fst>
    ///             <e:snd><e:var n="v"/>,<code>null</code>,<e:var n="r"/></e:snd>
    ///           </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///             <e:fst><m:hemp/></e:fst>
    ///             <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
    ///     <e:eq><e:fst><e:st n="L"/></e:fst><e:snd><m:scemp/></e:snd></e:eq>
    ///   </e:indent></e:snd>
    /// </e:exists>

    // Use <e:sep><e:fst><e:predicate>X</e:predicate></e:fst><e:snd><m:hemp/></e:snd></e:sep> = <e:predicate>X</e:predicate>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///         <e:fst><code>root</code></e:fst>
    ///         <e:snd><e:var n="v"/>,<code>null</code>,<e:var n="r"/></e:snd>
    ///       </e:fcell></e:fst>
    ///       <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
    ///     <e:eq><e:fst><e:st n="L"/></e:fst><e:snd><m:scemp/></e:snd></e:eq>
    ///   </e:indent></e:snd>
    /// </e:exists>

    // Substitution.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="r"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///         <e:fst><code>root</code></e:fst>
    ///         <e:snd><e:var n="v"/>,<code>null</code>,<e:var n="r"/></e:snd>
    ///       </e:fcell></e:fst>
    ///       <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><m:scemp/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>

    o = root.c[1];
    // Assignment.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///             <e:fst><code>root</code></e:fst>
    ///             <e:snd><e:var n="v"/>,<code>null</code>,<code>o</code></e:snd>
    ///           </e:fcell></e:fst>
    ///       <e:snd><e:pred name="Tree"><code>o</code>, <e:st n="R"/></e:pred></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><m:scemp/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>

    delete root;
    // Free heap chunk.
    /// <e:exists>
    ///   <e:fst><e:st n="R"/></e:fst>
    ///   <e:snd>
    ///     <e:pred name="Tree"><code>o</code>, <e:st n="R"/></e:pred> ∧
    ///     <e:pred name="TreeCompose"><m:scemp/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:snd>
    /// </e:exists>

    // Lemma: <e:logimpl>
    //   <e:fst><e:pred name="TreeCompose"><m:scemp/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred></e:fst>
    //   <e:snd><e:eq><e:fst><e:st n="R"/></e:fst><e:snd><e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:snd></e:eq></e:snd>
    // </e:logimpl>
    /// <e:exists>
    ///   <e:fst><e:st n="R"/></e:fst>
    ///   <e:snd>
    ///     <e:pred name="Tree"><code>o</code>, <e:st n="R"/></e:pred> ∧
    ///     <e:pred name="TreeCompose"><m:scemp/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
    ///     <e:eq><e:fst><e:st n="R"/></e:fst><e:snd><e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:snd></e:eq>
    ///   </e:snd>
    /// </e:exists>

    // Substitution.  Discard <e:pred name="TreeCompose"><m:scemp/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    // and <e:eq><e:fst><e:st n="R"/></e:fst><e:snd><e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:snd></e:eq>.
    /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:pred>
  }
  else {
    // Deny if-condition.
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
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
    ///     <e:noteq><e:fst><e:var n="l"/></e:fst><e:snd><code>null</code></e:snd></e:noteq>
    ///   </e:indent></e:snd>
    /// </e:exists>

    // Lemma: .
    /// <e:exists>
    ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
    ///   <e:snd><e:indent>
    ///     <e:sep>
    ///       <e:fst><e:fcell>
    ///             <e:fst><code>root</code></e:fst>
    ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
    ///           </e:fcell></e:fst>
    ///       <e:snd><e:sep>
    ///             <e:fst><e:pred name="NonEmptyTree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
    ///             <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
    ///       </e:sep></e:snd>
    ///     </e:sep> ∧<br />
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>

    if (root.c[1] == null) {
      // This branch mostly symmetrical to the previous…
      o = root.c[0];
      delete root;
      /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:pred>
    }
    else {
      // Deny if-condition.  Use lemma: .
      /// <e:exists>
      ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
      ///   <e:snd><e:indent>
      ///     <e:sep>
      ///       <e:fst><e:fcell>
      ///             <e:fst><code>root</code></e:fst>
      ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
      ///           </e:fcell></e:fst>
      ///       <e:snd><e:sep>
      ///             <e:fst><e:pred name="NonEmptyTree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
      ///             <e:snd><e:pred name="NonEmptyTree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
      ///       </e:sep></e:snd>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
      ///   </e:indent></e:snd>
      /// </e:exists>
      
      RemoveMaxRet r = removeMax(root.c[0]); auto nv = r.max; auto newLeft = r.root;
      // Specification of <code>removeMax</code>.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
      ///   <e:snd><e:indent>
      ///     <e:sep>
      ///       <e:fst><e:fcell>
      ///             <e:fst><code>root</code></e:fst>
      ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
      ///           </e:fcell></e:fst>
      ///       <e:snd><e:sep>
      ///             <e:fst><e:pred name="Tree"><code>newLeft</code>, <e:setminus><e:fst><e:st n="L"/></e:fst><e:snd><e:set><code>nv</code></e:set></e:snd></e:setminus></e:pred></e:fst>
      ///             <e:snd><e:pred name="NonEmptyTree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
      ///       </e:sep></e:snd>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
      ///     <e:pred name="Max"><code>nv</code>, <e:st n="L"/></e:pred>
      ///   </e:indent></e:snd>
      /// </e:exists>

      root.value = max; root.c[0] = newLeft;
      // Assignment.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
      ///   <e:snd><e:indent>
      ///     <e:sep>
      ///       <e:fst><e:fcell>
      ///             <e:fst><code>root</code></e:fst>
      ///             <e:snd><code>nv</code>,<code>newLeft</code>,<e:var n="r"/></e:snd>
      ///           </e:fcell></e:fst>
      ///       <e:snd><e:sep>
      ///             <e:fst><e:pred name="Tree"><code>newLeft</code>, <e:setminus><e:fst><e:st n="L"/></e:fst><e:snd><e:set><code>nv</code></e:set></e:snd></e:setminus></e:pred></e:fst>
      ///             <e:snd><e:pred name="NonEmptyTree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
      ///       </e:sep></e:snd>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
      ///     <e:pred name="Max"><code>nv</code>, <e:st n="L"/></e:pred>
      ///   </e:indent></e:snd>
      /// </e:exists>

      // <m:existsIntro/> on <code>newLeft</code> as <e:var n="l"/>.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
      ///   <e:snd><e:indent>
      ///     <e:sep>
      ///       <e:fst><e:fcell>
      ///             <e:fst><code>root</code></e:fst>
      ///             <e:snd><code>nv</code>,<e:var n="l"/>,<e:var n="r"/></e:snd>
      ///           </e:fcell></e:fst>
      ///       <e:snd><e:sep>
      ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:setminus><e:fst><e:st n="L"/></e:fst><e:snd><e:set><code>nv</code></e:set></e:snd></e:setminus></e:pred></e:fst>
      ///             <e:snd><e:pred name="NonEmptyTree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
      ///       </e:sep></e:snd>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
      ///     <e:pred name="Max"><code>nv</code>, <e:st n="L"/></e:pred>
      ///   </e:indent></e:snd>
      /// </e:exists>

      // Weakening lemma: .
      /// <e:exists>
      ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
      ///   <e:snd><e:indent>
      ///     <e:sep>
      ///       <e:fst><e:fcell>
      ///             <e:fst><code>root</code></e:fst>
      ///             <e:snd><code>nv</code>,<e:var n="l"/>,<e:var n="r"/></e:snd>
      ///           </e:fcell></e:fst>
      ///       <e:snd><e:sep>
      ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:setminus><e:fst><e:st n="L"/></e:fst><e:snd><e:set><code>nv</code></e:set></e:snd></e:setminus></e:pred></e:fst>
      ///             <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
      ///       </e:sep></e:snd>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
      ///     <e:pred name="Max"><code>nv</code>, <e:st n="L"/></e:pred>
      ///   </e:indent></e:snd>
      /// </e:exists>

      // Lemma: <e:logimpl>
      //   <e:fst><e:and>
      //     <e:fst><e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred></e:fst>
      //     <e:snd><e:pred name="Max"><code>nv</code>, <e:st n="L"/></e:pred></e:snd>
      //   </e:and></e:fst>
      //   <e:snd><e:pred name="TreeCompose"><e:setminus><e:fst><e:st n="L"/></e:fst><e:snd><e:set><code>nv</code></e:set></e:snd></e:setminus>, <code>nv</code>, <e:st n="R"/>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:pred></e:snd>
      // </e:logimpl>.<br />
      // Discard <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
      // and <e:pred name="Max"><code>nv</code>, <e:st n="L"/></e:pred>.
      /// <e:exists>
      ///   <e:fst><e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
      ///   <e:snd><e:indent>
      ///     <e:sep>
      ///       <e:fst><e:fcell>
      ///             <e:fst><code>root</code></e:fst>
      ///             <e:snd><code>nv</code>,<e:var n="l"/>,<e:var n="r"/></e:snd>
      ///           </e:fcell></e:fst>
      ///       <e:snd><e:sep>
      ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:setminus><e:fst><e:st n="L"/></e:fst><e:snd><e:set><code>nv</code></e:set></e:snd></e:setminus></e:pred></e:fst>
      ///             <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
      ///       </e:sep></e:snd>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="TreeCompose"><e:setminus><e:fst><e:st n="L"/></e:fst><e:snd><e:set><code>nv</code></e:set></e:snd></e:setminus>, <code>nv</code>, <e:st n="R"/>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:pred>
      ///   </e:indent></e:snd>
      /// </e:exists>

      // <m:existsIntro/> on <e:setminus><e:fst><e:st n="L"/></e:fst><e:snd><e:set><code>nv</code></e:set></e:snd></e:setminus> as <e:st n="L"/>.
      /// <e:exists>
      ///   <e:fst><e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
      ///   <e:snd><e:indent>
      ///     <e:sep>
      ///       <e:fst><e:fcell>
      ///             <e:fst><code>root</code></e:fst>
      ///             <e:snd><code>nv</code>,<e:var n="l"/>,<e:var n="r"/></e:snd>
      ///           </e:fcell></e:fst>
      ///       <e:snd><e:sep>
      ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
      ///             <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
      ///       </e:sep></e:snd>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="TreeCompose"><e:st n="L"/>, <code>nv</code>, <e:st n="R"/>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:pred>
      ///   </e:indent></e:snd>
      /// </e:exists>

      // Close <e:pred name="TopOfTree"><code>root</code>, <code>nv</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:pred>.
      /// <e:pred name="TopOfTree"><code>root</code>, <code>nv</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:pred>

      // <m:existsIntro/> on <code>nv</code> as <e:var n="v"/>.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/></e:fst>
      ///   <e:snd>
      ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:pred>
      ///   </e:snd>
      /// </e:exists>

      // Close <e:pred name="NonEmptyTree"><code>root</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:pred>.
      /// <e:pred name="NonEmptyTree"><code>root</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:pred>

      // Weakening
      /// <e:pred name="Tree"><code>root</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:pred>

      o = root;
      // Assignment.
      /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:pred>
    }
  // If-rule.
  /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:pred>
  }
  // If-rule.  Postcondition.
  /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><e:var n="v"/></e:set></e:snd></e:setminus></e:pred>

  return root;
}


// [snip:start]
unittest {
  auto a = new Node(5);
  a = removeRoot(a);
  assert(a == null);
}


unittest {
  auto a = new Node(5);
  a.c[0] = new Node(4);
  a = removeRoot(a);
  assert(a);
  assert(a.value == 4);
  assert(!a.c[0]);
  assert(!a.c[1]);
}


unittest {
  auto a = new Node(5);
  a.c[0] = new Node(4);
  a.c[1] = new Node(6);
  a = removeRoot(a);

  assert(a);
  assert(a.value == 4);
  assert(a.c[0] == null);
  assert(a.c[1]);
  assert(a.c[1].value == 6);
  assert(!a.c[1].c[0]);
  assert(!a.c[1].c[1]);
}
// [snip:stop]