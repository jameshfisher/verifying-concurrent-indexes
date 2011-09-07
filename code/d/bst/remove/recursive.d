module bst.remove.recursive;

import bst.node;
import bst.remove.removeRoot;


Node* remove(Node* root, int value) {
  Node* o;
  // Function precondition.
  /// <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
  
  if (root == null) {
    // Assert if-condition.  Lemma: .
    /// <e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred>
    o = root;
    /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred>
  }
  else {
    // Deny if-condition.  Lemma: .
    /// <e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred>

    // Open <e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/></e:fst>
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
    ///     <e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:snd>
    /// </e:exists>

    if (value == root.value) {
      // Assert if-condition.  Substitution.
      /// <e:exists>
      ///   <e:fst><e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
      ///   <e:snd><e:indent>
      ///     <e:sep>
      ///       <e:fst><e:fcell>
      ///             <e:fst><code>root</code></e:fst>
      ///             <e:snd><code>value</code>,<e:var n="l"/>,<e:var n="r"/></e:snd>
      ///           </e:fcell></e:fst>
      ///       <e:snd><e:sep>
      ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
      ///             <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
      ///       </e:sep></e:snd>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="Compose"><e:st n="L"/>, <code>value</code>, <e:st n="R"/>, <e:st n="S"/></e:pred>
      ///   </e:indent></e:snd>
      /// </e:exists>

      // Close <e:pred name="TopOfTree"><code>root</code>, <code>value</code>, <e:st n="S"/></e:pred>.
      /// <e:pred name="TopOfTree"><code>root</code>, <code>value</code>, <e:st n="S"/></e:pred>.

      o = removeRoot(root);
      // Specification for <code>removeRoot</code>.
      /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred>
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
      ///     <e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
      ///     <e:noteq><e:fst><e:var n="v"/></e:fst><e:snd><code>value</code></e:snd></e:noteq>
      ///   </e:indent></e:snd>
      /// </e:exists>

      if (value < root.value) {
        // Assert if-condition.
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
        ///     <e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
        ///     <e:lt><e:fst><code>value</code></e:fst><e:snd><e:var n="v"/></e:snd></e:lt>
        ///   </e:indent></e:snd>
        /// </e:exists>

        o.c[0] = remove(o.c[0], value);
        // Specification for <code>remove</code>.  Assignment.  <m:existsIntro/>.
        /// <e:exists>
        ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
        ///   <e:snd><e:indent>
        ///     <e:sep>
        ///       <e:fst><e:fcell>
        ///             <e:fst><code>root</code></e:fst>
        ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
        ///           </e:fcell></e:fst>
        ///       <e:snd><e:sep>
        ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:setminus><e:fst><e:st n="L"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred></e:fst>
        ///             <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
        ///       </e:sep></e:snd>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
        ///     <e:lt><e:fst><code>value</code></e:fst><e:snd><e:var n="v"/></e:snd></e:lt>
        ///   </e:indent></e:snd>
        /// </e:exists>

        // Lemma: <e:logimpl>
        //   <e:fst><e:and>
        //     <e:fst><e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred></e:fst>
        //     <e:snd><e:lt><e:fst><code>value</code></e:fst><e:snd><e:var n="v"/></e:snd></e:lt></e:snd>
        //   </e:and></e:fst>
        //   <e:snd><e:pred name="Compose"><e:setminus><e:fst><e:st n="L"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus>, <e:var n="v"/>, <e:st n="R"/>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred></e:snd>
        // </e:logimpl>.<br />
        // Discard <e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
        // and <e:lt><e:fst><code>value</code></e:fst><e:snd><e:var n="v"/></e:snd></e:lt>.
        /// <e:exists>
        ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
        ///   <e:snd><e:indent>
        ///     <e:sep>
        ///       <e:fst><e:fcell>
        ///             <e:fst><code>root</code></e:fst>
        ///             <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
        ///           </e:fcell></e:fst>
        ///       <e:snd><e:sep>
        ///             <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:setminus><e:fst><e:st n="L"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred></e:fst>
        ///             <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
        ///       </e:sep></e:snd>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="Compose"><e:setminus><e:fst><e:st n="L"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus>, <e:var n="v"/>, <e:st n="R"/>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred>
        ///   </e:indent></e:snd>
        /// </e:exists>
        
        // <m:existsIntro/> on <e:setminus><e:fst><e:st n="L"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus> as <e:st n="L"/>.
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
        ///     <e:pred name="Compose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred>
        ///   </e:indent></e:snd>
        /// </e:exists>

        // Close <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred>.
        /// <e:exists>
        ///   <e:fst><e:var n="v"/></e:fst>
        ///   <e:snd>
        ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred>
        ///   </e:snd>
        /// </e:exists>
        
        // Close <e:pred name="NonEmptyTree"><code>root</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred>.
        /// <e:pred name="NonEmptyTree"><code>root</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred>

        // Weakening
        /// <e:pred name="Tree"><code>root</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred>
      }
      else {
        // Symmetrical…
        o.c[1] = remove(o.c[1], value);
        /// <e:pred name="Tree"><code>root</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred>
      }
      // If-rule.
      /// <e:pred name="Tree"><code>root</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred>

      o = root;
      // Assignment.
      /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred>
    }
    // If-rule.
    /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred>
  }
  // If-rule.  Function postcondition.
  /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:fst><e:st n="S"/></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:setminus></e:pred>

  return o;
}