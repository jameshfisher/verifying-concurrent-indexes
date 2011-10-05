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
    /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus></e:pred>
  }
  else {
    // Deny if-condition.  Lemma: .
    /// <e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred>

    // Open <e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred>.
    /// <e:exists>
    ///   <e:var n="v"/>
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
    ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
    ///   </e:indent></e:expr>
    /// </e:exists>

    if (value == root.value) {
      // Assert if-condition.  Substitution.
      /// <e:exists>
      ///   <e:vars><e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
      ///   <e:expr><e:indent>
      ///     <e:sep>
      ///       <e:fcell>
      ///             <code>root</code>
      ///             <e:list><code>value</code>,<e:var n="l"/>,<e:var n="r"/></e:list>
      ///           </e:fcell>
      ///       <e:sep>
      ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
      ///             <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
      ///       </e:sep>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="TreeCompose"><e:st n="L"/>, <code>value</code>, <e:st n="R"/>, <e:st n="S"/></e:pred>
      ///   </e:indent></e:expr>
      /// </e:exists>

      // Close <e:pred name="TopOfTree"><code>root</code>, <code>value</code>, <e:st n="S"/></e:pred>.
      /// <e:pred name="TopOfTree"><code>root</code>, <code>value</code>, <e:st n="S"/></e:pred>.

      o = removeRoot(root);
      // Specification for <code>removeRoot</code>.
      /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus></e:pred>
    }
    else {
      // Deny if-condition.
      /// <e:exists>
      ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
      ///   <e:expr><e:indent>
      ///     <e:sep>
      ///       <e:fcell>
      ///             <code>root</code>
      ///             <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
      ///           </e:fcell>
      ///       <e:sep>
      ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
      ///             <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
      ///       </e:sep>
      ///     </e:sep> ∧<br />
      ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
      ///     <e:noteq><e:var n="v"/><code>value</code></e:noteq>
      ///   </e:indent></e:expr>
      /// </e:exists>

      if (value < root.value) {
        // Assert if-condition.
        /// <e:exists>
        ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
        ///   <e:expr><e:indent>
        ///     <e:sep>
        ///       <e:fcell>
        ///             <code>root</code>
        ///             <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
        ///           </e:fcell>
        ///       <e:sep>
        ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
        ///             <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
        ///       </e:sep>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
        ///     <e:lt><code>value</code><e:var n="v"/></e:lt>
        ///   </e:indent></e:expr>
        /// </e:exists>

        o.c[0] = remove(o.c[0], value);
        // Specification for <code>remove</code>.  Assignment.  <m:existsIntro/>.
        /// <e:exists>
        ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
        ///   <e:expr><e:indent>
        ///     <e:sep>
        ///       <e:fcell>
        ///             <code>root</code>
        ///             <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
        ///           </e:fcell>
        ///       <e:sep>
        ///             <e:pred name="Tree"><e:var n="l"/>, <e:setminus><e:st n="L"/><e:set><code>value</code></e:set></e:setminus></e:pred>
        ///             <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
        ///       </e:sep>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧
        ///     <e:lt><code>value</code><e:var n="v"/></e:lt>
        ///   </e:indent></e:expr>
        /// </e:exists>

        // Lemma: <e:logimpl>
        //   <e:and>
        //     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
        //     <e:lt><code>value</code><e:var n="v"/></e:lt>
        //   </e:and>
        //   <e:pred name="TreeCompose"><e:setminus><e:st n="L"/><e:set><code>value</code></e:set></e:setminus>, <e:var n="v"/>, <e:st n="R"/>, <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus></e:pred>
        // </e:logimpl>.<br />
        // Discard <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
        // and <e:lt><code>value</code><e:var n="v"/></e:lt>.
        /// <e:exists>
        ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
        ///   <e:expr><e:indent>
        ///     <e:sep>
        ///       <e:fcell>
        ///             <code>root</code>
        ///             <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
        ///           </e:fcell>
        ///       <e:sep>
        ///             <e:pred name="Tree"><e:var n="l"/>, <e:setminus><e:st n="L"/><e:set><code>value</code></e:set></e:setminus></e:pred>
        ///             <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
        ///       </e:sep>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TreeCompose"><e:setminus><e:st n="L"/><e:set><code>value</code></e:set></e:setminus>, <e:var n="v"/>, <e:st n="R"/>, <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus></e:pred>
        ///   </e:indent></e:expr>
        /// </e:exists>
        
        // <m:existsIntro/> on <e:setminus><e:st n="L"/><e:set><code>value</code></e:set></e:setminus> as <e:st n="L"/>.
        /// <e:exists>
        ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
        ///   <e:expr><e:indent>
        ///     <e:sep>
        ///       <e:fcell>
        ///             <code>root</code>
        ///             <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
        ///           </e:fcell>
        ///       <e:sep>
        ///             <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
        ///             <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
        ///       </e:sep>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus></e:pred>
        ///   </e:indent></e:expr>
        /// </e:exists>

        // Close <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus></e:pred>.
        /// <e:exists>
        ///   <e:vars><e:var n="v"/></e:vars>
        ///   <e:expr>
        ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus></e:pred>
        ///   </e:expr>
        /// </e:exists>
        
        // Close <e:pred name="NonEmptyTree"><code>root</code>, <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus></e:pred>.
        /// <e:pred name="NonEmptyTree"><code>root</code>, <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus></e:pred>

        // Weakening
        /// <e:pred name="Tree"><code>root</code>, <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus></e:pred>
      }
      else {
        // Symmetrical…
        o.c[1] = remove(o.c[1], value);
        /// <e:pred name="Tree"><code>root</code>, <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus></e:pred>
      }
      // If-rule.
      /// <e:pred name="Tree"><code>root</code>, <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus></e:pred>

      o = root;
      // Assignment.
      /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus></e:pred>
    }
    // If-rule.
    /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus></e:pred>
  }
  // If-rule.  Function postcondition.
  /// <e:pred name="Tree"><code>o</code>, <e:setminus><e:st n="S"/><e:set><code>value</code></e:set></e:setminus></e:pred>

  return o;
}