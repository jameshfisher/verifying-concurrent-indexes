module bst.insert.recursive;

import bst.node;

Node* insert(Node* root, int value) {
  Node* o;
  // Function precondition.
  /// <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
  if (root == null) {
    // Assert if-condition.
    /// <e:and>
    ///   <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:eq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:eq></e:snd>
    /// </e:and>

    // Lemma: <e:logimpl>
    //   <e:fst><e:and>
    //     <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    //     <e:snd><e:eq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:eq></e:snd>
    //   </e:and></e:fst>
    //   <e:snd><e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred></e:snd>
    // </e:logimpl>
    /// <e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred>

    // Lemma: <e:logimpl>
    //   <e:fst><e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    //   <e:snd><e:eq><e:fst><e:st n="S"/></e:fst><e:snd><m:scemp/></e:snd></e:eq></e:snd>
    // </e:logimpl>
    /// <e:and>
    ///   <e:fst><e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:eq><e:fst><e:st n="S"/></e:fst><e:snd><m:scemp/></e:snd></e:eq></e:snd>
    /// </e:and>

    o = new Node(value);
    /// <e:pred name="NonEmptyTree"><code>root</code>, <e:union><e:fst><e:st n="S" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred>
  }
  else {
    // Deny if-condition.
    /// <e:and>
    ///   <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    ///   <e:snd><e:noteq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:noteq></e:snd>
    /// </e:and>

    // Lemma: <e:logimpl>
    //   <e:fst><e:and>
    //     <e:fst><e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred></e:fst>
    //     <e:snd><e:noteq><e:fst><code>root</code></e:fst><e:snd><code>null</code></e:snd></e:noteq></e:snd>
    //   </e:and></e:fst>
    //   <e:snd><e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred></e:snd>
    // </e:logimpl>
    /// <e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred>

    bool eq = rootEq(root, value);
    // Specification for <code>rootEq</code>.
    /// <e:exists>
    ///   <e:fst><e:var n="v"/></e:fst>
    ///   <e:snd><e:and>
    ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
    ///     <e:snd><e:doubleimpl>
    ///       <e:fst><code>eq</code></e:fst>
    ///       <e:snd><e:eq>
    ///         <e:fst><e:var n="v"/></e:fst>
    ///         <e:snd><code>value</code></e:snd>
    ///       </e:eq></e:snd>
    ///     </e:doubleimpl></e:snd>
    ///   </e:and></e:snd>
    /// </e:exists>

    if (eq) {
      // Assert if-test.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/></e:fst>
      ///   <e:snd><e:and>
      ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
      ///     <e:snd><e:doubleimpl>
      ///       <e:fst><code>eq</code></e:fst>
      ///       <e:snd><e:eq>
      ///         <e:fst><e:var n="v"/></e:fst>
      ///         <e:snd><code>value</code></e:snd>
      ///       </e:eq></e:snd>
      ///     </e:doubleimpl></e:snd>
      ///   </e:and></e:snd>
      /// </e:exists> ∧ <code>eq</code>

      // Use <e:doubleimpl>
      //   <e:fst><code>eq</code></e:fst>
      //   <e:snd><e:eq>
      //     <e:fst><e:var n="v"/></e:fst>
      //     <e:snd><code>value</code></e:snd>
      //   </e:eq></e:snd>
      // </e:doubleimpl>.  Discard <code>eq</code>.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/></e:fst>
      ///   <e:snd><e:and>
      ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
      ///     <e:snd><e:eq>
      ///         <e:fst><e:var n="v"/></e:fst>
      ///         <e:snd><code>value</code></e:snd>
      ///       </e:eq></e:snd>
      ///   </e:and></e:snd>
      /// </e:exists>

      // Substitution.
      /// <e:pred name="TopOfTree"><code>root</code>, <code>value</code>, <e:st n="S"/></e:pred>

      o = root;
      /// <e:pred name="NonEmptyTree"><code>root</code>, <e:union><e:fst><e:st n="S" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred>
    }
    else {
      // Deny if-condition.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/></e:fst>
      ///   <e:snd><e:and>
      ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
      ///     <e:snd><e:doubleimpl>
      ///       <e:fst><code>eq</code></e:fst>
      ///       <e:snd><e:eq>
      ///         <e:fst><e:var n="v"/></e:fst>
      ///         <e:snd><code>value</code></e:snd>
      ///       </e:eq></e:snd>
      ///     </e:doubleimpl></e:snd>
      ///   </e:and></e:snd>
      /// </e:exists> ∧ ¬<code>eq</code>

      // Use <e:impl>
      //   <e:fst><code>eq</code></e:fst>
      //   <e:snd>(<e:eq>
      //     <e:fst><e:var n="v"/></e:fst>
      //     <e:snd><code>value</code></e:snd>
      //   </e:eq>)</e:snd>
      // </e:impl>.  Discard ¬<code>eq</code>.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/></e:fst>
      ///   <e:snd><e:and>
      ///     <e:fst><e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred></e:fst>
      ///     <e:snd><e:noteq>
      ///         <e:fst><e:var n="v"/></e:fst>
      ///         <e:snd><code>value</code></e:snd>
      ///       </e:noteq></e:snd>
      ///   </e:and></e:snd>
      /// </e:exists>

      // Open <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>.
      /// <e:exists>
      ///   <e:fst><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:fst>
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
      ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
      ///     <e:noteq>
      ///       <e:fst><e:var n="v"/></e:fst>
      ///       <e:snd><code>value</code></e:snd>
      ///     </e:noteq>
      ///   </e:indent></e:snd>
      /// </e:exists>
      if (value < root.value) {
        // Assert if-condition.
        /// <e:exists>
        ///   <e:fst><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:fst>
        ///   <e:snd><e:indent>
        ///     <e:sep>
        ///       <e:fst><e:fcell>
        ///           <e:fst><code>root</code></e:fst>
        ///           <e:snd><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:snd>
        ///         </e:fcell></e:fst>
        ///       <e:snd><e:sep>
        ///           <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred></e:fst>
        ///           <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
        ///       </e:sep></e:snd>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:lt>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:var n="v"/></e:snd>
        ///     </e:lt>
        ///   </e:indent></e:snd>
        /// </e:exists>
        Node* left = root.c[0];
        // <m:existsElim/> of <e:var n="l"/> as <code>left</code>.
        /// <e:exists>
        ///   <e:fst><e:var n="v"/>, <e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:fst>
        ///   <e:snd><e:indent>
        ///     <e:sep>
        ///       <e:fst><e:fcell>
        ///           <e:fst><code>root</code></e:fst>
        ///           <e:snd><e:var n="v"/>,<code>left</code>,<e:var n="r"/></e:snd>
        ///         </e:fcell></e:fst>
        ///       <e:snd><e:sep>
        ///           <e:fst><e:pred name="Tree"><code>left</code>, <e:st n="L"/></e:pred></e:fst>
        ///           <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
        ///       </e:sep></e:snd>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:lt>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:var n="v"/></e:snd>
        ///     </e:lt>
        ///   </e:indent></e:snd>
        /// </e:exists>
        Node* nleft = insert(left, value);
        // Specification for <code>insert</code>.
        /// <e:exists>
        ///   <e:fst><e:var n="v"/>, <e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:fst>
        ///   <e:snd><e:indent>
        ///     <e:sep>
        ///       <e:fst><e:fcell>
        ///           <e:fst><code>root</code></e:fst>
        ///           <e:snd><e:var n="v"/>,<code>left</code>,<e:var n="r"/></e:snd>
        ///         </e:fcell></e:fst>
        ///       <e:snd><e:sep>
        ///           <e:fst><e:pred name="Tree"><code>nleft</code>, <e:union><e:fst><e:st n="L" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred></e:fst>
        ///           <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
        ///       </e:sep></e:snd>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:lt>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:var n="v"/></e:snd>
        ///     </e:lt>
        ///   </e:indent></e:snd>
        /// </e:exists>
        root.c[0] = nleft;
        // Assignment.
        /// <e:exists>
        ///   <e:fst><e:var n="v"/>, <e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:fst>
        ///   <e:snd><e:indent>
        ///     <e:sep>
        ///       <e:fst><e:fcell>
        ///           <e:fst><code>root</code></e:fst>
        ///           <e:snd><e:var n="v"/>,<code>nleft</code>, <e:var n="r"/></e:snd>
        ///         </e:fcell></e:fst>
        ///       <e:snd><e:sep>
        ///           <e:fst><e:pred name="Tree"><code>nleft</code>, <e:union><e:fst><e:st n="L" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred></e:fst>
        ///           <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
        ///       </e:sep></e:snd>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:lt>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:var n="v"/></e:snd>
        ///     </e:lt>
        ///   </e:indent></e:snd>
        /// </e:exists>

        // <m:existsIntro/> on <code>nleft</code> as <e:var n="l"/>.
        /// <e:exists>
        ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
        ///   <e:snd><e:indent>
        ///     <e:sep>
        ///       <e:fst><e:fcell>
        ///           <e:fst><code>root</code></e:fst>
        ///           <e:snd><e:var n="v"/>,<e:var n="l"/>, <e:var n="r"/></e:snd>
        ///         </e:fcell></e:fst>
        ///       <e:snd><e:sep>
        ///           <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:union><e:fst><e:st n="L" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred></e:fst>
        ///           <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
        ///       </e:sep></e:snd>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:lt>
        ///       <e:fst><code>value</code></e:fst>
        ///       <e:snd><e:var n="v"/></e:snd>
        ///     </e:lt>
        ///   </e:indent></e:snd>
        /// </e:exists>

        // Lemma: <e:logimpl>
        //  <e:fst><e:and>
        //    <e:fst><e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred></e:fst>
        //    <e:snd><e:lt><e:fst><code>value</code></e:fst><e:snd><e:var n="v"/></e:snd></e:lt></e:snd>
        //  </e:and></e:fst>
        //  <e:snd><e:pred name="TreeCompose">
        //    <e:union><e:fst><e:st n="L" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>,
        //    <e:var n="v"/>, <e:st n="R"/>,
        //    <e:union><e:fst><e:st n="S" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>
        //  </e:pred></e:snd>
        // </e:logimpl>.<br />
        // Discard <e:pred name="TreeCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> and
        // <e:lt><e:fst><code>value</code></e:fst><e:snd><e:var n="v"/></e:snd></e:lt>.
        /// <e:exists>
        ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
        ///   <e:snd><e:indent>
        ///     <e:sep>
        ///       <e:fst><e:fcell>
        ///           <e:fst><code>root</code></e:fst>
        ///           <e:snd><e:var n="v"/>,<e:var n="l"/>, <e:var n="r"/></e:snd>
        ///         </e:fcell></e:fst>
        ///       <e:snd><e:sep>
        ///           <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:union><e:fst><e:st n="L" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred></e:fst>
        ///           <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
        ///       </e:sep></e:snd>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TreeCompose">
        ///       <e:union><e:fst><e:st n="L" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>,
        ///       <e:var n="v"/>, <e:st n="R"/>,
        ///       <e:union><e:fst><e:st n="S" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>
        ///     </e:pred>
        ///   </e:indent></e:snd>
        /// </e:exists>

        // <m:existsIntro/> on <e:union><e:fst><e:st n="L" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union> as <e:st n="L" />.
        /// <e:exists>
        ///   <e:fst><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:fst>
        ///   <e:snd><e:indent>
        ///     <e:sep>
        ///       <e:fst><e:fcell>
        ///           <e:fst><code>root</code></e:fst>
        ///           <e:snd><e:var n="v"/>,<e:var n="l"/>, <e:var n="r"/></e:snd>
        ///         </e:fcell></e:fst>
        ///       <e:snd><e:sep>
        ///           <e:fst><e:pred name="Tree"><e:var n="l"/>, <e:st n="L" /></e:pred></e:fst>
        ///           <e:snd><e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred></e:snd>
        ///       </e:sep></e:snd>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TreeCompose">
        ///       <e:st n="L" />,
        ///       <e:var n="v"/>, <e:st n="R"/>,
        ///       <e:union><e:fst><e:st n="S" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union>
        ///     </e:pred>
        ///   </e:indent></e:snd>
        /// </e:exists>

        // Close <e:pred name="NonEmptyTree"><code>root</code>, <e:union><e:fst><e:st n="S" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred>.
        /// <e:pred name="NonEmptyTree"><code>root</code>, <e:union><e:fst><e:st n="S" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred>
      }
      else {
        // (Symmetrical case…)
        root.c[1] = insert(root.c[1], value);
        /// <e:pred name="NonEmptyTree"><code>root</code>, <e:union><e:fst><e:st n="S" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred>
      }
      // If-rule.
      /// <e:pred name="NonEmptyTree"><code>root</code>, <e:union><e:fst><e:st n="S" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred>
      o = root;
      // Assignment.
      /// <e:pred name="NonEmptyTree"><code>o</code>, <e:union><e:fst><e:st n="S" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred>
    }
    // If-rule.
    /// <e:pred name="NonEmptyTree"><code>o</code>, <e:union><e:fst><e:st n="S" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred>
  }
  // If-rule.
  /// <e:pred name="NonEmptyTree"><code>o</code>, <e:union><e:fst><e:st n="S" /></e:fst><e:snd><e:set><code>value</code></e:set></e:snd></e:union></e:pred>
  return o;
}