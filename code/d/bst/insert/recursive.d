module bst.insert.recursive;

import bst.node;

Node* insert(Node* root, int value) {
  Node* o;
  // Function precondition.
  /// <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
  if (root == null) {
    // Assert if-condition.
    /// <e:and>
    ///   <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
    ///   <e:eq><code>root</code><code>null</code></e:eq>
    /// </e:and>

    // Lemma: <e:logimpl>
    //   <e:and>
    //     <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
    //     <e:eq><code>root</code><code>null</code></e:eq>
    //   </e:and>
    //   <e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred>
    // </e:logimpl>
    /// <e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred>

    // Lemma: <e:logimpl>
    //   <e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred>
    //   <e:eq><e:st n="S"/><m:scemp/></e:eq>
    // </e:logimpl>
    /// <e:and>
    ///   <e:pred name="EmptyTree"><code>root</code>, <e:st n="S"/></e:pred>
    ///   <e:eq><e:st n="S"/><m:scemp/></e:eq>
    /// </e:and>

    o = new Node(value);
    /// <e:pred name="NonEmptyTree"><code>root</code>, <e:union><e:st n="S" /><e:set><code>value</code></e:set></e:union></e:pred>
  }
  else {
    // Deny if-condition.
    /// <e:and>
    ///   <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
    ///   <e:noteq><code>root</code><code>null</code></e:noteq>
    /// </e:and>

    // Lemma: <e:logimpl>
    //   <e:and>
    //     <e:pred name="Tree"><code>root</code>, <e:st n="S"/></e:pred>
    //     <e:noteq><code>root</code><code>null</code></e:noteq>
    //   </e:and>
    //   <e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred>
    // </e:logimpl>
    /// <e:pred name="NonEmptyTree"><code>root</code>, <e:st n="S"/></e:pred>

    bool eq = rootEq(root, value);
    // Specification for <code>rootEq</code>.
    /// <e:exists>
    ///   <e:vars><e:var n="v"/></e:vars>
    ///   <e:expr><e:and>
    ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>
    ///     <e:doubleimpl>
    ///       <code>eq</code>
    ///       <e:eq>
    ///         <e:var n="v"/>
    ///         <code>value</code>
    ///       </e:eq>
    ///     </e:doubleimpl>
    ///   </e:and></e:expr>
    /// </e:exists>

    if (eq) {
      // Assert if-test.
      /// <e:exists>
      ///   <e:vars><e:var n="v"/></e:vars>
      ///   <e:expr><e:and>
      ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>
      ///     <e:doubleimpl>
      ///       <code>eq</code>
      ///       <e:eq>
      ///         <e:var n="v"/>
      ///         <code>value</code>
      ///       </e:eq>
      ///     </e:doubleimpl>
      ///   </e:and></e:expr>
      /// </e:exists> ∧ <code>eq</code>

      // Use <e:doubleimpl>
      //   <code>eq</code>
      //   <e:eq>
      //     <e:var n="v"/>
      //     <code>value</code>
      //   </e:eq>
      // </e:doubleimpl>.  Discard <code>eq</code>.
      /// <e:exists>
      ///   <e:vars><e:var n="v"/></e:vars>
      ///   <e:expr><e:and>
      ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>
      ///     <e:eq>
      ///         <e:var n="v"/>
      ///         <code>value</code>
      ///       </e:eq>
      ///   </e:and></e:expr>
      /// </e:exists>

      // Substitution.
      /// <e:pred name="TopOfTree"><code>root</code>, <code>value</code>, <e:st n="S"/></e:pred>

      o = root;
      /// <e:pred name="NonEmptyTree"><code>root</code>, <e:union><e:st n="S" /><e:set><code>value</code></e:set></e:union></e:pred>
    }
    else {
      // Deny if-condition.
      /// <e:exists>
      ///   <e:vars><e:var n="v"/></e:vars>
      ///   <e:expr><e:and>
      ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>
      ///     <e:doubleimpl>
      ///       <code>eq</code>
      ///       <e:eq>
      ///         <e:var n="v"/>
      ///         <code>value</code>
      ///       </e:eq>
      ///     </e:doubleimpl>
      ///   </e:and></e:expr>
      /// </e:exists> ∧ ¬<code>eq</code>

      // Use <e:impl>
      //   <code>eq</code>
      //   (<e:eq>
      //     <e:var n="v"/>
      //     <code>value</code>
      //   </e:eq>)
      // </e:impl>.  Discard ¬<code>eq</code>.
      /// <e:exists>
      ///   <e:vars><e:var n="v"/></e:vars>
      ///   <e:expr><e:and>
      ///     <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>
      ///     <e:noteq>
      ///         <e:var n="v"/>
      ///         <code>value</code>
      ///       </e:noteq>
      ///   </e:and></e:expr>
      /// </e:exists>

      // Open <e:pred name="TopOfTree"><code>root</code>, <e:var n="v"/>, <e:st n="S"/></e:pred>.
      /// <e:exists>
      ///   <e:vars><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:vars>
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
      ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
      ///     <e:noteq>
      ///       <e:var n="v"/>
      ///       <code>value</code>
      ///     </e:noteq>
      ///   </e:indent></e:expr>
      /// </e:exists>
      if (value < root.value) {
        // Assert if-condition.
        /// <e:exists>
        ///   <e:vars><e:var n="v"/>, <e:var n="l"/>,<e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:vars>
        ///   <e:expr><e:indent>
        ///     <e:sep>
        ///       <e:fcell>
        ///           <code>root</code>
        ///           <e:list><e:var n="v"/>,<e:var n="l"/>,<e:var n="r"/></e:list>
        ///         </e:fcell>
        ///       <e:sep>
        ///           <e:pred name="Tree"><e:var n="l"/>, <e:st n="L"/></e:pred>
        ///           <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
        ///       </e:sep>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:lt>
        ///       <code>value</code>
        ///       <e:var n="v"/>
        ///     </e:lt>
        ///   </e:indent></e:expr>
        /// </e:exists>
        Node* left = root.c[0];
        // <m:existsElim/> of <e:var n="l"/> as <code>left</code>.
        /// <e:exists>
        ///   <e:vars><e:var n="v"/>, <e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:vars>
        ///   <e:expr><e:indent>
        ///     <e:sep>
        ///       <e:fcell>
        ///           <code>root</code>
        ///           <e:list><e:var n="v"/>,<code>left</code>,<e:var n="r"/></e:list>
        ///         </e:fcell>
        ///       <e:sep>
        ///           <e:pred name="Tree"><code>left</code>, <e:st n="L"/></e:pred>
        ///           <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
        ///       </e:sep>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:lt>
        ///       <code>value</code>
        ///       <e:var n="v"/>
        ///     </e:lt>
        ///   </e:indent></e:expr>
        /// </e:exists>
        Node* nleft = insert(left, value);
        // Specification for <code>insert</code>.
        /// <e:exists>
        ///   <e:vars><e:var n="v"/>, <e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:vars>
        ///   <e:expr><e:indent>
        ///     <e:sep>
        ///       <e:fcell>
        ///           <code>root</code>
        ///           <e:list><e:var n="v"/>,<code>left</code>,<e:var n="r"/></e:list>
        ///         </e:fcell>
        ///       <e:sep>
        ///           <e:pred name="Tree"><code>nleft</code>, <e:union><e:st n="L" /><e:set><code>value</code></e:set></e:union></e:pred>
        ///           <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
        ///       </e:sep>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:lt>
        ///       <code>value</code>
        ///       <e:var n="v"/>
        ///     </e:lt>
        ///   </e:indent></e:expr>
        /// </e:exists>
        root.c[0] = nleft;
        // Assignment.
        /// <e:exists>
        ///   <e:vars><e:var n="v"/>, <e:var n="r"/>,<e:st n="L"/>,<e:st n="R"/></e:vars>
        ///   <e:expr><e:indent>
        ///     <e:sep>
        ///       <e:fcell>
        ///           <code>root</code>
        ///           <e:list><e:var n="v"/>,<code>nleft</code>, <e:var n="r"/></e:list>
        ///         </e:fcell>
        ///       <e:sep>
        ///           <e:pred name="Tree"><code>nleft</code>, <e:union><e:st n="L" /><e:set><code>value</code></e:set></e:union></e:pred>
        ///           <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
        ///       </e:sep>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:lt>
        ///       <code>value</code>
        ///       <e:var n="v"/>
        ///     </e:lt>
        ///   </e:indent></e:expr>
        /// </e:exists>

        // <m:existsIntro/> on <code>nleft</code> as <e:var n="l"/>.
        /// <e:exists>
        ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
        ///   <e:expr><e:indent>
        ///     <e:sep>
        ///       <e:fcell>
        ///           <code>root</code>
        ///           <e:list><e:var n="v"/>,<e:var n="l"/>, <e:var n="r"/></e:list>
        ///         </e:fcell>
        ///       <e:sep>
        ///           <e:pred name="Tree"><e:var n="l"/>, <e:union><e:st n="L" /><e:set><code>value</code></e:set></e:union></e:pred>
        ///           <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
        ///       </e:sep>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> ∧<br />
        ///     <e:lt>
        ///       <code>value</code>
        ///       <e:var n="v"/>
        ///     </e:lt>
        ///   </e:indent></e:expr>
        /// </e:exists>

        // Lemma: <e:logimpl>
        //  <e:and>
        //    <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred>
        //    <e:lt><code>value</code><e:var n="v"/></e:lt>
        //  </e:and>
        //  <e:pred name="TCompose">
        //    <e:union><e:st n="L" /><e:set><code>value</code></e:set></e:union>,
        //    <e:var n="v"/>, <e:st n="R"/>,
        //    <e:union><e:st n="S" /><e:set><code>value</code></e:set></e:union>
        //  </e:pred>
        // </e:logimpl>.<br />
        // Discard <e:pred name="TCompose"><e:st n="L"/>, <e:var n="v"/>, <e:st n="R"/>, <e:st n="S"/></e:pred> and
        // <e:lt><code>value</code><e:var n="v"/></e:lt>.
        /// <e:exists>
        ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
        ///   <e:expr><e:indent>
        ///     <e:sep>
        ///       <e:fcell>
        ///           <code>root</code>
        ///           <e:list><e:var n="v"/>,<e:var n="l"/>, <e:var n="r"/></e:list>
        ///         </e:fcell>
        ///       <e:sep>
        ///           <e:pred name="Tree"><e:var n="l"/>, <e:union><e:st n="L" /><e:set><code>value</code></e:set></e:union></e:pred>
        ///           <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
        ///       </e:sep>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TCompose">
        ///       <e:union><e:st n="L" /><e:set><code>value</code></e:set></e:union>,
        ///       <e:var n="v"/>, <e:st n="R"/>,
        ///       <e:union><e:st n="S" /><e:set><code>value</code></e:set></e:union>
        ///     </e:pred>
        ///   </e:indent></e:expr>
        /// </e:exists>

        // <m:existsIntro/> on <e:union><e:st n="L" /><e:set><code>value</code></e:set></e:union> as <e:st n="L" />.
        /// <e:exists>
        ///   <e:vars><e:var n="v"/>, <e:var n="l"/>, <e:var n="r"/>, <e:st n="L"/>, <e:st n="R"/></e:vars>
        ///   <e:expr><e:indent>
        ///     <e:sep>
        ///       <e:fcell>
        ///           <code>root</code>
        ///           <e:list><e:var n="v"/>,<e:var n="l"/>, <e:var n="r"/></e:list>
        ///         </e:fcell>
        ///       <e:sep>
        ///           <e:pred name="Tree"><e:var n="l"/>, <e:st n="L" /></e:pred>
        ///           <e:pred name="Tree"><e:var n="r"/>, <e:st n="R"/></e:pred>
        ///       </e:sep>
        ///     </e:sep> ∧<br />
        ///     <e:pred name="TCompose">
        ///       <e:st n="L" />,
        ///       <e:var n="v"/>, <e:st n="R"/>,
        ///       <e:union><e:st n="S" /><e:set><code>value</code></e:set></e:union>
        ///     </e:pred>
        ///   </e:indent></e:expr>
        /// </e:exists>

        // Close <e:pred name="NonEmptyTree"><code>root</code>, <e:union><e:st n="S" /><e:set><code>value</code></e:set></e:union></e:pred>.
        /// <e:pred name="NonEmptyTree"><code>root</code>, <e:union><e:st n="S" /><e:set><code>value</code></e:set></e:union></e:pred>
      }
      else {
        // (Symmetrical case…)
        root.c[1] = insert(root.c[1], value);
        /// <e:pred name="NonEmptyTree"><code>root</code>, <e:union><e:st n="S" /><e:set><code>value</code></e:set></e:union></e:pred>
      }
      // If-rule.
      /// <e:pred name="NonEmptyTree"><code>root</code>, <e:union><e:st n="S" /><e:set><code>value</code></e:set></e:union></e:pred>
      o = root;
      // Assignment.
      /// <e:pred name="NonEmptyTree"><code>o</code>, <e:union><e:st n="S" /><e:set><code>value</code></e:set></e:union></e:pred>
    }
    // If-rule.
    /// <e:pred name="NonEmptyTree"><code>o</code>, <e:union><e:st n="S" /><e:set><code>value</code></e:set></e:union></e:pred>
  }
  // If-rule.
  /// <e:pred name="NonEmptyTree"><code>o</code>, <e:union><e:st n="S" /><e:set><code>value</code></e:set></e:union></e:pred>
  return o;
}