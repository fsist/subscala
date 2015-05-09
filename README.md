# subscala

A Scala library that restricts the syntax, types used, or methods called by the code passed to it.

This project is not yet production ready, and so artifacts are not yet published.
    
Use cases include:
 
 * Defining a DSL as a subset of the Scala language (i.e. without a separate parser). 
 * Restricting scripts from accessing anything outside a set of interfaces meant for scripting a system.
 * Enforcing rules in a codebase such as not using a particular interface which cannot be marked @deprecated.

For usage examples, see the test code.
