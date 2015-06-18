# subscala

A Scala library that restricts the syntax, types used, or methods called by the code passed to it.

## Short overview

`subscala.Restrict` is a macro that fails compilation if the code passed to it doesn't match the given restrictions.
It doesn't affect the result of the compilation if it succeeds.

It can restrict the methods that code may call:
 
```
object Test {
  import com.fsist.subscala._
  
  trait AllowLength extends CallTargets.MethodsOf[String] { def length(): Int }
   
  val len = Restrict.targets[Int, AllowLength] {
    "foo".length()
  }
}
```
  
This succeeds and `val len` receives the value 3. On the other hand, if we try this:

``` 
val size = Restrict.targets[Int, AllowLength] {
  "foo".size
}
```

Compilation will fail with `error: Calling method scala.Predef.augmentString is disallowed`. Let's add the missing
methods:

```
import CallTargets.+
import collection.immutable.StringOps

trait AllowAugment extends CallTargets.MethodsOf[Predef.type] { def augmentString(x: String): StringOps }
trait AllowSize extends CallTargets.MethodsOf[StringOps] { def size: Int }

val size = Restrict.targets[Int, AllowAugment + AllowSize] {
  "foo".size
}

```

This project is not yet production ready, and so artifacts are not yet published.
    
Use cases include:
 
 * Defining a DSL as a subset of the Scala language (i.e. without a separate parser). 
 * Restricting scripts from accessing anything outside a set of interfaces meant for scripting a system.
 * Enforcing rules in a codebase such as not using a particular interface which cannot be marked @deprecated.

For usage examples, see the test code.
