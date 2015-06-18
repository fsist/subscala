package com.fsist.subscala

import com.fsist.subscala.CallTargets.{AllMethodsOf, +}
import shapeless.test.illTyped

object CallTargetsTest {
  // This file doesn't contain any runtime tests; the tests are done at compile time.

  def allMethodsOf = {
    Restrict.targets[String, CallTargets.AllMethodsOf[String]] {
      "foo".contains("f")
      "bar".indexOf("b")
      ""
    }

    illTyped(
      """
        Restrict[String, Syntax.All, CallTargets.AllMethodsOf[String]] {
          "foo".foreach(println) // foreach is a not method of String
          ""
        }
      """, "Calling method scala.collection.IndexedSeqOptimized.foreach is disallowed")
  }

  def oneMethod = {
    trait length extends CallTargets.MethodsOf[String] {
      def length(): Int
    }

    Restrict[String, Syntax.All, length] {
      "foo".length()
      ""
    }

    illTyped(
      """
        Restrict[String, Syntax.All, CallTargets.None] {
          "foo".equals("bar")
          ""
        }
      """, "Calling method java.lang.String.equals is disallowed"
    )
  }

  def methodWithParameters = {
    trait charAt extends CallTargets.MethodsOf[String] {
      def charAt(index: Int): Char
    }
    Restrict.targets[Char, charAt] {
      "foo".charAt(0)
    }
  }

  trait ObjectCtor extends CallTargets.MethodsOf[Object] {
    def `<init>`(): Unit
  }

  def defineAndCreateClass = {
    Restrict[String, Syntax.All, ObjectCtor + CallTargets.LocallyDefined] {
      class C
      new C()
      ""
    }

    illTyped(
      """
        Restrict[String, Syntax.All, CallTargets.LocallyDefined] {
          class C
          new C()
          ""
        }
      """, "Calling method java.lang.Object.<init> is disallowed"
    )

    illTyped(
      """
        Restrict[String, Syntax.All, ObjectCtor] {
          class C
          new C()
          ""
        }
      """, "Calling locally defined method com.fsist.subscala.CallTargetsTest.C.<init> is disallowed"
    )
  }

  def defineAndCallMethod = {
    Restrict[String, Syntax.All, CallTargets.LocallyDefined] {
      def foo(): Unit = ()
      foo()
      ""
    }

    illTyped(
      """
        Restrict[String, Syntax.All, CallTargets.None] {
          def foo(): Unit = ()
          foo()
          ""
        }
      """, "Calling locally defined method com.fsist.subscala.CallTargetsTest.foo is disallowed"
    )
  }

  def defineAndCallFunction = {
    // Here the actual call target is Function0.apply.
    // Note that the restrictions don't support generic specificity at the time of writing, so we write the generic type
    // Function0 and not the specific Function0[Unit].
    Restrict[String, Syntax.All, CallTargets.AllMethodsOf[Function0[Unit]]] {
      val foo = () => ()
      foo()
      ""
    }
  }

  def callMethodOfSubtype = {
    trait Parent {
      def foo(): Unit
    }
    trait Child extends Parent

    Restrict[Unit, Syntax.All, CallTargets.AllMethodsOf[Parent]] {
      val instance: Parent = ???
      instance.foo()
    }

    Restrict[Unit, Syntax.All, CallTargets.AllMethodsOf[Parent]] {
      val instance: Parent = ???
      instance.foo()
    }

  }

  def callMethodOfGenericType = {
    trait Generic[A, B, +R] {
      def method(x: A, y: B): R
    }

    trait Parent
    trait Child extends Parent

    // Explicit parent type
    Restrict[Parent, Syntax.All, CallTargets.AllMethodsOf[Generic[Int, Long, Parent]]] {
      val instance: Generic[Int, Long, Parent] = ???
      instance.method(1, 2L)
    }

    // Explicit child type
    Restrict[Parent, Syntax.All, CallTargets.AllMethodsOf[Generic[Int, Long, Child]]] {
      val instance: Generic[Int, Long, Child] = ???
      instance.method(1, 2L)
    }

    // Allow parent type, call child type
    Restrict[Parent, Syntax.All, CallTargets.AllMethodsOf[Generic[Int, Long, Parent]]] {
      val instance: Generic[Int, Long, Child] = ???
      instance.method(1, 2L)
    }

    illTyped(
      """
        Restrict[Parent, Syntax.All, CallTargets.AllMethodsOf[Generic[Int, Int, Parent]]] {
          val instance: Generic[Int, Long, Parent] = ???
          instance.method(1, 2L)
        }
      """, "Calling method com.fsist.subscala.CallTargetsTest.Generic.method is disallowed"
    )

    illTyped(
      """
        Restrict[Parent, Syntax.All, CallTargets.AllMethodsOf[Generic[Int, Long, Boolean]]] {
          val instance: Generic[Int, Long, Parent] = ???
          instance.method(1, 2L)
        }
      """, "Calling method com.fsist.subscala.CallTargetsTest.Generic.method is disallowed"
    )
  }

  def callGenericMethod = {
    trait Generic {
      def method[T](t: T): T
    }

    trait AllowT extends CallTargets.MethodsOf[Generic] {
      def method[T](t: T): T
    }

    trait AllowInt extends CallTargets.MethodsOf[Generic] {
      def method(t: Int): Int
    }

    trait AllowString extends CallTargets.MethodsOf[Generic] {
      def method(t: String): String
    }

    Restrict.targets[Int, AllowT] {
      val g: Generic = ???
      g.method("foo")
      g.method(1)
    }

    // This doesn't work because Reifier compares erased types, and the erased type of the actual method takes an Object,
    // while we permitted String. To enable this usage Reifier would have to compare actual, unerased types and compare
    // their type args directly, taking into account type bounds.

//    Restrict.targets[String, AllowString] {
//      val g: Generic = ???
//      g.method("foo")
//    }
//
//    illTyped(
//      """
//        Restrict.targets[Int, AllowInt] {
//          val g: Generic = ???
//          g.method(1)
//        }
//      """, "Calling method com.fsist.subscala.CallTargetsTest.Generic.method is disallowed"
//    )
  }

  def callWithVarargs = {
    trait Varargs {
      def method(strings: String*): Unit
    }

    trait Allow extends CallTargets.MethodsOf[Varargs] {
      def method(strings: String*): Unit
    }

    Restrict.targets[Unit, Allow] {
      val v: Varargs = ???
      v.method("a", "b")
    }

    trait AllowTwo extends CallTargets.MethodsOf[Varargs] {
      def method(a: String, b: String): Unit
    }

    illTyped(
      """
        Restrict.targets[Unit, AllowTwo] {
          val v: Varargs = ???
          v.method("a", "b")
        }
      """, "Calling method com.fsist.subscala.CallTargetsTest.Varargs.method is disallowed"
    )
  }

  def multipleParamLists = {
    trait Trait {
      def method(i: Int)(s: String): Int
    }

    trait Allow extends CallTargets.MethodsOf[Trait] {
      def method(i: Int)(s: String): Int
    }

    trait Partial extends CallTargets.MethodsOf[Trait] {
      def method(i: Int): String => Int
    }

    Restrict.targets[Int, Allow] {
      val t: Trait = ???
      t.method(1)("")
    }

    illTyped(
      """
        Restrict.targets[Int, Partial] {
          val t: Trait = ???
          t.method(1) _
          1
        }
      """, "Calling method com.fsist.subscala.CallTargetsTest.Trait.method is disallowed"
    )
  }

  def chainedCalls = {
    trait Trait {
      def method(i: Int): String => Int
    }

    trait Allow extends CallTargets.MethodsOf[Trait] {
      def method(i: Int): String => Int
    }

    trait AllowParamLists extends CallTargets.MethodsOf[Trait] {
      def method(i: Int)(s: String): Int
    }

    Restrict.targets[Int, Allow + AllMethodsOf[Function1[String, Int]]] {
      val t: Trait = ???
      t.method(1)("")
    }

    illTyped(
      """
        Restrict.targets[Int, AllowParamLists + AllMethodsOf[Function1[String, Int]]] {
          val t: Trait = ???
          t.method(1)("")
        }
      """, "Calling method com.fsist.subscala.CallTargetsTest.Trait.method is disallowed"
    )

    illTyped(
      """
        Restrict.targets[Int, Allow] {
          val t: Trait = ???
          t.method(1)("")
        }
      """, "Calling method scala.Function1.apply is disallowed"
    )
  }

  // TODO call method using argument subtype of expected parameter type
  // TODO call method with implicit params
  // TODO test overloading
}
