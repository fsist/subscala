package com.fsist.subscala

import com.fsist.subscala.CallTargets.+
import shapeless.test.illTyped

object CallTargetsTest {
  // This file doesn't contain any runtime tests; the tests are done at compile time.

  def allMethodsOf = {
    Restrict[String, Syntax.All, CallTargets.AllMethodsOf[String]] {
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
}
