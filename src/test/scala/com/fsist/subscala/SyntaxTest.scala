package com.fsist.subscala

import shapeless.test.illTyped

import Syntax._

class SyntaxTest {
  // This file doesn't contain any runtime tests; the tests are done at compile time.

  def testIf = {
    val t1 = Restrict[Int, All - If, CallTargets.None] {
      1 + 1
    }

    illTyped(
      """Restrict[Int, All - If, CallTargets.None] {
          if (true) 1 else 2
        }
      """, "`if` expressions are disallowed")

    // If should'nt block while (although while desugars to if)

    val t2 = Restrict[Unit, All - If, CallTargets.None] {
      while (true) println()
    }
  }

  def testWhile = {
    val _ = Restrict[Int, All - While, CallTargets.None] {
      if (true) 2 else 3
    }

    illTyped(
      """Restrict[Int, All - While, CallTargets.None] {
           var stop = false
           while (! stop) stop = true
           1
         }
      """, "`while` expressions are disallowed")
  }

  def testApply = {
    val _ = Restrict[Int, All - Apply, CallTargets.None] {
      123
    }

    illTyped(
      """Restrict[Boolean, All - Apply, CallTargets.None] {
           "".contains("")
         }
      """, "Applications are disallowed")

    illTyped(
      """Restrict[String, All - Apply, CallTargets.None] {
           new String("foo")
         }
      """, "Applications are disallowed")
  }

  // NOTE: all the classes and traits defined in tests below are instantiated because otherwise the Scala compiler
  // optimizes the class definitions away!

  def testAbstractClass = {
    val _ = Restrict[Int, All - AbstractClass, CallTargets.None] {
      class C;
      new C
      case class C2();
      new C2
      trait T;
      new T {}
      1
    }

    illTyped(
      """Restrict[Int, All - AbstractClass, CallTargets.None] {
          abstract class C
          class C2; new C2
          1
         }
      """, "Abstract class definitions are disallowed")
  }

  def testConcreteClass = {
    val _ = Restrict[Int, All - ConcreteClass, CallTargets.None] {
      abstract class C
      trait T
      object O extends C with T
      1
    }

    illTyped(
      """Restrict[Int, All - ConcreteClass, CallTargets.None] {
          class C; new C
          1
         }
      """, "Concrete class definitions are disallowed")
  }

  def testTrait = {
    val _ = Restrict[Int, All - Trait, CallTargets.None] {
      class C;
      new C
      abstract class C2
      class C3 extends C2;
      new C3
      object O
      case class C4();
      new C4
      1
    }

    illTyped(
      """Restrict[Int, All - Trait, CallTargets.None] {
          trait T; new T{}
          1
         }
      """, "Trait definitions are disallowed")
  }

  def testObject = {
    val _ = Restrict[Int, All - Object, CallTargets.None] {
      class C;
      new C
      abstract class C2
      class C3 extends C2;
      new C3
      trait T;
      new T {}
      case class C4();
      new C4
      1
    }

    illTyped(
      """Restrict[Int, All - Object, CallTargets.None] {
          object O
          1
         }
      """, "Object definitions are disallowed")
  }
}
