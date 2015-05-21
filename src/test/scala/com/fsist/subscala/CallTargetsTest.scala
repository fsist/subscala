package com.fsist.subscala

import shapeless.test.illTyped

object CallTargetsTest {
  // This file doesn't contain any runtime tests; the tests are done at compile time.

  def testAllMethodsOf = {
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
      """, "Calling method foreach of trait IndexedSeqOptimized is disallowed")
  }

  def testMethod = {
    trait length extends CallTargets.MethodsOf[String] {
      def length(): Int
    }

    Restrict[String, Syntax.All, length] {
      "foo".length()
      ""
    }

    illTyped(
      """
        Restrict[String, Syntax.All, CallTargets.Method[String, String, TNil]] {
          "foo".equals("bar")
          ""
        }
      """
    )
  }
}
