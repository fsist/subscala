package com.fsist.subscala

import scala.annotation.tailrec
import scala.language.experimental.macros
import scala.language.implicitConversions

import scala.reflect.macros.blackbox

class Restrict(val c: blackbox.Context)  { self =>
  val reifier = new Reifier { type U = self.c.universe.type; override val universe = c.universe: self.c.universe.type }
  import c.universe._
  import reifier._

  def restrict[T: WeakTypeTag, S <: Syntax : WeakTypeTag, C <: CallTargets : WeakTypeTag](t: Expr[T]): Expr[T] = {
    validate(t, ReifiedSyntax(weakTypeOf[S]), ReifiedCallTargets(weakTypeOf[C]))
    t
  }

  @tailrec private def owner(symbol: Symbol): TypeSymbol =
    if (symbol.isType) symbol.asType else owner(symbol.owner)

  def validate[T: c.WeakTypeTag](expr: Expr[T], syntax: ReifiedSyntax, callTargets: ReifiedCallTargets): Unit = {
    val traverser = new Traverser {
      override def traverse(tree: Tree): Unit = {
        tree match {
          case _: If =>
            if (!syntax.`if`) c.abort(tree.pos, "`if` expressions are disallowed")
            super.traverse(tree)

          // While loop desugars to a LabelDef named while$N whose .rhs is a synthetic If tree which we shouldn't block
          // if syntax.if is false
          case label: LabelDef if tree.toString.startsWith("while") =>
            if (!syntax.`while`) c.abort(tree.pos, "`while` expressions are disallowed")

            val If(cond, thenp, elsep) = label.rhs
            super.traverse(cond)
            super.traverse(thenp)
            super.traverse(elsep)

          case apply: Apply =>
            if (!syntax.apply) c.abort(tree.pos, "Applications are disallowed")

            val method = apply.fun.symbol.asMethod
            if (! callTargets.matches(method)) {
              c.abort(apply.pos, s"Calling $method of ${owner(method)} is disallowed")
            }

            super.traverse(tree)

          case defdef: DefDef if defdef.name.toString != "<init>" =>
            if (!syntax.`def`) c.abort(tree.pos, s"Method definitions are disallowed")
            super.traverse(tree)

          case clazz: ClassDef =>
            if (clazz.mods.hasFlag(Flag.TRAIT)) {
              if (!syntax.`trait`) c.abort(tree.pos, "Trait definitions are disallowed")
            }
            else {
              if (clazz.mods.hasFlag(Flag.ABSTRACT) && !syntax.abstractClass) c.abort(tree.pos, "Abstract class definitions are disallowed")
              else if (!clazz.mods.hasFlag(Flag.ABSTRACT) && !syntax.concreteClass) c.abort(tree.pos, "Concrete class definitions are disallowed")
            }
            super.traverse(tree)

          // Ignore synthetic companion objects generated for case clsases
          case module: ModuleDef if !module.mods.hasFlag(Flag.SYNTHETIC) =>
            if (!syntax.`object`) c.abort(tree.pos, s"Object definitions are disallowed")
            super.traverse(tree)

          case _: Import =>
            if (!syntax.`import`) c.abort(tree.pos, "`import` statements are disallowed")
            super.traverse(tree)

          case valdef: ValDef =>
            if (valdef.mods.hasFlag(Flag.MUTABLE) && !syntax.`var`) c.abort(tree.pos, "`var` definitions are disallowed")
            if (!valdef.mods.hasFlag(Flag.MUTABLE) && !syntax.`val`) c.abort(tree.pos, "`val` definitions are disallowed")
            if (valdef.mods.hasFlag(Flag.LAZY) && !syntax.`lazy`) c.abort(tree.pos, "`lazy` val and var definitions are disallowed")
            super.traverse(tree)

          case _ => super.traverse(tree)
        }
      }
    }

    traverser.traverse(expr.tree)
  }
}

object Restrict {
  def apply[T, S <: Syntax, C <: CallTargets](t: T): T = macro Restrict.restrict[T, S, C]
}

