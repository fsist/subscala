package com.fsist.subscala

import scala.annotation.tailrec
import scala.language.experimental.macros
import scala.reflect.api.Universe

import scala.reflect.macros.blackbox

class Restrict(val c: blackbox.Context) {

  import c.universe._

  // A wrapper for Type that can be placed in a Set
  implicit class TypeHolder(val tpe: Type) {
    override def equals(other: Any): Boolean = other match {
      case t: Type => tpe =:= t
      case holder: TypeHolder => tpe =:= holder.tpe
      case _ => false
    }

    override def hashCode(): Int = tpe.toString.hashCode
  }

  case class ReifiedSyntax(`if`: Boolean = false, `while`: Boolean = false, apply: Boolean = false, `def`: Boolean = false,
                           abstractClass: Boolean = false, concreteClass: Boolean = false,
                           `trait`: Boolean = false, `object`: Boolean = false,
                           `import`: Boolean = false,
                           `val`: Boolean = false, `var`: Boolean = false, `lazy`: Boolean = false) {
    def plus(other: ReifiedSyntax): ReifiedSyntax = ReifiedSyntax(
      `if` || other.`if`,
      `while` || other.`while`,
      apply || other.apply,
      `def` || other.`def`,
      abstractClass || other.abstractClass,
      concreteClass || other.concreteClass,
      `trait` || other.`trait`,
      `object` || other.`object`,
      `import` || other.`import`,
      `val` || other.`val`,
      `var` || other.`var`,
      `lazy` || other.`lazy`
    )

    def minus(other: ReifiedSyntax): ReifiedSyntax = ReifiedSyntax(
      `if` && !other.`if`,
      `while` && !other.`while`,
      apply && !other.apply,
      `def` && !other.`def`,
      abstractClass && !other.abstractClass,
      concreteClass && !other.concreteClass,
      `trait` && !other.`trait`,
      `object` && !other.`object`,
      `import` && !other.`import`,
      `val` && !other.`val`,
      `var` && !other.`var`,
      `lazy` && !other.`lazy`
    )
  }

  sealed trait ReifiedCallTargets {
    def plus(other: ReifiedCallTargets): ReifiedCallTargets
    def minus(other: ReifiedCallTargets): ReifiedCallTargets
    def validate(pos: Position, method: MethodSymbol): Unit
  }

  object ReifiedCallTargets {

    case object All extends ReifiedCallTargets {
      override def plus(other: ReifiedCallTargets): ReifiedCallTargets = All
      override def minus(other: ReifiedCallTargets): ReifiedCallTargets = All
      override def validate(pos: Position, method: MethodSymbol): Unit = ()
    }

    case class Some(allMethodsOf: Set[TypeHolder]) extends ReifiedCallTargets {
      override def plus(other: ReifiedCallTargets): ReifiedCallTargets = other match {
        case All => All
        case some: Some => Some(allMethodsOf.union(some.allMethodsOf))
      }

      override def minus(other: ReifiedCallTargets): ReifiedCallTargets = other match {
        case All => All
        case some: Some => Some(allMethodsOf.diff(some.allMethodsOf))
      }

      override def validate(pos: Position, method: MethodSymbol): Unit = {
        @tailrec def getOwner(symbol: Symbol): TypeSymbol = if (symbol.isType) symbol.asType else getOwner(symbol.owner)
        val owner = getOwner(method).toType
        if (!allMethodsOf.contains(owner)) {
          c.abort(pos, s"Calling $method of ${owner.typeSymbol.name} is disallowed")
        }
      }
    }
  }

  def restrict[T: c.WeakTypeTag, S <: Syntax : c.WeakTypeTag, C <: CallTargets : c.WeakTypeTag](t: Expr[T]): Expr[T] = {
    validate(t, reifySyntax(weakTypeOf[S]), reifyCallTargets(weakTypeOf[C]))
    t
  }

  def reifySyntax(tpe: Type): ReifiedSyntax = {
    if (tpe =:= typeOf[Syntax.If]) ReifiedSyntax(`if` = true)
    else if (tpe =:= typeOf[Syntax.While]) ReifiedSyntax(`while` = true)
    else if (tpe =:= typeOf[Syntax.Apply]) ReifiedSyntax(apply = true)
    else if (tpe =:= typeOf[Syntax.Def]) ReifiedSyntax(`def` = true)
    else if (tpe =:= typeOf[Syntax.AbstractClass]) ReifiedSyntax(abstractClass = true)
    else if (tpe =:= typeOf[Syntax.ConcreteClass]) ReifiedSyntax(concreteClass = true)
    else if (tpe =:= typeOf[Syntax.Trait]) ReifiedSyntax(`trait` = true)
    else if (tpe =:= typeOf[Syntax.Object]) ReifiedSyntax(`object` = true)
    else if (tpe =:= typeOf[Syntax.Import]) ReifiedSyntax(`import` = true)
    else if (tpe =:= typeOf[Syntax.Val]) ReifiedSyntax(`val` = true)
    else if (tpe =:= typeOf[Syntax.Var]) ReifiedSyntax(`var` = true)
    else if (tpe =:= typeOf[Syntax.Lazy]) ReifiedSyntax(`lazy` = true)
    else if (tpe <:< typeOf[Syntax.+[_, _]]) {
      val args = tpe.baseType(typeOf[Syntax.+[_, _]].typeSymbol).typeArgs
      reifySyntax(args(0)).plus(reifySyntax(args(1)))
    }
    else if (tpe <:< typeOf[Syntax.-[_, _]]) {
      val args = tpe.baseType(typeOf[Syntax.-[_, _]].typeSymbol).typeArgs
      reifySyntax(args(0)).minus(reifySyntax(args(1)))
    }
    else throw new NotImplementedError(tpe.toString)
  }

  def reifyCallTargets(tpe: Type): ReifiedCallTargets = {
    if (tpe =:= typeOf[CallTargets.None]) ReifiedCallTargets.Some(Set.empty)
    else if (tpe =:= typeOf[CallTargets.All]) ReifiedCallTargets.All
    else if (tpe <:< typeOf[CallTargets.AllMethodsOf[_]]) {
      val args = tpe.baseType(typeOf[CallTargets.AllMethodsOf[_]].typeSymbol).typeArgs
      ReifiedCallTargets.Some(Set(args(0)))
    }
    else if (tpe <:< typeOf[CallTargets.+[_, _]]) {
      val args = tpe.baseType(typeOf[CallTargets.+[_, _]].typeSymbol).typeArgs
      reifyCallTargets(args(0)).plus(reifyCallTargets(args(1)))
    }
    else if (tpe <:< typeOf[CallTargets.-[_, _]]) {
      val args = tpe.baseType(typeOf[CallTargets.-[_, _]].typeSymbol).typeArgs
      reifyCallTargets(args(0)).minus(reifyCallTargets(args(1)))
    }
    else throw new NotImplementedError(tpe.toString)
  }

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
            callTargets.validate(apply.pos, method)

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

