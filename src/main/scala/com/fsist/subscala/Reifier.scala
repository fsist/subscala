package com.fsist.subscala

import scala.annotation.tailrec
import scala.reflect.api.Universe

/** This wrapper lets us use the Type type, which is path-dependent on a Universe. */
trait Reifier {
  type U <: Universe with Singleton
  val universe: U
  import universe._

  /** Represents the [[Syntax]] type as a value. */
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

  object ReifiedSyntax {
    def apply(tpe: Type): ReifiedSyntax = {
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
        val List(a, b) = tpe.baseType(typeOf[Syntax.+[_, _]].typeSymbol).typeArgs
        apply(a).plus(apply(b))
      }
      else if (tpe <:< typeOf[Syntax.-[_, _]]) {
        val List(a, b) = tpe.baseType(typeOf[Syntax.-[_, _]].typeSymbol).typeArgs
        apply(a).minus(apply(b))
      }
      else throw new NotImplementedError(tpe.toString)
    }

  }

  /** A wrapper for Type that can be placed in a Set or Map.
    *
    * NOTE that it only compares equal with types from the same Universe as the Type it is holding, i.e. `u`.
    */
  implicit class TypeHolder(val tpe: Type) {
    override def equals(other: Any): Boolean = other match {
      case t: Type => tpe =:= t
      case holder: TypeHolder => tpe =:= holder.tpe
      case _ => false
    }
    override def hashCode(): Int = tpe.toString.hashCode
    override def toString: String = tpe.toString
  }

  /** A method specification derived from a CallTargets.MethodsOf type, which contains all the information necessary
    * to identify the matching method.
    */
  case class MethodSpec(name: String, paramTypess: List[List[Type]]) {
    override def equals(other: Any): Boolean = {
      if (!other.isInstanceOf[MethodSpec]) false
      else {
        val otherMethod = other.asInstanceOf[MethodSpec]
        name == otherMethod.name &&
          paramTypess.length == otherMethod.paramTypess.length &&
          paramTypess.zip(otherMethod.paramTypess).forall {
            case (a, b) => a.length == b.length && a.zip(b).forall {
              case (a, b) => a =:= b
            }
          }
      }
    }

    override def hashCode(): Int = name.hashCode + paramTypess.size * paramTypess.map(_.size).sum

    def matches(method: MethodSymbol): Boolean = {
      method.name.toString == name &&
        method.paramLists.size == paramTypess.size &&
        method.paramLists.zip(paramTypess).forall {
          case (paramList, paramTypes) =>
            paramList.size == paramTypes.size &&
              paramList.zip(paramTypes).forall {
                case (param, tpe) => param.asType.toType =:= tpe
              }
        }
    }
  }


  /** A selection of some methods of an unspecified type. */
  sealed trait MethodSpecs {
    def +(other: MethodSpecs): MethodSpecs
    def -(other: MethodSpecs): MethodSpecs

    def matches(method: MethodSymbol): Boolean
  }

  object MethodSpecs {
    def all: MethodSpecs = AllBut(Set.empty)
    def none: MethodSpecs = These(Set.empty)

    case class These(methods: Set[MethodSpec]) extends MethodSpecs {
      override def +(other: MethodSpecs): MethodSpecs = other match {
        case These(om) => These(methods ++ om)
        case AllBut(om) => AllBut(om -- methods)
      }

      override def -(other: MethodSpecs): MethodSpecs = other match {
        case These(om) => These(methods -- om)
        case AllBut(om) => These(methods intersect om)
      }

      override def matches(method: MethodSymbol): Boolean = methods.exists(_.matches(method))
    }

    case class AllBut(methods: Set[MethodSpec]) extends MethodSpecs {
      override def +(other: MethodSpecs): MethodSpecs = other match {
        case These(om) => AllBut(methods -- om)
        case AllBut(om) => AllBut(methods intersect om)
      }

      override def -(other: MethodSpecs): MethodSpecs = other match {
        case These(om) => AllBut(methods ++ om)
        case AllBut(om) if om.isEmpty => none
        case AllBut(om) =>
          throw new NotImplementedError("Not implemented; calculating the difference of two negative method groups without " +
            "knowing the set of all methods of that type. Try specifying the methods you want explicitly and not as " +
            "a difference.")
      }

      override def matches(method: MethodSymbol): Boolean = !methods.exists(_.matches(method))
    }
  }

  /** Represents the [[ReifiedCallTargets]] type as a value. */
  sealed trait ReifiedCallTargets {
    def +(other: ReifiedCallTargets): ReifiedCallTargets
    def -(other: ReifiedCallTargets): ReifiedCallTargets
    def matches(method: MethodSymbol): Boolean
  }

  object ReifiedCallTargets {

    /** Allows calls to all methods of all types. */
    case object All extends ReifiedCallTargets {
      override def +(other: ReifiedCallTargets): ReifiedCallTargets = this

      override def -(other: ReifiedCallTargets): ReifiedCallTargets = other match {
        case All => none
        case These(map) => throw new NotImplementedError("CallTargets specification by subtraction")
      }

      override def matches(method: MethodSymbol): Boolean = true
    }

    val none: ReifiedCallTargets = These(Map.empty)

    case class These(methods: Map[TypeHolder, MethodSpecs]) extends ReifiedCallTargets {
      private def mergeMethods(other: Map[TypeHolder, MethodSpecs],
                               merger: (MethodSpecs, MethodSpecs) => MethodSpecs): Map[TypeHolder, MethodSpecs] = {
        methods.foldLeft(other) {
          case (dict, (key, value)) =>
            dict.get(key) match {
              case None => dict.updated(key, value)
              case Some(orig) =>
                val merged = merger(orig, value)
                if (merged == MethodSpecs.These(Set.empty)) dict - key
                else dict.updated(key, merged)
            }
        }
      }

      override def +(other: ReifiedCallTargets): ReifiedCallTargets = other match {
        case All => other
        case These(otherMethods) => These(mergeMethods(otherMethods, _ + _))
      }

      override def -(other: ReifiedCallTargets): ReifiedCallTargets = other match {
        case All => none
        case These(otherMethods) => These(mergeMethods(otherMethods, _ - _))
      }

      override def matches(method: MethodSymbol): Boolean = {
        @tailrec def getOwner(symbol: Symbol): TypeSymbol =
          if (symbol.isType) symbol.asType else getOwner(symbol.owner)

        val owner = getOwner(method).toType
        methods.get(owner) match {
          case Some(methods) => methods.matches(method)
          case None => false
        }
      }
    }

    def apply(tpe: Type): ReifiedCallTargets = {
      if (tpe =:= typeOf[CallTargets.None]) none
      else if (tpe =:= typeOf[CallTargets.All]) All
      else if (tpe <:< typeOf[CallTargets.AllMethodsOf[_]]) {
        val List(owner) = tpe.baseType(typeOf[CallTargets.AllMethodsOf[_]].typeSymbol).typeArgs
        These(Map(TypeHolder(owner) -> MethodSpecs.all))
      }
      else if (tpe <:< typeOf[CallTargets.MethodsOf[_]]) {
        val List(owner) = tpe.baseType(typeOf[CallTargets.MethodsOf[_]].typeSymbol).typeArgs
        These(Map(TypeHolder(owner) -> reifyMethodsOf(owner)))
      }
      else if (tpe <:< typeOf[CallTargets.+[_, _]]) {
        val List(a, b) = tpe.baseType(typeOf[CallTargets.+[_, _]].typeSymbol).typeArgs
        apply(a) + apply(b)
      }
      else if (tpe <:< typeOf[CallTargets.-[_, _]]) {
        val List(a, b) = tpe.baseType(typeOf[CallTargets.-[_, _]].typeSymbol).typeArgs
        apply(a) - apply(b)
      }
      else throw new NotImplementedError(s"CallTargets type $tpe")
    }
  }

  private def reifyMethodsOf(owner: Type): MethodSpecs = {
    MethodSpecs.These(
      owner.decls.foldLeft(Set.empty[MethodSpec]) {
        case (methods, symbol) =>
          if (!symbol.isMethod) methods
          else {
            val method = symbol.asMethod
            val reified = MethodSpec(method.name.toString,
              method.paramLists.map(_.map(_.asTerm.info))
            )
            methods + reified
          }
      }
    )
  }
}
