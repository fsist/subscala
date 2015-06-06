package com.fsist.subscala

import scala.annotation.tailrec
import scala.reflect.api.Universe

/** This wrapper lets us use the Type type, which is path-dependent on a Universe. */
trait Reifier {
  type U <: Universe with Singleton
  val universe: U

  import universe._

  def info(msg: String): Unit

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
      val methodParamTypes = method.paramLists.map(_.map(_.typeSignature))

      if (method.name.encodedName.toString != name) false
      else {
        methodParamTypes.size == paramTypess.size &&
          methodParamTypes.zip(paramTypess).forall {
            case (actual, expected) =>
              actual.size == expected.size &&
                actual.zip(expected).forall {
                  case (tpe1, tpe2) =>
                    tpe1 =:= tpe2
                }
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
    def matches(method: MethodSymbol, methodType: MethodType, isLocallyDefined: Boolean): Boolean
  }

  object ReifiedCallTargets {

    /** Allows calls to all methods of all types, including locally defined ones. */
    case object All extends ReifiedCallTargets {
      override def +(other: ReifiedCallTargets): ReifiedCallTargets = this

      override def -(other: ReifiedCallTargets): ReifiedCallTargets = other match {
        case All => none
        case These(_, _) => throw new NotImplementedError("CallTargets specification by subtraction")
      }

      override def matches(method: MethodSymbol, methodType: MethodType, isLocallyDefined: Boolean): Boolean = true
    }

    val none: ReifiedCallTargets = These(Map.empty, false)

    /** Allows calls to the methods given in `methods` for each type.
      *
      * @param locallyDefined if true, allows calls to all new methods defined inside the same tree the macro is checking. */
    case class These(methods: Map[TypeHolder, MethodSpecs], locallyDefined: Boolean) extends ReifiedCallTargets {
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
        case All => All
        case These(otherMethods, otherLocal) => These(mergeMethods(otherMethods, _ + _), locallyDefined || otherLocal)
      }

      override def -(other: ReifiedCallTargets): ReifiedCallTargets = other match {
        case All => none
        case These(otherMethods, otherLocal) => These(mergeMethods(otherMethods, _ - _), locallyDefined && !otherLocal)
      }

      override def matches(method: MethodSymbol, methodType: MethodType, isLocallyDefined: Boolean): Boolean = {
        if (isLocallyDefined) locallyDefined
        else {
          @tailrec def getOwner(symbol: Symbol): TypeSymbol =
            if (symbol.isType) symbol.asType else getOwner(symbol.owner)

          // When calling a generic method, the method symbol contains explicit abstract types in place of type parameters,
          // for instance Function0[R], as opposed to the type constructor Function0, or the existential type Function0[_].
          // If you look at the R type parameter, it will be just some abstract Type with no extra info (except for type
          // bounds presumably).
          //
          // On the other hand, the method type is a concrete type, e.g. when calling Function0[Unit], it would be the
          // method type ()Unit. It's not a concrete generic type, i.e. it doens't know that the Unit is a generic type
          // argument.
          //
          // We need to construct the owner type Function0[Unit] instead of Function0[R] to make MethodsOf[Function0[Unit]] work.
          // To do this, we need to substitute R for Unit in the owner symbol, based on the known method type () => Unit.

          val owner = getOwner(method).toType
          val realOwner = owner
            .substituteTypes(List(method.returnType.typeSymbol), List(methodType.resultType))
            .substituteTypes(method.paramLists.flatten.map(_.info.typeSymbol), methodType.paramLists.flatten.map(_.info))

          // Should rewrite this to speed it up
          methods.exists {
            case (holder, ms) =>
              if (realOwner <:< holder.tpe) ms.matches(method)
              else false
          }
        }
      }
    }

    def apply(tpe: Type): ReifiedCallTargets = {
      if (tpe =:= typeOf[CallTargets.None]) none
      else if (tpe =:= typeOf[CallTargets.All]) All
      else if (tpe <:< typeOf[CallTargets.AllMethodsOf[_]]) {
        val List(owner) = tpe.baseType(typeOf[CallTargets.AllMethodsOf[_]].typeSymbol).typeArgs
        These(Map(TypeHolder(owner) -> MethodSpecs.all), false)
      }
      else if (tpe <:< typeOf[CallTargets.MethodsOf[_]]) {
        val List(owner) = tpe.baseType(typeOf[CallTargets.MethodsOf[_]].typeSymbol).typeArgs
        These(Map(TypeHolder(owner) -> reifyMethodsOf(tpe)), false)
      }
      else if (tpe =:= typeOf[CallTargets.LocallyDefined]) {
        These(Map.empty, true)
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
}
