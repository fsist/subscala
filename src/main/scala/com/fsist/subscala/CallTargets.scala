package com.fsist.subscala

import scala.annotation.StaticAnnotation
import scala.language.higherKinds

sealed trait CallTargets

object CallTargets {
  sealed trait +[A <: CallTargets, B <: CallTargets] <: CallTargets
  sealed trait -[A <: CallTargets, B <: CallTargets] <: CallTargets

  sealed trait None <: CallTargets
  sealed trait All <: CallTargets

  sealed trait AllMethodsOf[T] <: CallTargets

  /** Any custom trait that extends this will allow calls to methods of the type `Owner` which have the same signatures
    * as the methods of the custom type. A signature here means the number of param lists and the number, order and
    * types of the parameters in each list, but not e.g. type parameters, method and parameter implicitness,
    * method visibility, val/var, annotations, or return types.
    *
    * For instance, to allow calls to String.length(), you would write:
    *
    * trait AllowLength extends MethodsOf[String] { def length(): Int }
    */
  trait MethodsOf[Owner] extends CallTargets

  /** Allow calling methods defined in the same code tree passed to the validation macro.
    * This includes constructors of classes defined in the same tree.
    */
  sealed trait LocallyDefined <: CallTargets
}

