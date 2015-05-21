package com.fsist.subscala

import scala.annotation.StaticAnnotation
import scala.language.higherKinds

sealed trait CallTargets

object CallTargets {
  type +[A <: CallTargets, B <: CallTargets] <: CallTargets
  type -[A <: CallTargets, B <: CallTargets] <: CallTargets

  type None <: CallTargets
  type All <: CallTargets

  type AllMethodsOf[T] <: CallTargets

  /** Any custom trait that extends this will allow calls to methods of the type `Owner` which have the same signatures
    * as the methods of the custom type. A signature here means the number of param lists and the number, order and
    * types of the parameters in each list, but not e.g. type parameters, method and parameter implicitness,
    * method visibility, val/var, annotations, or return types.
    *
    * For instance, to allow calls to String.length(), you would write:
    *
    * trait AllowEquals extends MethodsOf[String] { def length(): Int }
    */
  trait MethodsOf[Owner] extends CallTargets
}

