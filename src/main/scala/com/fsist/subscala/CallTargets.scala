package com.fsist.subscala

import scala.language.higherKinds
import scala.reflect.api.Universe

sealed trait CallTargets

object CallTargets {
  type +[A <: CallTargets, B <: CallTargets] <: CallTargets
  type -[A <: CallTargets, B <: CallTargets] <: CallTargets

  type None <: CallTargets

  type AllMethodsOf[T] <: CallTargets
}
