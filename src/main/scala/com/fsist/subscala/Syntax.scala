package com.fsist.subscala

import scala.language.higherKinds

/** Restrictions on Scala syntax features */
sealed trait Syntax

object Syntax {
  type +[A <: Syntax, B <: Syntax] <: Syntax
  type -[A <: Syntax, B <: Syntax] <: Syntax

  type If <: Syntax
  type While <: Syntax
  type Apply <: Syntax

  type Def <: Syntax

  type AbstractClass <: Syntax
  type ConcreteClass <: Syntax
  type Class = AbstractClass + ConcreteClass

  type Trait <: Syntax

  type Object <: Syntax

  type Import <: Syntax

  type Val <: Syntax
  type Var <: Syntax
  type Lazy <: Syntax

  type LazyVal = Lazy + Val
  type LazyVar = Lazy + Var

  type All = If + While + Apply + Def + Class + Trait + Object + Import + Val + Var + Lazy
}

