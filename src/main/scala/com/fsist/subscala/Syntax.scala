package com.fsist.subscala

import scala.language.higherKinds

/** Restrictions on Scala syntax features */
sealed trait Syntax

object Syntax {
  sealed trait +[A <: Syntax, B <: Syntax] <: Syntax
  sealed trait -[A <: Syntax, B <: Syntax] <: Syntax

  sealed trait If <: Syntax
  sealed trait While <: Syntax
  sealed trait Apply <: Syntax

  sealed trait Def <: Syntax

  sealed trait AbstractClass <: Syntax
  sealed trait ConcreteClass <: Syntax
  sealed trait Class extends (AbstractClass + ConcreteClass)

  sealed trait Trait <: Syntax

  sealed trait Object <: Syntax

  sealed trait Import <: Syntax

  sealed trait Val <: Syntax
  sealed trait Var <: Syntax
  sealed trait Lazy <: Syntax

  sealed trait LazyVal extends (Lazy + Val)
  sealed trait LazyVar extends (Lazy + Var)

  sealed trait All extends (If + While + Apply + Def + Class + Trait + Object + Import + Val + Var + Lazy)
}

