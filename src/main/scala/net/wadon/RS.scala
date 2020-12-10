package net.wadon

import cats._
import cats.implicits._
import higherkindness.droste._
import higherkindness.droste.implicits._
import higherkindness.droste.data._
import higherkindness.droste.data.list.{ConsF, ListF, NilF}
import higherkindness.droste.util.DefaultTraverse
import console._

object RS extends App {

  val natCoalgebra: Coalgebra[Option, Int] = Coalgebra {
    n => if (n > 0) Some(n-1) else None
  }

  val sumAlgebra: Algebra[Option, Int] = Algebra {
    case None => 0
    case Some(n) => n + 1
  }

  val fibAlgebra: CVAlgebra[Option, Int] = CVAlgebra {
    case None => 0
    case Some(_ :< None) => 1
    case Some(r1 :< Some(r2 :< _)) => r1 + r2
  }

  red(scheme.zoo.dyna(fibAlgebra, natCoalgebra).apply(10).toString)

  //

  sealed trait Expr
  case class Const(value: BigDecimal) extends Expr
  case class Add(x: Expr, y: Expr) extends Expr
  case class Mul(x: Expr, y: Expr) extends Expr

  sealed trait ExprF[A]
  case class ConstF[A](value: BigDecimal) extends ExprF[A]
  case class AddF[A](x: A, y: A) extends ExprF[A]
  case class MulF[A](x: A, y: A) extends ExprF[A]

  object ExprF {
    implicit val exprFFunctor: Functor[ExprF] = new Functor[ExprF] {
      override def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match {
        case _ @ ConstF(x) => ConstF[B](x)
        case AddF(x, y) => AddF(f(x), f(y))
        case MulF(x, y) => MulF(f(x), f(y))
      }
    }

    val embedAlgebra: Algebra[ExprF, Expr] = Algebra {
      case ConstF(v)  => Const(v)
      case AddF(x, y) => Add(x, y)
      case MulF(x, y) => Mul(x, y)
    }

    val projectCoalgebra: Coalgebra[ExprF, Expr] = Coalgebra {
      case Const(v)  => ConstF(v)
      case Add(x, y) => AddF(x, y)
      case Mul(x, y) => MulF(x, y)
    }

    implicit val basisExprF: Basis[ExprF, Expr] =
      Basis.Default(embedAlgebra, projectCoalgebra)
  }

  val evaluateAlgebra: Algebra[ExprF, BigDecimal] = Algebra {
    case ConstF(v) => v
    case AddF(x, y) => x + y
    case MulF(x, y) => x * y
  }

  val printAlgebra: Algebra[ExprF, String] = Algebra {
    case ConstF(v) => s"${v}"
    case AddF(x, y) => s"(${x} + ${y})"
    case MulF(x, y) => s"[${x} * ${y}]"
  }

  val evaluate: Expr => BigDecimal = scheme.cata(evaluateAlgebra)
  val print: Expr => String = scheme.cata(printAlgebra)

  def run: Expr => Unit = { e =>
    println(s"${print(e)} = ${evaluate(e)}")
  }

  run(Const(1))
  run(Add(Const(1), Const(1)))
  run(Add(Add(Const(1), Const(2)), Const(5)))
  run(Mul(Add(Const(1), Const(4)), Const(5)))

  // ---

  val evenAlgebra = Algebra[ListF[Int, *], Boolean] {
    case NilF => false
    case ConsF(_, bool) => !bool
  }

  val calcRAlgebra = RAlgebra[Boolean, ListF[Int, *], Int] {
    case NilF => 0
    case ConsF(n, (b, x)) => if (b) n + x else n - x
  }

  // plusMinus(List(a, b, c, d, e)) = a - (b + (c - (d + e)))
  val plusMinus = scheme.zoo.zygo[ListF[Int, *], List[Int], Boolean, Int](evenAlgebra, calcRAlgebra)

  println(plusMinus(List()))
  println(plusMinus(List(1, 2, 3, 4, 5)))
  println(plusMinus(List(1, -1, 1, -1, 1, -1)))

}
