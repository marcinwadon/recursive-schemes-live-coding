package net.wadon

import cats.Functor
import cats.implicits._
import net.wadon.console._

object FoldRightUnfoldExample extends App {

  cyan(" --- FoldRight ---")
  // right to left
  // applies binary operation to all elements of a list and a start value
  // returns initial value if the list if empty

  def foldRight[E, B](init: List[E])(z: B)(op: (E, B) => B): B = ???

  // List(1, 2, 3) == 1 :: 2 :: 3 :: Nil
  // List(1) == 1 :: Nil
  // List() == Nil
  def foldRight0[E, B](init: List[E])(z: B)(op: (E, B) => B): B = init match {
    case Nil => z
    case head :: tail => op(head, foldRight0(tail)(z)(op))
  }

  // List[E] ---> A
  // A ----> List[E]

  val prodOp: (Int, Int) => Int = _ * _

  yellow(foldRight0(1 :: 10 :: 20 :: Nil)(1)(prodOp))

  // unfold --- f: A ----> Option[(E, A)]
  // fold --- f: Option[(E, B)] ---> A

  // value: B
  // ()  ---z--->     B
  // (E, B) ---op---> B

  // Either[(), (E, B)] ---> B
  // Option[(E, B)]     ---> B

  // either: a + b
  // (): 1
  // either: 1 + a
  // option: 1 + a

  def foldRight1[E, B](init: List[E])(f: Option[(E, B)] => B): B = init match {
    case Nil => f(None)
    case head :: tail => f(Some((head, foldRight1(tail)(f))))
  }

  val prodf: Option[(Int, Int)] => Int = {
    case None => 1
    case Some((x, y)) => x * y
  }

  yellow(foldRight1(1 :: 10 :: 20 :: Nil)(prodf))

  def foldRight2[E, B](f: Option[(E, B)] => B): List[E] => B = {
    lazy val kernel: List[E] => B = _ match {
      case Nil => f(None)
      case head :: tail => f(Some((head, kernel(tail))))
    }

    kernel
  }

  yellow(foldRight2(prodf)(1 :: 10 :: 20 :: Nil))

  def foldRight3[E, B](f: Option[(E, B)] => B): List[E] => B = {
    new (List[E] => B) { kernel =>
      def apply(init: List[E]): B = init match {
        case Nil => f(None)
        case head :: tail => f(Some((head, kernel(tail))))
      }
    }
  }

  yellow(foldRight3(prodf)(1 :: 10 :: 20 :: Nil))

  /*
  Fold:
  1. unpack/project structure
  2. recursion
  3. computation
   */

  def foldRight4[E, B](f: Option[(E, B)] => B): List[E] => B =
    new (List[E] => B) { kernel =>

      def step1: List[E] => Option[(E, List[E])] = { _ match {
        case Nil => None
        case head :: tail => Some((head, tail))
      } }

      def step2: Option[(E, List[E])] => Option[(E, B)] = _ match {
        case None => None
        case Some((e, le)) => Some((e, kernel(le)))
      }

      def step3: Option[(E, B)] => B = f

      def apply(init: List[E]): B = {
        step3(step2(step1(init)))
      }
    }

  yellow(foldRight4(prodf)(1 :: 10 :: 20 :: Nil))

  type ListF[A, B] = Option[(A, B)]
  implicit def functor[A]: Functor[ListF[A, *]] = Functor[Option].compose[(A, *)]

  def projectList[E]: List[E] => ListF[E, List[E]] = {
    _ match {
      case Nil => None
      case head :: tail => Some((head, tail))
    }
  }

  def foldRight5[F[_]: Functor, S, B](f: F[B] => B)(project: S => F[S]): S => B = {
    new (S => B) { kernel =>
      def apply(init: S): B = f(project(init).fmap(kernel))
    }
  }

  val prodFList: ListF[Int, Int] => Int = {
    case None => 1
    case Some((x, y)) => x * y
  }

  red(foldRight5(prodFList)(projectList).apply(1 :: 10 :: 20 :: Nil))

  // F[A] => A --- algebra
  // A => F[A] --- coalgebra

  // folding
  def cata[F[_]: Functor, S, B](algebra: F[B] => B)(project: S => F[S]): S => B = {
    new (S => B) { kernel =>
      def apply(init: S): B = algebra(project(init).fmap(kernel))
    }
  }

  // unfolding
  def ana[F[_]: Functor, S, B](coalgebra: B => F[B])(embed: F[S] => S): B => S = {
    new (B => S) { kernel =>
      def apply(init: B): S = embed(coalgebra(init).fmap(kernel))
    }
  }

  /*
       project (unpack)
  S -------------------> F[S]
  |                       |
                          |  recursion (functor map)
  |                       |
 \ /                     \ /
  B <------------------- F[B]
       compute
 */

  cyan(" --- Unfold ---")

  def unfold[E, A](init: A)(f: A => Option[(E, A)]): List[E] = ???

  def unfold0[E, A](init: A)(f: A => Option[(E, A)]): List[E] = f(init) match {
    case None => Nil
    case Some((e, a)) => e :: unfold0(a)(f)
  }

  val rangeOp: Int => Option[(Int, Int)] =
    v => if (v <= 0) None else Some((v, v - 1))

  yellow(unfold0(10)(rangeOp))

  def unfold2[E, A](f: A => Option[(E, A)]): A => List[E] = {
    lazy val kernel: A => List[E] = f(_) match {
      case None => Nil
      case Some((e, a)) => e :: kernel(a)
    }

    kernel
  }

  yellow(unfold2(rangeOp)(10))

  def unfold3[E, A](f: A => Option[(E, A)]): A => List[E] = {
    new (A => List[E]) { kernel =>
      def apply(init: A): List[E] = {
        f(init) match {
          case None => Nil
          case Some((e, a)) => e :: kernel(a)
        }
      }
    }
  }

  yellow(unfold3(rangeOp)(10))

}
