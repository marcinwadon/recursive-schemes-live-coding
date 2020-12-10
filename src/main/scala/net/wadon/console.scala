package net.wadon

import cats.Show

object console {
  private def printC(color: String)(s: String) = println(s"${color}${s}${Console.RESET}")

  def red[A](a: A)(implicit show: Show[A]): Unit = {
    printC(Console.RED)(show.show(a))
  }
  def yellow[A](a: A)(implicit show: Show[A]): Unit = {
    printC(Console.YELLOW)(show.show(a))
  }
  def green[A](a: A)(implicit show: Show[A]): Unit = {
    printC(Console.GREEN)(show.show(a))
  }
  def white[A](a: A)(implicit show: Show[A]): Unit = {
    printC(Console.WHITE)(show.show(a))
  }
  def blue[A](a: A)(implicit show: Show[A]): Unit = {
    printC(Console.BLUE)(show.show(a))
  }
  def magenta[A](a: A)(implicit show: Show[A]): Unit = {
    printC(Console.MAGENTA)(show.show(a))
  }
  def cyan[A](a: A)(implicit show: Show[A]): Unit = {
    printC(Console.CYAN)(show.show(a))
  }

}
