package example

import scala.annotation.tailrec

@main def run(): Unit = {
  val value = List(1, 2, 3)
  println("List: " + value)
  println("Max: " + Lists.max(value))
  println("Sum: " + Lists.sum(value))

  def factorial(x: Int): Int =
    @tailrec
    def factorialHelper(acc: Int, minuend: Int): Int =
      if minuend == 0 then acc
      else factorialHelper(minuend * acc, minuend - 1)

    factorialHelper(1, x)

  println("fact(3): " + factorial(3))

  def product(a: Int, b: Int): Int = {
    @tailrec
    def f(a: Int, acc: Int): Int =
      if a > b then acc
      else f(a + 1, if acc == 0 then a else a * acc)
    f(a, 0)
  }

  println("1 * 2 * 3 = " + product(1, 3))
  println("2 * 3 = " + product(2, 3))
  println("3 * 4 * 5 * 6 = " + product(3, 6))

  def sum(a: Int, b: Int): Int = {
    @tailrec
    def f(a: Int, acc: Int): Int =
      if a > b then acc
      else f(a + 1, a + acc)
    f(a, 0)
  }

  println("1 + 2 + 3 = " + sum(1, 3))
  println("2 + 3 = " + sum(2, 3))
  println("3 + 4 + 5 + 6 = " + sum(3, 6))

  def productGen: (Int, Int) => Int = (a, b) => a * b
  def sumGen: (Int, Int) => Int = (a, b) => a + b

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int ={
    def recur(a: Int): Int =
      if a > b then zero
      else combine(f(a), recur(a + 1))
    recur(a)
  }

  println("mapReduce: 3 + 4 + 5 + 6 = " + mapReduce(x => x, sumGen, 0)(3, 6))
  println("mapReduce: 3 * 4 * 5 * 6 = " + mapReduce(x => x, productGen, 1)(3, 6))

  def factProduct(b: Int): Int = product(1, b)

  println("factProduct(3) " + factProduct(3))

}