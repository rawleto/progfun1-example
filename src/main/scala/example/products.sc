def product(f: Int => Int)(a: Int, b: Int): Int = {
  if a > b then 1 else f(a) * product(f)(a + 1, b)
}

product(x => x * x)(1, 5)

def fact(n: Int): Int =
  product(x => x)(1, n)

fact(5)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  def recur(a: Int): Int =
    if a > b then zero
    else combine(f(a), recur(a + 1))
  recur(a)
}

mapReduce(x => x * 2, (a, b) => a + b, 0)(0, 9)

