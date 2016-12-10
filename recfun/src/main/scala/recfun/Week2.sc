def op(m: Int => Int, r: (Int, Int) => Int)(unit: Int, a: Int, b: Int): Int =
  if (a > b) unit
  else r(m(a), op(m, r)(unit, a + 1, b))

//def sum(f: Int => Int)(a: Int, b: Int): Int =
//  if (a > b) 0
//  else f(a) + sum(f)(a + 1, b)

def sum(f: Int => Int)(a: Int, b: Int): Int =
  op(f, _ + _)(0, a, b)

//def product(f: Int => Int)(a: Int, b: Int): Int =
//  if (a > b) 1
//  else f(a) * product(f)(a + 1, b)

def product(f: Int => Int)(a: Int, b: Int): Int =
  op(f, _ * _)(1, a, b)

def fact(n: Int): Int = product(x => x)(1, n)

sum(x => x)(1, 5)
fact(5)
product(x => x * x)(2, 4)
