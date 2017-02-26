// Take the sum of the integers between a and b
def sumInts(a: Int, b: Int): Int =
  if (a > b) 0 else a + sumInts(a+1, b)

sumInts(1, 3)

// Take the sum of the cube of all integers between a and b
def cube(x: Int): Int =
  x * x * x

def sumCubes(a: Int, b: Int): Int =
  if (a > b) 0 else cube(a) + sumCubes(a+1, b)

sumCubes(1, 3)

// Using Higher Order Functions with Currying and Tail Recursion
def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(acc: Int, a: Int): Int =
    if (a > b) acc else loop(acc+f(a), a+1)

  loop(0, a)
}

sum((x: Int) => x)(1, 3)
sum((x: Int) => x * x * x)(1, 3)

/* Function that calculates the product of the values of a function
for the point on a given interval */
def product(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(acc: Int, a: Int): Int =
    if (a > b) acc else loop(acc*f(a), a+1)

  loop(1, a)
}

product((x: Int) => x * x)(3, 4)

// Factorial in terms of product
def factorial(n: Int): Int =
  product((x: Int) => x)(1, n)

factorial(5)

// General function that generalizes both sum and product
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)
             (a: Int, b: Int): Int =
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a+1, b))

def sum1(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x+y, 0)(a, b)

def product1(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x*y, 1)(a, b)

sum1((x: Int) => x)(1, 3)
product1((x: Int) => x * x)(3, 4)

// Creating complex data structure
class Rational(x: Int, y: Int) {
  require(y > 0, "Denominator must be non-zero")
  val numer: Int = x / gcd(x, y)
  val denom: Int = y / gcd(x, y)

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def + (that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def unary_- : Rational = new Rational(-numer, denom)

  def - (that: Rational): Rational =
    this + -that

  def < (that: Rational): Boolean =
    numer * that.denom < that.numer * denom

  def max(that: Rational): Rational =
    if (this < that) that else this

  override def toString: String =
    numer + "/" + denom
}

val x = new Rational(1, 2)
val y = new Rational(3, 4)
val z = new Rational(5)

x.+(y)
-x
x.-(y)
x.<(y)
x.max(y)

// Infix notation
x + y
x - y
x < y
x max y