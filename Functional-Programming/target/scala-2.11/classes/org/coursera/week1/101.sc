"Hello, World!"

// Call by value
def square(x: Double): Double =
  x * x

square(2)

// Call by name
def square2(x: => Double): Double =
  x * x

square2(2)

// Conditional expressions
def abs(x: Double): Double =
  if (x >= 0) x else -x

abs(-2)

// Benefits of using Call by name
def loop: Boolean = loop

def and(x: Boolean, y: => Boolean): Boolean =
  if (x) y else false

and(x=false, y=false)
and(x=false, y=true)
and(x=true, y=false)
and(x=true, y=true)
and(x=false, loop)

// Finding sqrt of a number using Newton's method
def sqrt(x: Double): Double = {
  def sqrtIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrtIter(improve(guess))

  def isGoodEnough(guess: Double): Boolean =
    abs(guess * guess - x) / x < 0.001

  def improve(guess: Double): Double =
    (guess + x / guess) / 2

  sqrtIter(1.0)
}

sqrt(2)
sqrt(4)
sqrt(1e-6)
sqrt(1e60)

// Functions to illustrate tail recursion and recursion
def gcd(a: Int, b: Int): Int =
  if (b == 0) a else gcd(b, a % b)

def factorial(n: Int): Int =
  if (n == 1) 1 else n * factorial(n-1)

def fact(n: Int): Int = {
  def loop(acc: Int, n: Int): Int =
    if (n == 1) acc else loop(acc * n, n-1)

  loop(1, n)
}

fact(5)
