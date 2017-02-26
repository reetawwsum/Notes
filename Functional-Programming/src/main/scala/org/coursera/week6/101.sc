val nums: Vector[Int] = Vector(0, 1, 2, 3, 4)

val r: Range = 1 until 5

val s: String = "Hello, World"

s flatMap (x => List(".", x))

def isPrime(n: Int): Boolean =
  (2 until n) forall (d => n % d != 0)

// Find all pairs whose sum is prime number and individual number is less than n
val n = 7

(1 until n) flatMap (i =>
  (1 until i) map (j => (i, j))) filter (pair =>
    isPrime(pair._1 + pair._2))

for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

val x: Set[Int] = (1 to 5).toSet

val roman = Map("I" -> 1, "V" -> 5, "X" -> 10)
roman("I")
roman get "I"
