val nums: List[Int] = List(1, 2, 3, 4, 5)

nums.length
nums.last
nums.init
nums take 2
nums drop 2
nums(2)

val nums2: List[Int] = List(6, 7, 8, 9, 10)

nums ++ nums2
nums.reverse
nums updated (2, 10)
nums indexOf 3
nums contains 3

val pair = ("answer", 42)
val (label, value) = pair

def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = {
      (xs, ys) match {
        case (xs, Nil) => xs
        case (Nil, ys) => ys
        case (x :: xs1, y :: ys1) =>
          if (ord.lt(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }
    }

    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

val num = List(2, -4, 5, 7, 1)
msort(num)

val fruits = List("Apple", "Pineapple", "Orange", "Banana")
msort(fruits)

num filter (x => x > 0)
num filterNot (x => x > 0)
num partition (x => x > 0)

num map (x => math.abs(x))

num takeWhile (x => x > 0)
num dropWhile (x => x > 0)
num span (x => x > 0)
