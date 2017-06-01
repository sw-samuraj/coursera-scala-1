val l = List(1)
val ll = List(2, 1)
val lll = List(7, 3, 9, 2)

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
}

def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}

isort(l)
isort(ll)
isort(lll)

last(l)
last(ll)
last(isort(ll))
last(lll)
last(isort(lll))
