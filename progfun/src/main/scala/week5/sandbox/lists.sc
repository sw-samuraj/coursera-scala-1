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

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}

def removeAt(n: Int, xs: List[Int]) = (xs take n) ::: (xs drop n + 1)

isort(l)
isort(ll)
isort(lll)

last(l)
last(ll)
last(isort(ll))
last(lll)
last(isort(lll))

init(l)
init(ll)
init(lll)
init(isort(lll))
// init(List())

concat(l, ll)
concat(ll, isort(ll))
isort(concat(ll, lll))

reverse(List())
reverse(l)
reverse(ll)
reverse(isort(lll))

removeAt(0, l)
removeAt(1, l)
removeAt(0, ll)
removeAt(1, ll)
val sl = isort(lll)
removeAt(0, sl)
removeAt(1, sl)
removeAt(2, sl)
removeAt(3, sl)
removeAt(4, sl)
removeAt(-1, sl)
