import week4._

def show (e: Expr): String = e match {
  case Number(x) => x.toString
  case Sum(l, r) => show(l) + " + " + show(r)
  case Var(x) => x
  case Prod(l, r) => show(l) + " * " + show(r)
}

show(Sum(Number(1), Number(44)))
show(Sum(Var("x"), Var("y")))
show(Prod(Number(1), Number(44)))
show(Sum(Prod(Number(2), Var("x")), Var("y")))
show(Prod(Sum(Number(2), Var("x")), Var("y")))
