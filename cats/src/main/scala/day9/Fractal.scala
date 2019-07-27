package day9

object Fractal extends App{
  val xss = List(List(1), List(2, 3), List(4))
  println(xss.flatten)
  println(xss.foldLeft(List(): List[Int]){_ ++ _})

  val o1 = Some(None: Option[Int]): Option[Option[Int]]
  val o2 = Some(Some(1): Option[Int]): Option[Option[Int]]
  val o3 = None: Option[Option[Int]]

  println(o1.foldLeft(None: Option[Int]){(_, y) => y})
  println(o2.foldLeft(None: Option[Int]){(_, y) => y})
  println(o3.foldLeft(None: Option[Int]){(_, y) => y})
}
