package day15

sealed trait Person
case object John extends Person
case object Mary extends Person
case object Sam extends Person

sealed trait Breakfast
case object Eggs extends Breakfast
case object Oatmeal extends Breakfast
case object Toast extends Breakfast
case object Coffee extends Breakfast

object CatTheory extends App{
  val a: Set[Person] = Set[Person](John, Mary, Sam)

  //arrow
  val favoriteBreakfast: Person => Breakfast = {
    case John => Eggs
    case Mary => Coffee
    case Sam => Coffee
  }

  //endomorphism
  val favoritePerson: Person => Person = {
    case John => Mary
    case Mary => John
    case Sam => Mary
  }

  //identity arrow
  println(identity(John))

  val favoritePersonBreakfast = favoriteBreakfast compose favoritePerson
  println(favoritePersonBreakfast(John))
  println(favoritePersonBreakfast(Mary))

  val johnPoint: Unit => Person = { case () => John}
  val johnFav = favoriteBreakfast compose johnPoint
  println(johnFav())
}
