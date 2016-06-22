package funsets

object Main extends App {

  import FunSets._

  println(contains(singletonSet(1), 1))
  printSet(boundedSet(-100, 100))
  printSet(map(boundedSet(-100, 100), (x: Int) => (x * x)))
  printSet(boundedSet(1, 1000))
  printSet(map(boundedSet(1, 1000), (x: Int) => (x - 1)))

}
