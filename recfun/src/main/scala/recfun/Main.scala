package recfun

object Main {
  def main(args: Array[String]) {
//    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row)
//        print(pascal(col, row) + " ")
//      println()
//    }

//    println(pascal(0, 2))
//    println(pascal(1, 2))
//    println(pascal(1, 3))
      println(countChange(5, List(1,2)))
  }

  /**
   * Exercise 1
   *     1
   *    1 1
   *   1 2 1
   *  1 3 3 1
   * 1 4 6 4 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    balanceIter(chars, 0)
  }

  def balanceIter(chars: List[Char], balanceCount: Int): Boolean = {
    if (balanceCount < 0) false
    else if (chars.isEmpty) if ((balanceCount % 2) != 0) false else true
    else if (chars.head == '(') balanceIter(chars.tail, balanceCount + 1)
    else if (chars.head == ')') balanceIter(chars.tail, balanceCount - 1)
    else balanceIter(chars.tail, balanceCount)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

}
