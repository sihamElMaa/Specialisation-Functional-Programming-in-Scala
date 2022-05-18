package recfun

object RecFun extends RecFunInterface :

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || r < 0)
      0
    else if (c == 0 || c == r)
      1
    else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def b2(chars: List[Char], count: Int): Boolean = {
      if (chars.nonEmpty) {
        chars.head match {
          case '(' => b2(chars.tail, count + 1)
          case ')' =>
            if (count == 0)
              false
            else
              b2(chars.tail, count - 1)
          case _ => b2(chars.tail, count)
        }
      }
      else {
        true
      }
    }

    b2(chars, 0)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    if (money < 1) 0
    else {
      def count(coins: List[Int], ways: Int, coinList: List[Int]): Int = {

        if (coins.nonEmpty) {
          val sum = coinList.sum + coins.head
          if (sum < money) {
            count(coins, ways, coinList ++ List(coins.head)) + count(coins.tail, ways, coinList)
          }
          else if (sum == money) {
            println(coinList ++ List(coins.head))
            ways + 1
          }
          else {
            ways
          }
        }
        else ways
      }

      count(coins, 0, List.empty[Int])

    }
  }
