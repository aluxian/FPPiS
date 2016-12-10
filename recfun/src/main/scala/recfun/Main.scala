package recfun

object Main {

  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 && r == 0) 1
    else if (c > r || c < 0) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], depth: Int): Boolean =
      if (chars.isEmpty) depth == 0
      else if (depth < 0) false
      else if (chars.head == '(') loop(chars.tail, depth + 1)
      else if (chars.head == ')') loop(chars.tail, depth - 1)
      else loop(chars.tail, depth)

    loop(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def total(money: Int, coins: List[Int], pos: Int): Int =
      if (money < 0) 0
      else if (money == 0) 1
      else if (pos == coins.length && money > 0) 0
      else total(money - coins(pos), coins, pos) +
        total(money, coins, pos + 1)

    total(money, coins, 0)
  }

}
