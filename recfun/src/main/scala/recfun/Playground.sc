def countChange(money: Int, coins: List[Int]): Int = {
  def total(money: Int, coins: List[Int], pos: Int): Int =
    if (money < 0) 0
    else if (money == 0) 1
    else if (pos == coins.length && money > 0) 0
    else total(money - coins(pos), coins, pos) +
      total(money, coins, pos + 1)

  total(money, coins, 0)
}

countChange(0, List(1, 2, 3))
countChange(5, List())
countChange(4, List(1, 2))

countChange(4, List(1, 2)) // 3
countChange(300, List(5, 10, 20, 50, 100, 200, 500)) // 1022
countChange(301, List(5, 10, 20, 50, 100, 200, 500)) // 0
countChange(300, List(500, 5, 50, 100, 20, 200, 10)) // 1022
