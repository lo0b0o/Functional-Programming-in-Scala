package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do print(s"${pascal(col, row)} ")
      println()

  /** Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c == r || c == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /** Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceHelper(cs: List[Char], count: Int): Boolean =
      if (cs.isEmpty) count == 0
      else if (cs.head == '(') count >= 0 && balanceHelper(cs.tail, count + 1)
      else if (cs.head == ')') count > 0 && balanceHelper(cs.tail, count - 1)
      else count >= 0 && balanceHelper(cs.tail, count)
    balanceHelper(chars, 0)
  }

  /** Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else {
      val takeHead = countChange(money - coins.head, coins)
      val notTakeHead = countChange(money, coins.tail)
      takeHead + notTakeHead
    }
