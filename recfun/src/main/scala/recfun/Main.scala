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
      if (c < 0 || r < 0 || c > r) 0
      else if (c == 0) return 1
      else pascal(c - 1,  r - 1) + pascal(c, r - 1);
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def bal(list: List[Char], count: Int): Boolean = {
        if (list.isEmpty) true
        else if (list.head == '(') bal(list.tail, count + 1)
        else if (list.head == ')') {
          if (count == 0)  false
          else bal(list.tail, count - 1)
        } else bal(list.tail, count)
      }

    bal(chars, 0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def countC(mny: Int,  cs: List[Int], count: Int): Int = {
        if (cs.isEmpty) count
        else {
          val m = mny - cs.head
          if (m < 0)  count
          else if (m == 0) count + 1
          else {
            val cnt = countC(m, cs, count)
            countEach(m, cs.tail, cnt)
          }
        }
      }

    def countEach(mon: Int, cs: List[Int], count: Int): Int = {
      if (cs.isEmpty) return count
      val cnt = countC(mon, cs, count)
      countEach(mon, cs.tail, cnt)
    }

      countEach(money, coins , 0)
   }
  }
