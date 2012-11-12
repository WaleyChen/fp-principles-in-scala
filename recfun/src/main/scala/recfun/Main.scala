package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println(countChange(5, List(1, 2)))
    println(countChange(4,List(1,2)))
    println(countChange(300,List(5,10,20,50,100,200,500)))
    println(countChange(301,List(5,10,20,50,100,200,500)))
    println(countChange(300,List(500,5,50,100,20,200,10)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if(c == 0 || c == r ) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def head:String = chars.head.toString()
    if(chars.isEmpty) true
    else if(head == ")") false
    else if(head == "(") balance(closingParen(chars.tail, 1))
    else balance(chars.tail)
   }
  
  def closingParen(chars: List[Char], closingParensCount: Int): List[Char] = {
    def head:String = chars.head.toString()
    if(chars.isEmpty) ")".toList
    else if(head == "(") closingParen(chars.tail, closingParensCount + 1)
    else if(head == ")") {
      if(closingParensCount - 1 == 0) chars.tail
      else closingParen(chars.tail, closingParensCount - 1)
    }
    else closingParen(chars.tail, closingParensCount)
  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if(money == 0) 1
    else if(money < 0 || coins.isEmpty) 0
    else if(money == 0) 1
    else subtractChange(money, 0, coins)
    
  def subtractChange(money: Int, coinCount: Int, coins: List[Int]): Int = {
    val moneyLessChange = money - coins.head * coinCount
    if(moneyLessChange < 0) 0
    else countChange(moneyLessChange, coins.tail) + subtractChange(money, coinCount + 1, coins)
   }
}