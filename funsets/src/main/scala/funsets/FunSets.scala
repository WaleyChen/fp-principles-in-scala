package funsets

import common._

/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = {
    def char_fn(int: Int) = 
      int == elem
    char_fn
  }
    
    

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = {
    def char_fn(int: Int) =
      contains(s, int) || contains(t, int)
    char_fn
  }

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` or `t`.
   */
  def intersect(s: Set, t: Set): Set = {
    def char_fn(int: Int) =
      contains(s, int) && contains(t, int)
    char_fn
  }

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = {
    def char_fn(int: Int) =
      contains(s, int) && !contains(t, int)
    char_fn
  }
  
  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = {
    def char_fn(int: Int) =
      p(int) && contains(s, int)
    char_fn
  }

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if(a == bound + 1) true
      else if (!p(a) && contains(s, a)) false
      else iter(a + 1)
    }
    iter(-1000)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = {
    def opp_bool (f: Int => Boolean): (Int => Boolean) = {
      def opp(int: Int): Boolean = {
        !p(int)
      }
      opp
    }
    
    def exist(s: Set): Boolean = {
      !forall(s, opp_bool(p))
    }
    exist(s)
  }

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = {
    def char_fn(int: Int): Boolean = {
      def map_inner(int_1: Int): Boolean = {
        f(int_1) == int
      }
      
      exists(s, map_inner)
    }
    char_fn
  }

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
