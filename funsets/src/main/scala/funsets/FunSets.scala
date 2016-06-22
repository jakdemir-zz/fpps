package funsets


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
    def containsCurry(elem: Int)(elem2: Int): Boolean = {
      elem == elem2
    }

    containsCurry(elem)
  }

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = {
    def unionCurry(orig: Int): Boolean = {
      contains(s, orig) || contains(t, orig)
    }

    unionCurry
  }

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = {
    def intersectCurry(orig: Int): Boolean = {
      contains(s, orig) && contains(t, orig)
    }

    intersectCurry
  }

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = {
    def diffCurry(orig: Int): Boolean = {
      contains(s, orig) && !contains(t, orig)
    }

    diffCurry
  }

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = {
    def filterCurry(orig: Int): Boolean = {
      p(orig) && contains(s, orig)
    }

    filterCurry
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
      if (a < -bound) true
      else if (a <= bound && contains(s, a) && !p(a)) false
      else iter(a - 1)
    }
    iter(bound)
  }

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a < -bound) false
      else if (a <= bound && contains(s, a) && p(a)) true
      else iter(a - 1)
    }
    iter(bound)
  }


  def myExists(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a < -bound) false
      else if (a <= bound && contains(s, a) && p(a)) true
      else iter(a - 1)
    }
    iter(bound)
  }

  def boundedSet(boundLower: Int, boundUpper:Int): Set = {
    if (boundLower <= boundUpper) union(singletonSet(boundLower), boundedSet(boundLower + 1, boundUpper)) else (x: Int) => (false)
  }

  def isOdd(x: Int): Boolean = {
    x % 2 == 1
  }

  def isEven(x: Int): Boolean = {
    !isOdd(x)
  }

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = {
    def mapCurry(x: Int): Boolean = {
      exists(s, (y: Int) => f(y) == x)
    }

    mapCurry
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
