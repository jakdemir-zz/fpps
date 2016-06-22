object Week0 {

  def sqrt(x: Double): Double = {
    sqrtIter(1.00, x)
  }

  def sqrtIter(guess: Double, x: Double): Double = {
    if (isGoodEnough(guess, x)) guess else sqrtIter(improve(guess, x), x)
  }

  def isGoodEnough(guess: Double, x: Double): Boolean = {
    abs(guess * guess - x) / x < 0.0001
  }

  def abs(x: Double): Double = {
    if (x < 0) -x else x
  }

  def improve(guess: Double, x: Double): Double = {
    (guess + x / guess) / 2
  }

  //  sqrt(1e60)
  //  sqrt(1e-6)
  //  sqrt(4)
  //  sqrt(1)
  //  sqrt(2)
  //  sqrt(0.01)
  //  sqrt(0.1e-20)
  //  sqrt(1.0e20)
  //  sqrt(1.0e50)

  def factorial(x: Int): Int = {
    factorialIter(x, 1)
  }

  def factorialIter(x: Int, result: Int): Int = {
    if (x <= 1) result else factorialIter(x - 1, x * result)
  }

  factorial(10)
}
