package wiggly.poker

object MathUtil {
  def factorial(n: BigInt): BigInt =
    List.range[BigInt](1, n + 1).product

  def nPermK(n: BigInt, k: BigInt): BigInt =
    List.range[BigInt](n, n - k, -1).product

  def nCombK(n: BigInt, k: BigInt): BigInt =
    List.range[BigInt](n, n - k, -1).product / factorial(k)
}
