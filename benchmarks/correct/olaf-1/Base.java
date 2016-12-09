public class Example {
  public static long fib(int n) {
    if (n <= 1) return n;
    else return fib(n−1) + fib(n−2);
  }

  public static long fac(int n) {
    long result = 1;
    for (int i = 1; i <= n; i++)
      result ∗= i;
    return result;
  }
}
