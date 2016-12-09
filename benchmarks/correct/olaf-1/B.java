public class Example {
  public static long fac(int n) {
    long result = 1;
    if (n>1)
      for (int i = 2; i <= n; i++)
        result ∗= i;
    return result;
  }

  public static long fib(int n) {
    if (n <= 1) return n;
    else return fib(n−1) + fib(n−2);
  }
}
