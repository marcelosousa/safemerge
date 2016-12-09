public class Example2 {
  public static int compute(int n) {
    if (n < 0) n*=-1;
    while(n != 1) {
      if(n % 2 == 0)
        n = n / 2;
      else
        n = 3∗n + 1;
    }
    return n;
  }
}
