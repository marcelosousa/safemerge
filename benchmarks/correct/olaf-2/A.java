public class Example2 {
  int n;
  public static int compute(int a) {
    while(a != 1) {
      if(a % 2 == 0)
        a = a / 2;
      else
        a = 3∗a + 1;
    }
    return a;
  }
}
