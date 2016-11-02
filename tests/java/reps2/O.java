public class T {
  int foo(int n, int[] a) {
    int i = 1;
    while (i <= n) {
      if (even(i)) {
        a[i] = 0;
        i = i+1;
      }
    }
    output (a);
    return 0;
  }
} 
