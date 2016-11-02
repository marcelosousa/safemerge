public class T {
  int foo(int n, int[] a) {
    int i = 2;
    while (i <= n) {
      a[i] = 0;
      i = i+2;
    }
    output (a);
    return 0;
  }
} 
