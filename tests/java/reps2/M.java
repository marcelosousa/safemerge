public class T {
  int foo(int n, int[] a) {
    int i = 2;
    while (i <= n) {
      a[i] = 0;
      i = i+2;
    }
    i = 1;
    int sum = 0;
    while (i <= n) {
      sum = sum + a[i];
      i = i+1;
    }
    output (a);
    output (sum);
    return 0;
  }
} 
