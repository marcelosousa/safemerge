public class T {
  int foo() {
    int k = 0, i = 1;
    int j = 0;
    int twoi = 2;
    while (i <= 100) {
      j = twoi;
      while (j < 1000) {
        k = k + i * 10 + j;
        j = j + 1;
      }
      twoi = twoi + 2;
      i = i + 1;
    }
    output (k);
    return 0;
  }
} 
