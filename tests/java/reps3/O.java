public class T {
  int foo() {
    int k = 0, i = 1;
    int j = 0;
    while (i <= 100) {
      j = i * 2;
      while (j < 1000) {
        k = k + i * 10 + j;
        j = j + 1;
      }
      i = i + 1;
    }
    output (k);
    return 0;
  }
} 
