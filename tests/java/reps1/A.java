public class T {
  int foo(int debug) {
    double pi = 3.14;
    int rad;
    if (debug > 0) {
      rad = 4;
    } else {
      rad = 2; 
    } 
    double area = pi * Math.pow(rad, 2);
    output (area);
    return 0;
  }
} 
