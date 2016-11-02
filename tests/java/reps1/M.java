public class T {
  int foo(int debug) {
    double pi = 3.14;
    int rad = 2;
    if (debug > 0) {
      rad = 4;
    } else {
      rad = 2;
    } 
    double area = pi * Math.pow(rad, 2);
    int height = 10;
    double vol = height * area;
    output (area);
    output (vol);
    return 0; 
  }
} 
