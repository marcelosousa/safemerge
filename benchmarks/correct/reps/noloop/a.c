void prog(int debug){
  int pi = 3;
  int rad;
  if (debug==1){
    rad = 4;
  }else{
    rad = 2;
  }
  int area = pi * (rad * rad);
  output(area);
}
