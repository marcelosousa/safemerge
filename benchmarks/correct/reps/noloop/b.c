void prog(int debug){
  int p = 3;
  int rad = 2;
  if (debug==1){
      rad = 4;
  }
  int area = p * (rad * rad);
  int height = 10;
  int vol = height*area;
  output(area);
  output(vol);
}

