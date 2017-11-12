class UnitTest { 
  int f()
  {
    int x = -1;
    int y = 2;
    int ret = 0;

    if (x > 0) {
      ret = y;
    } else {
      ret = -y;
    }
    
    return ret;
  }  
}
