class UnitTest { 

  Test obj;

  int f(int x)
  {
    obj.setA(x+1);
    set(x+2);

    return x;
  }  
  
  void set(int x){
    obj.setB(x);
  }
}
