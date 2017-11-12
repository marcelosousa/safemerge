class UnitTest { 

  Test obj;

  int f(int x)
  {
    obj.setA(x);
    obj.setB(x+2);

    return x;
  }  
}
