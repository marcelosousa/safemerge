class UnitTest { 

  Test obj;

  int f(int x)
  {
    obj.setA(x);
    obj.setB(x+1);

    return x;
  }  
}
