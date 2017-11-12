class UnitTest { 

  Test obj;

  int f(int x)
  {
    obj.setA(x+1);
    obj.setB(x+1);

    return x;
  }  
}
