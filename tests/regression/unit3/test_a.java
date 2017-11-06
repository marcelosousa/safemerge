class UnitTest { 
  int aux;

  int f(int n)
  {
    int i = 0;
    while (i < n) 
    {
      i = i * n * 2;
    }  
    this.aux = aux + n;
    return i;
  }  
}
