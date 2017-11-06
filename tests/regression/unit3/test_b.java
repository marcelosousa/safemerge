class UnitTest { 
  int aux;

  int f(int n)
  {
    int i = 0;
    while (i < n) 
    {
      i = i * n;
    }  
    this.aux = aux + n + 1;
    return i;
  }  
}
