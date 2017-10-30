public class Test {

 public int method(int grade){
  int r = 0;

  if (grade == 1)
  { 
    r = 1;
  } 
  
  if (grade == 2)
  { 
    r = 2;
  } 

  if ((grade != 1) && (grade != 2))
  {
    r = 0;
  }

  return r;
 } 

}
