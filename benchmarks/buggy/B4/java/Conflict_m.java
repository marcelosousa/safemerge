public class Conflict {
 int x,y;

 public int method(){
  int r = 0;
  y = 2;

  if (y==1)
  {
    r=y;
  } else {
    r=0;
  }

  return r;
 } 

}