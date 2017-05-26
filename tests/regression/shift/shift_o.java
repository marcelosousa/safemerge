import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Date;

public class Shift 
{
 private int[] x;

 public int test(int y,int z)
 {
   int i=0;
   int r=0;
   int n=3;
   i++;
   this.x[i] = 1;
   i++;
   this.x[i] = 2;

   for (int j = 0; j < n; j++){
     if(this.x[j] == y)
     {
       r = 1;
     } else
     {
       r = 2;
     }
   }
   return r;
 }
}

