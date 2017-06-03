import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Date;

// Unit test for conditionals 

public class Cond 
{
 private int x;

 public int test1(int y)
 {
   int i = 1;

   if (i > 0)
   {
     if (y >= 0)
     {
       i+=y;
     }
     else 
     {
       i++;
     }
   }
 
   return i;
 }
}

