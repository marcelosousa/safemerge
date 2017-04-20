import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Date;

public class Cond 
{
 private int x;
 public int test(int y)
 {
   int i = 0;

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

