import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Date;

public class Collect 
{
  private List<int[]> res;
  private int[] x;

/*
  public Collect(List<Integer> intArray, int[] otherArray)
  {
    _intArray = new ArrayList(intArray);
    _otherArray = new int[otherArray.length];
    for (int i = 0; i < otherArray.length; i++)
      _otherArray[i] = otherArray[i];
  } 

  public String toString()
  {
    String res = "Collect object\nintArray = " + this._intArray.toString() + "\notherArray = { ";
    int i = 0;
    for (i = 0; i < _otherArray.length - 1; i++)
      res += _otherArray[i] + ", ";

    res += _otherArray[i] + " }";  
    return res;
  }
*/

 public int test(int y,int z)
 {
   int i=0;

   i++;
   this.x[i] = 1;
   i++;
   this.x[i] = 2;
 
   if (x[1] == y)
     {
       return 1;
     }
   if (x[1] == z)
   {
     return 2;
   }
   return 0;
 }
/*
  public int test(int y)
  { 
    int i = 0;

    
    for (int k = 0; k <= x; k++)
    {
      r = r + this._otherArray[k];
    }

    return r; 
  }
*/
/*  
  public static void main(String args[]) 
  {
    List s = new ArrayList(Arrays.asList(1,2,3,4));
    int[] r = { 5, 6, 7, 8 };

    Collect f = new Collect(s,r);
    System.out.println("Hello World");
    System.out.println(f.toString());
    System.out.println(f.test(0));
  }
*/
}

