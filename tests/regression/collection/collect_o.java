import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Date;

public class Collect 
{
//  private List<Integer> _intArray;
  private int[] _otherArray;
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
  public int test(int x)
  { 
    int k = 0; //this._intArray.get(x);
    int m = this._otherArray[x];
    this._otherArray[0] = 1;

    return k + m; 
  }
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

