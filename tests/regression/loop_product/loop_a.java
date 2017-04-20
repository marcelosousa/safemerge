import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Date;

public class Loop 
{
  private int x;
  private ArrayList<Integer> intArray;

  public int test(int _x)
  {
    this.x = _x;
    int aux = 0;
    for (int k : intArray)
    {
      aux += k; 
    }
    return aux;
  }
 
}

