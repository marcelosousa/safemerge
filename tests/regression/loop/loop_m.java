import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Date;

public class Loop 
{
  private int x;
  private String name;
  private ArrayList<Integer> intArray;

  public Loop(int _x, String _name, List _intArray)
  {
    x = _x;
    name = _name;
    intArray = new ArrayList(_intArray);
  } 

  int getX()
  {
    return x;
  }

  String getName()
  {
    return name;
  }
 
  ArrayList<Integer> getList()
  {
    return intArray;
  }
  
  public String toString()
  {
    return "Foo object\n" + "x = " + this.x + "\n" + "name = " + this.name + "\nintArray = " + this.intArray.toString();
  }

  public int test(int _x)
  {
    this.x = _x;
    int aux = 0;
    for (int k : intArray)
    {
      aux += k + 1; 
    }
    return aux;
  }
 
  public static void main(String args[]) 
  {
    List s = new ArrayList(Arrays.asList(1,2,3,4));

    Loop f = new Loop(0,"test",s);
    System.out.println("Hello World");
    System.out.println(f.toString());
    f.test(0);
  }
}

