import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;

public class Foo 
{
  private int x;
  private String name;
  private List intArray;

  public Foo(int _x, String _name, List _intArray)
  {
    x = _x;
    name = _name;
    intArray = new ArrayList(_intArray);
  } 

  public Foo(int _x)
  {
    x = _x;
    name = "uninitialized";
    intArray = new ArrayList();
  }

  public Foo(String _name)
  {
    x = 0;
    name = _name;
    intArray = new ArrayList();
  }

  public Foo(List _intArray)
  {
    x = 0;
    name = "uninitialized";
    intArray = new ArrayList(_intArray);
  }

  public Foo()
  {
    x = 0;
    name = "uninitialized";
    intArray = new ArrayList();
  }

  int getX()
  {
    return x;
  }

  String getName()
  {
    return name;
  }
 
  List getList()
  {
    return intArray;
  }
  
  public String toString()
  {
    return "Foo object\n" + "x = " + this.x + "\n" + "name = " + this.name + "\nintArray = " + this.intArray.toString();
  }
  
  public static void main(String args[]) 
  {
    List s = new ArrayList(Arrays.asList(1,2,3,4));

    Foo f = new Foo(s);
    System.out.println("Hello World");
    System.out.println(f.toString());
  }
}
