import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Date;

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

  public int hello(int _x)
  {
    return _x+1;
  }

  public int test(int x)
  {
    int y = this.x;
    int z = this.hello(y);
    z = hello(y);

    return z;
  }
}

