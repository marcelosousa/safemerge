public class T {

private int a;
private int b;
private int c;
private int d;
private int e;
private int f;


int main() {
 int res = 0;
 if (a > 0)
 {
   if (b > 0)
   {
     if (c > 0)
     {
      if (d > 0)
      {
        if (e > 0)
        {
          if (f > 0)
          {
            res = 1;
          }
        }
      }
     }
   }
 }
 else 
 {
   if (b > 0)
   {
     if (c > 0)
     {
       if (d > 0)
       {
        if (e > 0)
        {
          if (f > 0)
          {
            res = -1;
          }
        }
       }
     }
   }
 }

 // read b and c, write to a and b
 if (b > 0)
 {
   a = 1;
   if (c > 0)
   {
     b = 1;
   }
   else 
   {
     b = -1;
   }
 }
 else 
 {
   a = -1;
   if (c > 0)
   {
     b = 1;
   }
   else 
   {
     b = -1;
   }
 }

 // read c and d, write to a and b
 if (c > 0)
 {
   a = 1;
   if (d > 0)
   {
     b = 1;
   }
   else 
   {
     b = -1;
   }
 }
 else 
 {
   a = -1;
   if (d > 0)
   {
     b = 1;
   }
   else 
   {
     b = -1;
   }
 }

 // read e and f, write to a and b
 if (e > 0)
 {
   a = 1;
   if (f > 0)
   {
     b = 1;
   }
   else 
   {
     b = -1;
   }
 }
 else 
 {
   a = -1;
   if (f > 0)
   {
     b = 1;
   }
   else 
   {
     b = -1;
   }
 }

 return res;
}
}
