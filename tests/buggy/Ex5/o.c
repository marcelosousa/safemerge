int toGPA(char grade)
{
     int r;
     switch(grade)
     {
        case 'A' : r=4;
        case 'B' : r=3;
        case 'C' : r=2;
        case 'D' : r=1;
        case 'F' : r=0;    

     }
     return r;
}