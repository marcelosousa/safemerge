void prog(){
 int i=1;
 while(i<N){
   if (even(i)) {
     a[i] = 0;
   }
   i = i+1;
 }
 output(a);
}
