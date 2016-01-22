void prog(){
 int i=1;
 while(i<N){
   if (even(i)) {
     a[i] = 0;
   }
   i = i+1;
 }
 i = 1;
 int sum = 0;
 while(i<=N){
     sum = sum + a[i];
     i=i+1;
 }
 output(a);
 output(sum);
}
