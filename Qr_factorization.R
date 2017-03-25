quicksort <- function(a) {
   n = length(a)
   if (n > 1){
     p = floor(n/2)
     pivot = a[p]
     asmaller = c()
     alarger = c()
     for (i in (1:n)){
       if (i != p){
         if (a[i] <= pivot){
           asmaller = rbind(asmaller,a[i])
           }
         else{
           alarger = rbind(a[i],alarger)
           }
         }
       }
     asmaller = quicksort(asmaller)
     alarger = quicksort(alarger)
   a = rbind(asmaller,a[p],alarger)
   
   }
   return (a)
}
   
   