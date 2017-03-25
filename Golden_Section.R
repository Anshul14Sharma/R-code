#f <- function(x) {1 - log(x) * exp(-x^2)}
golden.section.search = function(f, a, b, tolerance)
{
  golden.ratio = 2/(sqrt(5) + 1)
  
  ### Use the golden ratio to set the initial test points
  x1 = b - golden.ratio*(b - a)
  x2 = a + golden.ratio*(b - a)
  
  ### Evaluate the function at the test points
  f1 = f(x1)
  f2 = f(x2)
  
  iteration = 0
  
  while (abs(b - a) > tolerance)
  {
    iteration = iteration + 1
    cat('', '\n')
    cat('Iteration #', iteration, '\n')
    cat('f1 =', f1, '\n')
    cat('f2 =', f2, '\n')
    
    if (f2 > f1)
      # then the minimum is to the left of x2
      # let x2 be the new upper bound
      # let x1 be the new upper test point
    {
      cat('f2 > f1', '\n')
      ### Set the new upper bound
      b = x2
      cat('New Upper Bound =', b, '\n')
      cat('New Lower Bound =', a, '\n')
      ### Set the new upper test point
      ### Use the special result of the golden ratio
      x2 = x1
      cat('New Upper Test Point = ', x2, '\n')
      f2 = f1
      
      ### Set the new lower test point
      x1 = b - golden.ratio*(b - a)
      cat('New Lower Test Point = ', x1, '\n')
      f1 = f(x1)
    } 
    else 
    {
      cat('f2 < f1', '\n')
      # the minimum is to the right of x1
      # let x1 be the new lower bound
      # let x2 be the new lower test point
      
      ### Set the new lower bound
      a = x1
      cat('New Upper Bound =', b, '\n')
      cat('New Lower Bound =', a, '\n')
      
      ### Set the new lower test point
      x1 = x2
      cat('New Lower Test Point = ', x1, '\n')
      
      f1 = f2
      
      ### Set the new upper test point
      x2 = a + golden.ratio*(b - a)
      cat('New Upper Test Point = ', x2, '\n')
      f2 = f(x2)
    }
  }
  
  ### Use the mid-point of the final interval as the estimate of the optimzer
  cat('', '\n')
  cat('Final Lower Bound =', a, '\n')
  cat('Final Upper Bound =', b, '\n')
  estimated.minimizer = (a + b)/2
  cat('Estimated Minimizer =', estimated.minimizer, '\n')
}