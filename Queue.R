queue1 <- function(a, p, numarrivals) {
  
  wait_time  <- 0;          # waiting time excluding service time
  total_wait_time <- 0;     # total waiting time
  total_idle_time <- 0;     # total idle time
  total_arrival_time <- 0;  # total arrival time
  # Suppose that the queue already has a customer who arrived at time total_arrival_time=0
  # Name this customer as customer 0
  
  for (i in 1:numarrivals) {
    
    service_time <- rexp(1, 1/p);      # service time of customer i-1
    interarival_time <- rexp(1, 1/a);  # interarrival time between customer i-1 and i
    
    total_arrival_time <- total_arrival_time + interarival_time;  # advancing the clock from total_arrival_time to total_arrival_time+interarival_time
    wait_time  <- wait_time - interarival_time + service_time;    # wait_time-interarival_time+service_time is custoimer i's waiting time given that customer (i-1) waited for wait_time time
    # if wait_time>=0, it is the work found by customer i
    # if wait_time<0, it is the server idle time between finishing customer i-1 and starting customer i
    
    if (wait_time >= 0)
      total_wait_time <- total_wait_time + wait_time    # total_wait_time includes the waiting time of the (i)th customer 
    else {
      total_idle_time <- total_idle_time - wait_time;   # the (i)th customer does not experince wait but finds service idle
      wait_time  <- 0;                                  # since the queue is idle, customer i waiting time is zero
    }
  }
  Utilization=1-total_idle_time/total_arrival_time
  TimeInQueue=total_wait_time/numarrivals
  TimeInSystem=total_wait_time/numarrivals+p
  NumberInQueue=total_wait_time/total_arrival_time     
  NumberInSystem=total_wait_time/total_arrival_time+p/a
  #result <- data.frame(Utilization=1-total_idle_time/total_arrival_time,
  #                     TimeInQueue=total_wait_time/numarrivals,
  #                     TimeInSystem=total_wait_time/numarrivals+p,
  #                     NumberInQueue=total_wait_time/total_arrival_time,      
  #                     NumberInSystem=total_wait_time/total_arrival_time+p/a);
  result <- list("Expected average delay in queue" = TimeInQueue ,
                 "Expected average number of customers in queue "= NumberInQueue,
                 "Expected utilization" = Utilization)
  return(result)   
}