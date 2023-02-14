base_conversion <- function(num,base)
  
{
  vec <- c()
  
  while(num>0)
  {
    if (num%%base == 0)
    {
      vec <- c(0, vec)
      
      num <- num%/%base     
    }
    else
    {
      vec <- c(num%%base, vec)
      num <- num%/%base 
    }        
  }
  son<-paste(vec,collapse ="")
  return(c(son))
  
  
}

fuel_cost <- function(fuel_consumption,trip_length,gas_price)
{
  ghost<-trip_length/100
  value<-fuel_consumption*gas_price*ghost
  a<-round(value,digits=2)
  
  
  return(a)
}

fuel_cost_new<-function(fuel_consumption, trip_length, gas_price, top_speed,cruising_speed) 
{
  fuel_consumption<-fuel_consumption*cruising_speed/top_speed
  ghost<-trip_length/100
  value<-fuel_consumption*gas_price*ghost
  a_d<-round(value,digits=2)
  return(a_d)
}

travel_cost<-function(fuel_consumption, top_speed, gas_price, distances, speed_limits, num_cities)
{
  fuel_consumption<-fuel_consumption*speed_limits/top_speed
  ghost<-distances/100
  value<-fuel_consumption*gas_price*ghost*2
  a_d<-value
  sıralı<-sort(a_d)
  order<-c()
  don<-c()  
  a<-order(distances)
  vec<-0
  n<-length(distances)
  if(n>=num_cities)
  {
    
    
    for(i in sıralı[1:num_cities])
    {
      order<-paste(order,as.character(match(i,a_d)))
      vec<-vec+i
      
      
      
    }
    
    don<-c(order,round(vec))
    return(noquote(don))
    
  }
  
  else{
    return("n is too large")
  }
  
}
