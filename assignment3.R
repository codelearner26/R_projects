reverse_returns <- function(dataframe) {
  
  bilbo_baggins <- dataframe[nrow(dataframe):1, ]
  
  for (ghost in 2:nrow(bilbo_baggins)) {
    donut <- (bilbo_baggins[ghost,] - bilbo_baggins[ghost-1,]) / bilbo_baggins[ghost-1,]
    a<-round(donut,4)
    bilbo_baggins[ghost-1,] <- a
  }
  return(bilbo_baggins)
}

highest_returns <- function(dataframe) {
  
  ortalama <- rowMeans(dataframe)
  ortalama <- round(ortalama, 4)
  ortalama <- sort(ortalama, decreasing = TRUE)
  manipule<-ortalama[-1]
  return(head(manipule, 3))
}

portfolio_returns <- function(dataframe) {
  
  wall_street <- data.frame(Return=0, Max_stock=0 , Min_stock=0 )
  
  
  for (i in 1:nrow(dataframe)) {
    
    Return <- dataframe[i,]
    
    
    en_buyuk<- which.max(Return)
    
    
    en_kucuk <- which.min(Return)
    
    
    port_return <- Return[ en_buyuk] - Return[ en_kucuk]
    
    Max_stock<-names(en_buyuk)
    Min_stock<-names(en_kucuk)
    
    
    wall_street[i,] <- c(round(port_return, 4), Max_stock, Min_stock)
  }
  
  return(wall_street)
}

parking<-function(vehicle_list) {
  w <- 4
  h <- 4
  parking_lots <- matrix(0, nrow = h, ncol = w,byrow = T)
  row_pointer <- 1
  column_pointer <- 1
  vehicle_pointer <- 1
  while (vehicle_pointer <= length(vehicle_list) && row_pointer < 5) {
    car_number1 <- vehicle_list[vehicle_pointer]
    if (car_number1 == 1) {
      parking_lots[row_pointer, column_pointer] =car_number1
      column_pointer = column_pointer + 1
      if (column_pointer == 5) {
        row_pointer = row_pointer + 1
        if (row_pointer == 5) {
          break
        }
        column_pointer <- 1
      }
    } else if (car_number1 == 2) {
      if (column_pointer == 4) {
        row_pointer = row_pointer + 1
        if (row_pointer == 5) {
          break
        }
        column_pointer <- 1
      }
      if (column_pointer > 1 && !is.na(parking_lots[row_pointer , column_pointer-1])) 
      {
        parking_lots[row_pointer, column_pointer] = NA
        column_pointer = column_pointer + 1
      }
      parking_lots[row_pointer, column_pointer] = car_number1
      column_pointer <- column_pointer + 1
      if (column_pointer < 5) {
        parking_lots[row_pointer, column_pointer] <- NA
        column_pointer <- column_pointer + 1
        if (column_pointer == 5) {
          row_pointer <- row_pointer + 1
          if (row_pointer == 5) {
            break
          }
          column_pointer <- 1
        }
      } else {
        row_pointer <- row_pointer + 1
        if (row_pointer == 5) {
          break
        }
        column_pointer <- 1
      }
    }
    vehicle_pointer <- vehicle_pointer + 1
  }
  return(parking_lots)
}
