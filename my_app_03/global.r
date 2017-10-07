stocksOfInterest          <-  c("EBAY") 
q <- 3 #create q Colomns
stocksOfInterestDataSet   <<- list()
InitialInvestment = 10000
MinimumInvestmentTime = 180 # (6 Months)
Dollars     <<- InitialInvestment
StocksHeld  <<- 0
TotalWealth <<- 0  
Decisions <- list()
vv = length(stocksOfInterest)
Dollars     <<- InitialInvestment
StocksHeld  <<- 0
TotalWealth <<- 0  

v=0
for(i in stocksOfInterest)
{
  v=v+1 
  thestock = stocksOfInterest[v]
  basicURL= "http://ichart.finance.yahoo.com/table.csv?s=CHANGE&d=5&e=3&f=2014&g=d&a=.csv"
  basicURL = sub("CHANGE", thestock , basicURL)
  t = stocksOfInterest[v]
    (nchar(t)==1)
  {
    t = paste0(t, "   ")
  }
  if(nchar(t)==2)
  {
    t = paste0(t, "  ")
  }
  if(nchar(t)==3)
  {
    t = paste0(t, " ")
  }
  
  stocksOfInterestDataSet[[i]] <- read.csv(basicURL,header=TRUE,colClasses = c("Date","numeric","numeric","numeric","numeric","numeric","numeric"))
  stocksOfInterestDataSet[[i]]$Volitility <- (stocksOfInterestDataSet[[i]]$High - stocksOfInterestDataSet[[i]]$Low)
  RollingAverageHighColomnNames <- c(paste("RollingAverageHigh", 1:q,sep = ""))
  RollingAverageLowColomnNames  <- c(paste("RollingAverageLow",  1:q,sep = ""))
  
  for(r in 1:q)
  {
    for(j in 1:(nrow(stocksOfInterestDataSet[[i]])))
    {   
      stocksOfInterestDataSet[[i]][j,(9)] <- FALSE
    }      
  }
  for(r in 1:q)
  {
    for(j in 1:(nrow(stocksOfInterestDataSet[[i]])))
    {   
      stocksOfInterestDataSet[[i]][j,(10)] <- FALSE
    }      
  }
  
  for(r in 1:q)
  {
   for(j in 1:(nrow(stocksOfInterestDataSet[[i]])-r))
    {   
     stocksOfInterestDataSet[[i]][j,(10+r)] <- mean(stocksOfInterestDataSet[[i]][(j+1):(j+r),3])     
    }      
  }
  
 for(r in 1:q)
  {
    for(j in 1:(nrow(stocksOfInterestDataSet[[i]])-r))
    {      
      stocksOfInterestDataSet[[i]][j,(10+q+r)] <- mean(stocksOfInterestDataSet[[i]][(j+1):(j+r),4])
    }    
  }
 
 Decisions <- array(FALSE, c(nrow(stocksOfInterestDataSet[[i]]),1,2,q,q,vv))
                   
 for(b in 1:q)
 {
   for(s in 1:q)
   {
     for(j in 1:(nrow(stocksOfInterestDataSet[[i]])))
     {   
       if((stocksOfInterestDataSet[[i]][j,3]) < (stocksOfInterestDataSet[[i]][j,(10+b)]) & !is.na(stocksOfInterestDataSet[[i]][j,(10+q)]) & !is.na(stocksOfInterestDataSet[[i]][j,3]))
       {
         #BUY         
         Decisions[j,1,1,b,s,v] = TRUE
         #print(paste("Buy[",j,",1,1," ,b,s,v))
       }
       if((stocksOfInterestDataSet[[i]][j,4]) > (stocksOfInterestDataSet[[i]][j,(10+q+s)]) & !is.na(stocksOfInterestDataSet[[i]][j,(10+q+r)]) & !is.na(stocksOfInterestDataSet[[i]][j,4]))
       {
         #SELL
         Decisions[j,1,2,b,s,v] = TRUE 
         #print(paste("Sell[",j,",1,1," ,b,s,v))
       }
     }
   }   
 }
 Decisions[1,1,2,b,s,v] = TRUE
 
 
 cat(c(t,": loaded : "))
 cat(c(basicURL,"\n"))
}




