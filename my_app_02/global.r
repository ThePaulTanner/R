stocksOfInterest          <-  c("AAPL", "ADT")
stocksOfInterestDataSet   <- list()
stocksOfInterestStartDate <- list()

v=0
for(i in stocksOfInterest)
{
  v=v+1 
  thestock = stocksOfInterest[v]
  basicURL= "http://ichart.finance.yahoo.com/table.csv?s=CHANGE&d=5&e=3&f=2014&g=d&a=.csv"
  basicURL = sub("CHANGE", thestock , basicURL)
  t = stocksOfInterest[v]
  if(nchar(t)==1)
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
  cat(c(t,": loaded : "));
  cat(c(basicURL,"\n"));
  
  stocksOfInterestStartDate[i] = stocksOfInterestDataSet[[i]][nrow(stocksOfInterestDataSet[[i]]),1]
  stocksOfInterestDataSet[[i]] <- read.csv(basicURL,header=TRUE,colClasses = c("Date","numeric","numeric","numeric","numeric","numeric","numeric"))
  stocksOfInterestDataSet[[i]]$Volitility <- (stocksOfInterestDataSet[[i]]$High - stocksOfInterestDataSet[[i]]$Low)
  stocksOfInterestDataSet[[i]]$RollingAverage <- (stocksOfInterestDataSet[[i]]$High-stocksOfInterestDataSet[[i]]$High)
  
  MemoryLength = 5
  
  for(j in 1:(nrow(stocksOfInterestDataSet[[i]]) - MemoryLength))
  {
    
    stocksOfInterestDataSet[[i]][(j+MemoryLength),9] <- mean(stocksOfInterestDataSet[[i]][(j+MemoryLength-5):(j+MemoryLength-1),6])
    
  }   
}
