getDateTime <- function(data){
   
        #The step interval in this dataset is awkward. This function converts it into a time (beginning of interval)
        dates <- data$date
        int <- data$interval
        
        mint <- int %/% 100
        newint <- paste0(mint,":",int-mint*100)
        datetime <- strptime(paste0(dates,newint),format="%Y-%m-%d %H:%M")
        
        #merge this with the original data as a new column
        newdata <- data.frame(data, date.time=datetime)
        newdata
        
        
}