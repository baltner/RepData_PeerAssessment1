fillNA <- function(d,means){
        
        #find the missing (NA) values
        filled<-d
        missing <- which(is.na(d$steps)==T)
        
        #loop through the missing values and insert the mean for that time interval
        for(i in 1:length(missing)){
                int <- d[missing[i],]$interval
                value <- means$x[means$Interval==int]
                filled[missing[i],]$steps <- value
        }
        return(filled)
}