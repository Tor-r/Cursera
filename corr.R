
corr <- function(directory, threshold = 0) {
    files <- list.files(directory, pattern="*.csv", full.names = TRUE)
    spots_data <- lapply(files, read.csv)
    spots_data <- do.call(rbind, spots_data)
    spots_data <- spots_data[complete.cases(spots_data), ]
    
    v <- numeric()
    
    for (i in 1:length(files)) {
        data <- subset(spots_data, spots_data[, 4] == i)
        n_rows <- nrow(data)
        
        if (n_rows > threshold) {
            c <- cor(data$sulfate, data$nitrate)
            #c <- round(c, digits = 4)
            v <- c(v, c)
        }
    }
    return(v)
}
