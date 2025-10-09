# Day 1 Example Plot

viremia <- read.csv("viremia_data_full.csv")

View(viremia)

colnames(viremia) <- c("Bird", "n", "Species", "Family", "Order", "1", "3", "4", "6")

#choose some colors
cols <- c("black", "grey", rainbow(26[4:26])
          
 #Plot by species
 plot(c(1,3,4,6), as.numeric(viremia[1, 6:9, drop = TRUE]),
      type = "l", 
      lwd = 2, 
      ylim = range(as.numeric(unlist(viremia[, 6:9])), na.rm = TRUE),
      xlab = "Day Postinfection",
      ylab = "Log PFU/ml Serum")
 
 # Loop through rows 2 to n
 for (i in 2:nrow(viremia)) {
   lines(c(1,3,4,6),                      # x-values
         as.numeric(viremia[i, 6:9, drop = TRUE]),  # y-values
         lwd = 2,
         col = cols[i])
 }
 
?round
 round(1029, digits = -3)
 round(4.2839191, digits = 2)
round (pi, digits = 6)
?mean
mean(c(2, 7, 10, 29, 20, 10, 11, 10, 9, 20), trim = 0.3) 
mode (c(2, 2, 2, 2, 2, 10, 29, 21, 2, 4, 5, 1, 5, 6, 9))

?knitr

