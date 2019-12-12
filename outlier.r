outlier <- function (x,method="mean",addthres=FALSE){
  if (method=="boxplot") {
    Q1 <- quantile(x, 0.25)
    Q3 <- quantile(x, 0.75)
    IntQ <-Q3-Q1
    dtf <<- data.frame(ID=seq.int(length(x)), obs=x, outlier=x<Q1-1.5*IntQ | x>Q3+1.5*IntQ)
    midp <<- median(x)
    lower <<- Q1-1.5*IntQ
    upper <<- Q3+1.5*IntQ
    outliern <<- length(which(dtf=="TRUE"))
    } else {}
  if (addthres==TRUE) {
    p <- ggplot(dtf, aes(x=ID, y=obs, label=ID)) + geom_point(aes(colour=outlier)) + geom_text_repel(data = subset(dtf, outlier=="TRUE"), aes(label = ID), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x=paste("observation ID number\n number of outliers detected=", outliern, "\n( outlier detection method=", method, ")"), y="observation value") + geom_hline(yintercept = midp, colour="black", linetype = "longdash") + geom_hline(yintercept = lower, colour="black", linetype = "longdash") + geom_hline(yintercept = upper, colour="black", linetype = "longdash")
    } else {
  p <- ggplot(dtf, aes(x=ID, y=obs, label=ID)) + geom_point(aes(colour=outlier)) + geom_text_repel(data = subset(dtf, outlier=="TRUE"), aes(label = ID), size = 2.7, colour="black", box.padding = unit(0.35, "lines"), point.padding = unit(0.3, "lines")) + labs(x=paste("observation ID number\n( outlier detection method=", method, ")"), y="observation value") #requires 'ggrepel'
  }
  return(p)
}


new_replace_outliers <- function(k) {
  new = k
  i <- 1
  while (i <= length(ncol(new))) {
    Q1 <- quantile(new[,i], 0.25)
    Q3 <- quantile(new[,i], 0.75)
    IntQ <-Q3-Q1
    lower <- Q1-1.5*IntQ
    upper <- Q3+1.5*IntQ
    j <- 1
    while (j <= length(new[,i])) {
      if (new[j,i] < lower) {new[j,i] <- lower}
      if (new[j,i] > upper) {new[j,i] <- upper}
      j<- j+1
    }
    i <- i + 1
  }
  
  return(new)
}

