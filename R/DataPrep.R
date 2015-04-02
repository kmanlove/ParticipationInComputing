DataPrep <- function(data.frame)
{
  no.cite.papers.in <- c(1, 494, 603, 858)
  names(data.frame)[5] <- "Keep"
  data.frame <- subset(data.frame, Keep == 1)
  dim(data.frame) # 1632 papers
  data.frame$Source <- factor(data.frame$Source)
  data.frame$AnnualizedCitationRate <- data.frame$TimesCited / (2014 - data.frame$PubYear)
  data.frame <- data.frame[-no.cite.papers.in, ] 
  
  return(data.frame)
}