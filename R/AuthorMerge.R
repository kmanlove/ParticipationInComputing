

# function to merge multiple records for same author
AuthorMerge <- function(author.frame)
{
  unique.authors <- length(levels(factor(author.frame$AuthorID))) #  unique authors
  author.papers <- vector("list", unique.authors)
  author.unique.frame <- as.data.frame(matrix(NA, nrow = unique.authors, ncol = 11))
  names(author.unique.frame) <- c("AuthorID", "TotPapers", "TotUniv", "TotMath", 
                                  "TotStat", "TotEcol", "TotEvol", "TotEpi", "TotMed", 
                                  "TotVet", "TotBiol")
  
  for(i in 1:unique.authors)
  { # build dataframe with one row per author, and indicators author math, stat, ecol, evol, etc. affiliations over ALL affiliations for that author
    # extract all references for a given AuthorID
    author.papers[[i]] <- subset(author.frame, AuthorID == levels(factor(author.frame$AuthorID))[i])
    author.unique.frame$AuthorID[i] <- trim(levels(factor(author.frame$AuthorID))[i])
    author.unique.frame$TotPapers[i] <- length(levels(factor(author.papers[[i]]$Paper.Number)))
    author.unique.frame$TotUniv[i] <- ifelse(sum(author.papers[[i]]$Univ) >= 1, 1, 0)
    author.unique.frame$TotMath[i] <- ifelse(sum(author.papers[[i]]$Math) >= 1, 1, 0)
    author.unique.frame$TotStat[i] <- ifelse(sum(author.papers[[i]]$Stat) >= 1, 1, 0)
    author.unique.frame$TotEcol[i] <- ifelse(sum(author.papers[[i]]$Ecol) >= 1, 1, 0)
    author.unique.frame$TotEvol[i] <- ifelse(sum(author.papers[[i]]$Evol) >= 1, 1, 0)
    author.unique.frame$TotBiol[i] <- ifelse(sum(author.papers[[i]]$Biol) >= 1, 1, 0)
    author.unique.frame$TotEpi[i] <- ifelse(sum(author.papers[[i]]$Epi) >= 1, 1, 0)
    author.unique.frame$TotMed[i] <- ifelse(sum(author.papers[[i]]$Med) >= 1, 1, 0)
    author.unique.frame$TotVet[i] <- ifelse(sum(author.papers[[i]]$Vet) >= 1, 1, 0)
    author.unique.frame$TotCtr[i] <- ifelse(sum(author.papers[[i]]$Ctr) >= 1, 1, 0)
    #  print(i)
  } #i
  
  return(author.unique.frame)
}