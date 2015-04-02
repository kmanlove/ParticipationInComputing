# from Scott Chamberlain's blogpost http://ropensci.org/blog/2013/03/15/r-metadata/

# install_github('rmetadata', 'ropensci') # uncomment to install
# install_github("ropensci/rcrossref")
require(rmetadata)
require(rcrossref)
require("gender")

issns <- c("1467-9868", # Journal Roy Stat Soc Series B
                        # Annals of Mathematics
           "0003-486X", # Annals of Statistics
           "0090-5364", # Vital and health statistics. Series 13, Data from the National Health Survey
           "0083-2006", # Journal of Statistical Software
           "1548-7660", # Vital and health statistics. Series 10, Data from the National Health Survey
           "0083-1972", # Journal of Business and Economic Statistics
           "1537-2707", # Bioinformatics
           "1367-4811", # JASA
           "1537-274X", # Biometrika
           "1464-3510", # Statistical science
           "0883-4237", # Annals of Probability
           "0091-1798", # Statistica Sinica
           "1017-0405", # Journal of Informetrics
           "1751-1577", # Biostatistics
           "1468-4357", # Bayesian Analysis
           "1936-0975", # Probability Theory and Related Fields
           "1432-2064", # Journal of Comp and Graphical Stats
           "1537-2715", # Annals of Applied Probability
           "1050-5164", # Statistics in Medicine
           "1097-0258", # Journal of Machine Learning research
           "1533-7928", # Scandanavian Journal of Statistics
           "1467-9469", # Statistics Surveys
           "1935-7516", # Annales de l'institut Henri Poincare (B) Probability and Statistics
           "0246-0203", # Biometrics
           "1532-7906"  # Multivariate behavioural research
  )

source("./R/GetAuthorGenders.R")
get.tests <- vector("list", length(issns))

for(i in 1:length(issns)){
  get.tests[[i]] <- GetAuthorGenders(issn.in = issns[i], sample.in = 1000)
  print(i)
}