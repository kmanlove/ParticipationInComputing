# from Scott Chamberlain's blogpost http://ropensci.org/blog/2013/03/15/r-metadata/

# install_github('rmetadata', 'ropensci') # uncomment to install
# install_github("ropensci/rcrossref")
require(rmetadata)
require(rcrossref)

crossref_search(query = c("renear", "palmer"), year = 2010)[, 1:3]

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

journal.test <- cr_journals(issn = issns[1], works = T, sample = 100, filter = c(from_pub_date = '2005-01-01'))
# journal name is in the container.title field of journal.text
test0 <- cr_cn(journal.test$data$DOI[1], format = "ris")
# strip out author names
authornames.front <- strsplit(test0[1], split = "\nAU")[[1]]
number.authors <- length(authornames.front) - 1
authornames.last <- strsplit(authornames.front[number.authors + 1], split = "\nPY")[[1]][1]

individ.authornames <- c(authornames.front[2 : max(2, (number.authors - 1))], authornames.last)
authornodash <- unlist(strsplit(individ.authornames, split = c("-")))
authorfirst <- strsplit(authornodash, split = c(","))
getauthorfirst <- 

test <- cr_works(query = "Journal of the Royal Statistical Society Series B", type = "journal-article")
test2 <- cr_cn(doi = test$data$DOI[1])

cr.test <- cr_r(container.title = journal.test$data$container.title[6])

#sampled.dois <- journal.test$data$DOI
sampled.refs <- cr_r(sample = 10, query = "Journal of the Royal Statistical Society Series B", )
ref.extract <- cr_cn(dois = "10.1126/science.169.3946.635")
ref.extract <- cr_cn(dois = sampled.dois[1:10])
, format = "citeproc-json")


doi.extract <- cr_r(issn = issns[1])

cr.search.free.test <- cr_search_free(query = "Journal of the Royal Statistical Society. Series B: Statistical Methodology")
doi.extract <- cr_r(cr.search.free.test$doi)
