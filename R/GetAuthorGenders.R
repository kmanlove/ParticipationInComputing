GetAuthorGenders <- function(issn.in, sample.in)
{
  journal.test <- cr_journals(issn = issn.in, works = T, 
                            sample = sample.in, 
                            filter = c(from_pub_date = '2005-01-01')
                            ) 

  X <- sprintf("journals/%s/works", issns[1])
  sample <- sample.in
  myfilter <- c(from_pub_date = '2005-01-01')

  filter <- rcrossref:::filter_handler(myfilter)
  args <- rcrossref:::cr_compact(list(query = NULL, filter = filter, offset = NULL,
                                      rows = NULL, sample = sample, sort = NULL, 
                                      order = NULL))
  test.out <- rcrossref:::cr_GET(endpoint = X, args, todf = T) 
  # switch todf to T from default F to get all authors in list

  author.out <- test.out$message$items$author
  author.first <- vector("list", length(author.out))
  for(i in 1:sample){
    author.first[[i]] <- author.out[[i]]$given
  }

  author.first.all <- do.call("c", author.first)
  author.first.noinits <- strsplit(x = author.first.all[10], split = "..[:punct:]")
  author.first.noinits <- strsplit(x = author.first.all[10], split = "\\s")

  author.first.noinits <- rep(NA, length(author.first.all))

  for(i in 1:length(author.first.all)){
    author.first.noinits[i] <- strsplit(x = author.first.all[i], split = "\\s")[[1]]
  }

  author.genders <- gender(as.character(author.first.noinits))
  gender.out <- prop.female <- rep(NA, length(author.first.noinits))
  for(i in 1:length(author.first.noinits)){
    prop.female[i] <- author.genders[[i]]$proportion_female
    gender.out[i] <- author.genders[[i]]$gender
    print(i)
  }

  gender.nas <- table(is.na(gender.out) == T)["TRUE"]

  return(list(gender.out = gender.out, prop.female = prop.female, gender.nas = gender.nas))

}