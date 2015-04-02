require("tools")
require("gender")
require("genderdata")
require("Hmisc")
require("stringr")
require("ggplot2")
require("scales")
require("dplyr")
require("XML")
require("RCurl")
require("reshape2")

# Thanks Dirk: http://stackoverflow.com/a/11561793/1036500
getPackagesWithTitle <- function() {
  contrib.url(getOption("repos")["CRAN"], "source") 
  description <- sprintf("%s/web/packages/packages.rds",  getOption("repos")["CRAN"])
  con <- if(substring(description, 1L, 7L) == "file://") {
    file(description, "rb")
  } else {
    url(description, "rb")
  }
  on.exit(close(con))
  db <- readRDS(gzcon(con))
  rownames(db) <- NULL
  return(db)
}

## Set default repo
local({r <- getOption("repos"); 
       r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)})

db <- getPackagesWithTitle()

db_df <- data.frame(db)
maintainer_year_unclean <- data.frame(name = unname(db_df$Maintainer),
                                      year = as.integer(substr(unname(db_df$Published), 1, 4)))

clean_up <- function(x){
  # get rid of punctuation
  x1 <- gsub("[[:punct:]]", " ", x)
  # remove leading space before first name
  x2 <- str_trim(x1 , side="left") 
  # get first name only
  x3 <- unlist(lapply(x2, first.word))
  # gender function only takes lower case
  x4 <- tolower(x3)
  return(x4)
}
# apply function to data
maintainer_year_clean <- maintainer_year_unclean
maintainer_year_clean$name <- clean_up(maintainer_year_clean$name)

# compute gender of package maintainers (this takes some time!)
cran_genders_with_nas <- gender(maintainer_year_clean$name)
# remove NA (where we can't be sure of the gender because
# we've only got a first initial, or similar)
cran_genders_with_nas <- unlist(lapply(cran_genders_with_nas, function(i) i$gender))
idx <- !is.na(cran_genders_with_nas)
cran_genders <- cran_genders_with_nas[idx]

# plots...
# overall counts
counts <- data.frame(female = cran_genders == "female" ,
                     male = cran_genders == "male")
counts_l <- melt(counts + 0)
ggplot(counts_l, aes(variable, value)) +
  geom_bar(stat = "identity") +
  theme_minimal(base_size = 14) +
  ggtitle("CRAN package maintainers by gender") +
  ylab("Number of packages") +
  xlab("gender")

# overall ratio
all <- length(cran_genders)
props <- round(data.frame(female_prop = sum(cran_genders == "female")/all,
                          male_prop = sum(cran_genders == "male")/all), 2)
props_l <- melt(props)
ggplot(props_l, aes(variable, value)) +
  geom_bar(stat = "identity") +
  theme_minimal(base_size = 14) +
  ggtitle("CRAN package maintainers by gender") +
  ylab("Proportion of all packages") +
  xlab("gender")

# change over time
# # 'published' date is only most recent version, date
# # of first appearence must come from 'Old sources' archive list
# # have to webscrape on http://cran.r-project.org/src/contrib/Archive/$PACKAGE This has the side-effect of only considering packages 
# that have at least one archived version - first version packages 
# are excluded. That's tends to favour the inclusion of older packages
# and ones under more active development. 

pckg_data <- vector("list", length(nrow(db_df)))
for(i in 1:nrow(db_df)){
  pckge <- db_df$Package[i]
  theurl <- paste0("http://cran.r-project.org/src/contrib/Archive/", db_df$Package[i])
  # print(paste0(i, " ", theurl)) # only for interactive use to know when to come back!
  result <- try(
    tab <- readHTMLTable(theurl),
  ); if(class(result) == "try-error") next;
  
  # get the date of the earliest archive
  first_date <- strptime(tab[[1]]$`Last modified`[3], "%d-%b-%Y %H:%M")
  pckg_data[[i]] <- data.frame(year = first_date, pckge = pckge)
}

# add year on
the_year  <- as.POSIXlt(strptime(maintainer_year_clean$year, "%Y"))$year + 1900 
cran_genders_by_year <- cbind(cran_genders_with_nas, the_year) 
# omit NA
cran_genders_by_year <- data.frame(na.omit(cran_genders_by_year))

# plot absolute values
ggplot(cran_genders_by_year, aes(as.factor(the_year), fill = cran_genders_with_nas)) +
  geom_bar(binwidth = 0.5) +
  scale_x_discrete(breaks = unique(cran_genders_by_year$the_year ), 
                   labels = unique(cran_genders_by_year$the_year )) +
  theme_minimal(base_size = 14) +
  xlab("year") +
  ggtitle("CRAN package maintainers by gender") +
  ylab("Number of packages") +
  scale_fill_discrete(name = "gender")

# table of counts and proportions
gender_table <- cran_genders_by_year %>%
  group_by(the_year) %>%
  select(the_year, cran_genders_with_nas) %>%
  summarise(
    count_ml = sum(cran_genders_with_nas == "male"),
    ml = sum(cran_genders_with_nas == "male")/length(cran_genders_with_nas),
    count_fl = sum(cran_genders_with_nas == "female"),
    fl = sum(cran_genders_with_nas == "female")/length(cran_genders_with_nas)
  )

# factor nuisance...
gender_table$the_year <- as.numeric(as.character(gender_table$the_year))

ggplot(gender_table, aes(the_year,  fl)) +
  geom_text(aes(label = paste0("n=", count_ml + count_fl)), size = 4) +
  theme_minimal(base_size = 14) +
  xlab("Year") +
  ylab("Proportion of packages in each year") +
  ggtitle("Change over time in gender ratio of R package maintainers \n(with n = number of packages in each year)") +
  scale_x_continuous(breaks = unique(gender_table$the_year), 
                     labels = unique(gender_table$the_year)) 

overall_prop <- round(sum(gender_table$count_fl) / (sum(gender_table$count_ml) + sum(gender_table$count_fl)), 2)

# sanity check for interactive work
x <- identical(
  round(sum(gender_table$count_fl) / (sum(gender_table$count_ml) + sum(gender_table$count_fl)), 2), 
  # from above
  xx <- round(props$female_prop, 2))

# Who are the pioneering women of R?
maintainter_year_unclean_no_na_female <- maintainer_year_clean[idx & cran_genders_with_nas == 'female', ]
table(maintainter_year_unclean_no_na_female[maintainter_year_unclean_no_na_female$year %in% 2006:2010,])