source("./R/BuildAuthorFrame.R")
source("./R/DataPrep.R")
source("./R/GetAuthorAffils.R")
source("./R/AuthorMerge.R")

require("gender")
require("genderdata")


authors <- read.csv("./data/StatComputingPapers_27Mar2015.csv", header = T)
names(authors)
#author.data <- DataPrep(authors)

all.authors <- BuildAuthorFrame(data.frame.in = authors)
head(all.authors)
all.authors$First

author.genders <- vector("list", length(all.authors$First))
prop.male <- prop.female <- au.gender <- rep(NA, length(all.authors$First))
for(i in 1:length(prop.male))
#  for(i in 1:10)
  {
  author.genders[[i]] <- gender(as.character(all.authors$First[i]))
  prop.male[i] <- author.genders[[i]]$proportion_male[1]
  prop.female[i] <- author.genders[[i]]$proportion_female[1]
  au.gender[i] <- author.genders[[i]]$gender[1]
  print(i)
}

plot(au.gender)
table(au.gender)

all.authors$au.gender <- au.gender
all.authors$au.gender <- factor(ifelse(is.na(all.authors$au.gender) == T, "unclear", all.authors$au.gender))
all.authors$prop.female <- prop.female
all.authors$prop.male <- prop.male
all.authors <- all.authors[complete.cases(all.authors), ]
all.authors.reduced <- subset(all.authors, PubYear != "2005" & PubYear != "2006")
all.authors <- all.authors.reduced
all.authors$PubYear <- factor(all.authors$PubYear)


# Control papers

control.authors <- read.csv("./data/ControlPapers_27Mar2015.csv", header = T)
names(control.authors)
#author.data <- DataPrep(authors)

control.all.authors <- BuildAuthorFrame(data.frame.in = control.authors)
head(control.all.authors)
control.all.authors$First

control.author.genders <- vector("list", length(all.authors$First))
control.prop.male <- control.prop.female <- control.au.gender <- rep(NA, length(control.all.authors$First))
for(i in 1:length(control.prop.male))
  #  for(i in 1:10)
{
  control.author.genders[[i]] <- gender(as.character(control.all.authors$First[i]))
  control.prop.male[i] <- control.author.genders[[i]]$proportion_male[1]
  control.prop.female[i] <- control.author.genders[[i]]$proportion_female[1]
  control.au.gender[i] <- control.author.genders[[i]]$gender[1]
  print(i)
}

plot(control.au.gender)
table(control.au.gender)

control.all.authors$control.au.gender <- control.au.gender
control.all.authors$control.au.gender <- factor(ifelse(is.na(control.all.authors$control.au.gender) == T, "unclear", control.all.authors$control.au.gender))
control.all.authors$control.prop.female <- control.prop.female
control.all.authors$control.prop.male <- control.prop.male
control.all.authors <- control.all.authors[complete.cases(control.all.authors), ]
control.all.authors.reduced <- subset(control.all.authors, PubYear != "2005" & PubYear != "2006")
control.all.authors <- control.all.authors.reduced
control.all.authors$PubYear <- factor(control.all.authors$PubYear)


# END control papers
par(mfrow = c(1, 2))
barplot(table(control.all.authors$control.au.gender, control.all.authors$PubYear), legend = T, main = "Control papers")
barplot(table(all.authors$au.gender, all.authors$PubYear), legend = T, main = "Computing papers")
table(control.all.authors$control.au.gender)
table(all.authors$au.gender)

all.authors$au.rank <- as.numeric(as.character(all.authors$Position)) / as.numeric(as.character(all.authors$TotAuthors))
fems <- subset(all.authors, au.gender == "female")
males <- subset(all.authors, au.gender == "male")

par(mfrow = c(2, 2))
hist(as.numeric(as.character(fems$TotAuthors)), main = "", xlim = c(1, 12), col = "grey60", xlab = "Tot authors on \n female-authored papers")
hist(as.numeric(as.character(males$TotAuthors)), main = "", xlim = c(1, 12), col = "grey60", xlab = "Tot authors on \n male-authored papers")

hist(fems$au.rank, main = "", col = "grey60", xlab = "female author rank")
hist(males$au.rank, main = "", col = "grey60", xlab = "male author rank")
