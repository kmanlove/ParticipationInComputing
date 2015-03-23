#-- Journal rankings through time --#
jo.ranks <- read.csv("./data/SampledJosQuantsThroughTime.csv", header = T)
jo.ranks$status.ind <- ifelse(jo.ranks[, 16] == "not", 1, 2)

# consider: each year, number of journals moving up in ranking vs. staying the same or moving down
jo.ranks$QuantCh1 <- ifelse(jo.ranks$Quant2002 - jo.ranks$Quant2000 < 0, 1, 0)
jo.ranks$QuantCh2 <- ifelse(jo.ranks$Quant2004 - jo.ranks$Quant2002 < 0, 1, 0)
jo.ranks$QuantCh3 <- ifelse(jo.ranks$Quant2006 - jo.ranks$Quant2004 < 0, 1, 0)
jo.ranks$QuantCh4 <- ifelse(jo.ranks$Quant2008 - jo.ranks$Quant2006 < 0, 1, 0)
jo.ranks$QuantCh5 <- ifelse(jo.ranks$Quant2010 - jo.ranks$Quant2008 < 0, 1, 0)
jo.ranks$QuantCh6 <- ifelse(jo.ranks$Quant2012 - jo.ranks$Quant2010 < 0, 1, 0)

computing <- as.matrix(subset(jo.ranks, KRMClass == "comput"))
control <- as.matrix(subset(jo.ranks, KRMClass == "not"))


par(mfrow = c(1, 2))
plot(jitter(5 - as.numeric(as.character(computing[1, 18:24])), 1) ~ seq(2000, 2012, by = 2), main = "Computational/Software", type = "l", ylim = c(.5, 4.5), ylab = "Quartile of Journal's Rank", xlab = "", col = rgb(1, 0, 0, alpha = .5), lty = 1, lwd = 2, yaxt = "n")
for(i in 2:dim(computing)[1]){
  lines(jitter(5 - as.numeric(as.character(computing[i, 18:24])), 1) ~ seq(2000, 2012, by = 2), col = rgb(1, 0, 0, alpha = .5), lty = 1, lwd = 2)
}
lines(lowess())
axis(side = 2, at = c(1:4), labels = c("4", "3", "2", "1"))
#leg.text <- c("Computational", "Control")
#legend("bottomleft", leg.text, col = c("red", "black"), lty = c(1, 1), lwd = c(2, 2), bty = "n")
plot(jitter(5 - as.numeric(as.character(control[1, 18:24])), 1) ~ seq(2000, 2012, by = 2), main = "Control", type = "l", ylim = c(.5, 4.5), ylab = "", xlab = "", col = rgb(0, 0, 0, alpha = .5), lty = 1, lwd = 2, yaxt = "n")
for(i in 2:dim(control)[1]){
  lines(jitter(5 - as.numeric(as.character(control[i, 18:24])), 1) ~ seq(2000, 2012, by = 2), col = rgb(0, 0, 0, alpha = .5), lty = 1, lwd = 2)
}
axis(side = 2, at = c(1:4), labels = c("4", "3", "2", "1"))

long.form <- reshape(jo.ranks, 
                     varying = c("QuantCh1", "QuantCh2", "QuantCh3",
                                 "QuantCh4", "QuantCh5", "QuantCh6"), 
                     idvar = "Title", 
                     v.names = "Quant",
                     direction = "long")

require(lme4)
long.form.susc <- subset(long.form, select = c("Rank", "KRMClass", "time", "Quant"))
long.form.susc <- long.form.susc[complete.cases(long.form.susc), ]
logistic.test <- glmer(Quant ~ time * KRMClass + (1 | Rank), data = long.form.susc, family = "binomial")
logistic.test.2 <- glmer(Quant ~ time + KRMClass + (1 | Rank), data = long.form.susc, family = "binomial")
logistic.test2 <- glm(Quant ~ time * KRMClass, data = long.form.susc, family = "binomial")

long.form.susc$classquant <- paste(long.form.susc$KRMClass, "_", long.form.susc$Quant)
barplot(table(long.form.susc$classquant, long.form.susc$time), beside = T)
