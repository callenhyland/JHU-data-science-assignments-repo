
rm(list=ls())
library(datasets)
data(ToothGrowth)
tg <- ToothGrowth
rm(ToothGrowth)

# change the dose variable to a factor (supp already is factor)
tg$dose <- as.factor(tg$dose)

# overview of dataset, number of factors:
table(tg$dose)
table(tg$supp)

# plot two-facet grid with boxplots for each dose
ggplot(data = tg) + geom_boxplot(aes(x= tg$dose, y=tg$len)) + facet_grid(.~tg$supp)

# It looks like there is a strong effect of dose on tooth growth for both supplement types
# however, it is unclear whether there is a difference between the two supplements
# In these experiments we will use alpha = 0.05

# t-test between supplement groups independent of dose:
t.test(len ~ supp, data = tg, alternative = "two.sided")
# As predicted, p-value is 0.06, so different is not significant

# There are two relevant tests that need to be done
# 1. Three pair-wise t-tests between the two supplements for each dose
# 2. Two sets of three par-wise t-tests between doses within each supplement group

pValues_supp <- NULL
doses <- levels(tg$dose)
for (i in 1:length(doses)){
    ss <- subset(tg, dose == doses[i])
    pValues_supp[i] <- t.test(len ~ supp, data = ss)$p.value
}
pValues_supp_adj <- p.adjust(pValues_supp, method="BH")
# p-values are 0.009537910 0.003115128 0.963851589
# for alpha = 0.5, there's a difference between supplement types at dose levels of 0.5 and 1.0
# but no difference between supplement types at dose level of 1.5

# compare effect of doses within each supplement group
oj <- subset(tg, supp == "OJ")
vc <- subset(tg, supp == "VC")
pValues_oj <- pairwise.t.test(oj$len, oj$dose)$p.value
pValues_vc <- pairwise.t.test(vc$len, vc$dose)$p.value

# combine the p-value vectors together and correct for multiple testing using BH method:
pValues_oj <- as.vector(pValues_oj)
pValues_vc <- as.vector(pValues_vc)
# Put all of the vectors together, then adjust for multiple comparisons
pValues_all <- c(pValues_supp, pValues_oj, pValues_vc)
pValues_adj <- p.adjust(pValues_all, method = "BH")

# remove NA's from the p-value vector:
pValues_adj <- pValues_adj[!is.na(pValues_adj)]

# Create labels for each of the comparisons:
labels <- c("0.5: OJ vs. VC", 
            "1.0: OJ vs. VC", 
            "2.0: OJ vs. VC", 
            "OJ: 0.5 vs. 1.0",
            "OJ: 0.5 vs. 2.0",
            "OJ: 1.0 vs. 2.0",
            "VC: 0.5 vs. 1.0",
            "VC: 0.5 vs. 2.0",
            "VC: 1.0 vs. 2.0")

# Print results to console:
for (i in 1:length(labels)){
    print(labels[i])
    print(pValues_adj[i])
}

