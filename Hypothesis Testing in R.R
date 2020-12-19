
#####################################################################
#                      Hypothesis Testing
#####################################################################

install.packages("MASS")
library(MASS)


# One sample t-test
chem
chem=data.frame(chem)
t.test(chem, mu = 1, alternative = "greater")
t.test(chem, mu = 1, alternative = "two.sided")
t.test(chem, mu = 1, alternative = "less")


# C. Two sample t-test
cats
male_cats <- subset(cats, Sex == "M")
female_cats <- subset(cats, Sex == "F")
t.test(male_cats$Bwt, female_cats$Bwt, alternative = "two.sided")


# D. Shoes Dataset Paired t-test
shoes
shoes <- data.frame(shoes)
t.test(shoes$A, shoes$B, alternative = "greater", paired = T)


# E. Bacteria data set test of equal or given proportions
active = table(bacteria[which(bacteria$ap == 'a'),1])
placebo = table(bacteria[which(bacteria$ap == 'p'),1])
cat("active")
print(active)
cat("\nplacebo")
print(placebo)
prop.test(x=c(active[1],placebo[1]),
          n=c(active[1]+active[2],placebo[1]+placebo[2]), alternative='greater')


# F. Cats data set f-test
cats
var.test(male_cats$Bwt, female_cats$Bwt)


