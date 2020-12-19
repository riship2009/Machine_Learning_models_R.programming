#####################################################################
#                      Inferential Statistics
#####################################################################


forestfires <- read.csv(file.choose())
View(forestfires)


#One sample t-test for temperature greater than 18 degree celsius
t.test(forestfires$temp, mu =18, alternative = "greater")


#Two sample t-test wind speed equal to 4km/h
Month_Aug <- forestfires[which(forestfires$month=='aug'),]
Month_Sep <- forestfires[which(forestfires$month=='sep'),]
t.test(Month_Aug$wind, Month_Sep$wind, mu=4, alternative = "two.sided")


#Paired t-test for temperature equal to 9 degree celsius
Month_Apr <- forestfires[which(forestfires$month=='apr'),]
Month_Dec <- forestfires[which(forestfires$month=='dec'),]
t.test(Month_Apr$temp, Month_Dec$temp, mu=9, alternative = "two.sided", paired = T)


#Test of equal or given proportions for rainfall in aug
forestfires$rainfall <- ifelse(forestfires$rain == 0, 'n', 'y')
temp_rainfall <- table(Month_Aug$month, Month_Aug$rainfall)
temp_rainfall <- as.matrix(temp_rainfall[c('aug'),])
colnames(temp_rainfall) <- c('aug')
temp_rainfall <- t(temp_rainfall)
prop.test(temp_rainfall, length(temp_rainfall), alternative = "two.sided", conf.level = 0.95, correct = TRUE)


#f-test
var.test(Month_Aug$temp, Month_Sep$temp)
