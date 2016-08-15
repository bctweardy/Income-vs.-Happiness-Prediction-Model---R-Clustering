demographics = read.csv("C:/Users/Brad_2/Desktop/DemographicsData2.csv")

sapply(demographics, mean, na.rm = FALSE)
sapply(demographics, sd, na.rm = FALSE)
sapply(demographics, median, na.rm = FALSE)
sapply(demographics, min, na.rm = FALSE)
sapply(demographics, max, na.rm = FALSE)

X=demographics[, -11]
Y=demographics[, 11]

pca.cor=princomp(X, cor = T)
summary(pca.cor)

pca.cor <- princomp(X, retx=TRUE, center=TRUE, scale.=TRUE)
sd <- pca.cor$sdev
loadings <- pca.cor$rotation
scores <- pca.cor$X

## Determines cutoff and prints loadings with cutoff requirement to remove "unimporant" loadings.
cut <- sqrt(1/ncol(X))
print(cut)
loadings(pca.cor, digits = 3, cutoff = cut || cutoff < -1*cut, sort = TRUE)

## Plots bar chart with red line that intercepts bars on bar chart. 
## The line is the point at which all ten principal components would
## equally contribute to the variance.
var <- sd^2
var.percent <- var/sum(var)
dev.new()
barplot(var.percent, main = "Scree Plot of Principal Components", xlab="Principal Components", 
        ylab="Percent Variance", names.arg=1:length(var.percent), las=1, ylim=c(0,max(var.percent)), 
        col="red")
abline(h=1/ncol(X), col="blue")


## Principal components 1 and 2 individual % variance explained and combined varinace explained.
var.percent[1:3]
sum(var.percent[1:3])

## Perform regression analysis on PC1 and PC2
dim(pca.cor$scores)
pca1 = pca.cor$scores[,1]
pca2 = pca.cor$scores[,2]
pca3 = pca.cor$scores[,3]
pclm = lm(Y ~ pca1 + pca2 + pca3)
summary.lm(pclm, correlation = T)

## Took out Math and Auto because of high correlations with other predictors
lm2 = lm(Y ~ Science + Arith + Word + Parag + Numer + Coding + Mechanic + Elec, data = demographics)
summary.lm(lm2, correlation = T)

## Added Math and Auto back in to see what happens
lm3 = lm(Y ~ Science + Arith + Word + Parag + Numer + Coding + Mechanic + Elec + Math + Auto, data = demographics)
summary.lm(lm3, correlation = T)

## Here Income2005 is modeled by everything but Income2005
lm4 = lm(Income2005 ~ ., data = demographics)
summary.lm(lm4, correlation = T)