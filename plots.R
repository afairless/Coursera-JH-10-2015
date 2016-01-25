
medcol = 4
plotdata = traindata

# boxplots for each measurement by 'alternative' or 'conventional' medicine
dirname = "boxplot"
if (!file.exists(dirname)) dir.create(file.path(getwd(), dirname))

for (iter in (medcol + 1):dim(plotdata)[2]) {
     fname = paste(dirname, "/", iter, colnames(plotdata[iter]), ",", 
                   colnames(plotdata[medcol]), ",", dirname, ".png", sep = "")
     png(file = fname, width = 640, height = 512)     
     boxplot(plotdata[ , iter] ~ plotdata$medicine, col = 8, main = fname)
     dev.off()
}

# histograms for each measurement by 'alternative' or 'conventional' medicine
dirname = "density"
if (!file.exists(dirname)) dir.create(file.path(getwd(), dirname))

for (iter in (medcol + 1):dim(plotdata)[2]) {
     fname = paste(dirname, "/", iter, colnames(plotdata[iter]), ",", 
                   colnames(plotdata[medcol]), ",", dirname, ".png", sep = "")
     png(file = fname, width = 640, height = 512)     
     plot(density(plotdata[ , iter], na.rm = TRUE), lwd = 3, main = fname)
     lines(density(plotdata[plotdata$medicine == "conventional" , iter], na.rm = TRUE), col = 4, lwd = 2)
     lines(density(plotdata[plotdata$medicine == "alternative" , iter], na.rm = TRUE), col = 2, lwd = 2)
     mtext("blue = conventional, red = alternative, black = all", side = 1, padj = 3)
     dev.off()
}

# barplot of ratings by 'alternative' or 'conventional' medicine 
plotdata = expdata
starsbymed = cbind(as.data.frame(table(plotdata[plotdata$medicine == "conventional", ]$stars)),
                   as.data.frame(table(plotdata[plotdata$medicine == "alternative", ]$stars)))
starsbymed = starsbymed[ , -c(1, 3)]
starsmedprop = starsbymed
starsmedprop[ , 1] = starsmedprop[ , 1] / sum(starsmedprop[ , 1])
starsmedprop[ , 2] = starsmedprop[ , 2] / sum(starsmedprop[ , 2])
colnames(starsmedprop) = c("conventional", "alternative")

barplot(as.matrix(starsbymed), beside = T, ylim = c(0, 1))
abline(h = 0)


# plot of example graphs for report

medcol = 4
plotdata = traindata

par(mfrow = c(2, 2))

iter = 13
fname = "characters per word"

boxplot(plotdata[ , iter] ~ plotdata$medicine, col = 8, main = fname)

plot(density(plotdata[ , iter], na.rm = TRUE), lwd = 3, main = fname)
lines(density(plotdata[plotdata$medicine == "conventional" , iter], na.rm = TRUE), col = 4, lwd = 2)
lines(density(plotdata[plotdata$medicine == "alternative" , iter], na.rm = TRUE), col = 2, lwd = 2)
mtext("blue = conventional, red = alternative, black = all", side = 1, padj = 3, cex = 0.8)

iter = 29
fname = "diversity, Shannon index"

boxplot(plotdata[ , iter] ~ plotdata$medicine, col = 8, main = fname)

plot(density(plotdata[ , iter], na.rm = TRUE), lwd = 3, main = fname)
lines(density(plotdata[plotdata$medicine == "conventional" , iter], na.rm = TRUE), col = 4, lwd = 2)
lines(density(plotdata[plotdata$medicine == "alternative" , iter], na.rm = TRUE), col = 2, lwd = 2)
mtext("blue = conventional, red = alternative, black = all", side = 1, padj = 3, cex = 0.8)


# plot of tree for report
library(rpart.plot)

par(mfrow = c(1, 1))
prp(rpartmodelover$finalModel)
