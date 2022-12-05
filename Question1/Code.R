#https://medium.com/analytics-vidhya/feature-extraction-and-brand-perceptual-map-using-pca-in-r-31477002fdf2

sink("./Output.txt",append=T)
# http://goo.gl/IQl8nc
#read data
rating <- read.csv("http://goo.gl/IQl8nc")


#scale data
rating.sc <- rating
rating.sc[,1:15] <- scale(rating[,1:15])

#compute mean
brand.mean <- aggregate(rating.sc[,1:15], list(rating.sc[,16]), mean)

rownames(brand.mean) <- paste("",letters,sep="")[1:10]
(brand.mean <- brand.mean[,-1] )
rating.pc <- prcomp(brand.mean, scale=TRUE)
summary(rating.pc)

jpeg("rating_pc.jpeg",quality = 100)
plot(rating.pc, type="l")
dev.off()

#positioning
jpeg("Brand_Positioning_6.jpeg",quality = 100)
biplot(rating.pc, main="Brand Positioning", cex=c(1,0.7))
dev.off()

brand.dist <- dist(brand.mean)
(brand.mds <- cmdscale(brand.dist))

#differentiation
jpeg("Brand_Positioning_2.jpeg",quality = 100)
plot(brand.mds, type="n", main="Brand Differentiation")
rownames <- paste("",letters,sep="")[1:10]
text(brand.mds, rownames, cex=1)
dev.off()

sink()

# To RUn Execute the following command
# Rscript Code.R