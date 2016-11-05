args <- commandArgs(trailingOnly = TRUE)
library(methods)
library(ggplot2)
library(reshape2)
library(plyr)


# Inputs
inFile  <- args[1]   	# List of data input name and data
outPath <- args[2]  	# Output path
main.title <- args[3]		# Graph Title
datapaths <- read.table(file=inFile, sep=",", header=F, stringsAsFactors=F)

n <- nrow(datapaths)/2
df <- data.frame()
cbPalette <- c("#000000", "#D55E00", "#CC79A7", "#0072B2", "#009E73", "#F0E442", "#56B4E9", "#E69F00")
scene <- list()
invisible(file.create(paste(outPath, ".csv", sep="")))

# Read Data
for (i in 1:n) {
	hdf <- read.csv(file=datapaths[i*2-1,3])
	fdf <- read.csv(file=datapaths[i*2,3])	
	scene <- c(scene, datapaths[i*2,2])

	# Dropping additional historical data points
	hdf <- hdf[,1:length(fdf)]
	tdf <- (fdf-hdf)/hdf*100
	tdf[,1] <- hdf[,1]

	odf <- tdf
	#print(odf[,2:length(fdf)])
	odf$median <- apply(tdf[,-1], 1, median)
	odf$mean <- apply(tdf[,-1], 1, mean)
	odf$scn <- paste(datapaths[i*2, 1],'-',datapaths[i*2,2])
	odf <- odf[,c(ncol(odf), 1:ncol(odf)-1)]
	suppressWarnings(write.table(odf, paste(outPath, ".csv", sep=""), append=TRUE, sep=",", row.names=FALSE))
	
	tdf$scn <- datapaths[i*2,2]
	tdf$hydro <- datapaths[i*2, 1]
	tdf <- tdf[tdf$returnYr < 101,]
	#tdf <- tdf[(tdf$returnYr == 10 | tdf$returnYr == 100), ]
	
	tdf <- melt(tdf, id=c("returnYr", "scn", "hydro"))
	df <- rbind(df, tdf)
}
scene <- unique(scene)
df$scn <- factor(df$scn, levels=scene)
df$hydro <- factor(df$hydro)
df$returnYr <- factor(df$returnYr)
levels(df$returnYr) <- c(paste(levels(df$returnYr), "Year Return"))
nfac <-nlevels(df$returnYr)
h <- (nfac*3) + 0.5

ymax <- max(df$value)
ymin <- min(df$value)


g <- ggplot(df, aes(hydro, value, color=hydro))
p <- g + 
	geom_boxplot(subset=.(scn!='BC-WRF' & scn!='RAW-WRF')) + 
	geom_point() + 
	facet_grid(returnYr~scn, scales="free_y") + 
	#scale_y_continuous(limits = c(ymin, ymax)) +  
	scale_colour_manual(values=cbPalette) + 
	theme_bw() + theme(legend.title = element_blank(), legend.position='None') + 
	theme(axis.title.y=element_text(vjust=1), plot.title=element_text(vjust=2, face="bold")) + 
	labs(x="", y="Percent Change (%)", title=main.title)
ggsave(file=paste(outPath, ".png", sep=""), plot=p, width=7, h=(h))


