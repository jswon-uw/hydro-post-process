args <- commandArgs(trailingOnly = TRUE)
options(scipen=10)
library(methods)
library(ggplot2)
library(reshape2)
library(optparse)
library(grid)

option_list <- list(
  make_option(c("-d", "--delim"), action="store", default=",", type='character', help="Specify deliminator for parsing input file."),
  make_option(c("-t", "--title"), action="store", default="", type='character', help="Option to specifiy a title."),
  make_option(c("-c", "--color"), action="store_true", default=FALSE, help="Fixes color to provided value"),
  make_option(c("-f", "--filePath"), action="store", default=NA, type='character', help="File with filepath, name and color (optional)"),
  make_option(c("-o", "--outPath"), action="store", default=NA, type='character', help="Output filepath"),
  make_option(c("-s", "--shape"), action="store_true", default=FALSE, help="Fixes shape to provided value"),
  make_option(c("-z", "--zero"), action="store_true", default=FALSE, help="Forces minimum to 0")  
  )

opt <- parse_args(OptionParser(option_list=option_list))
main.title=opt$t
# Inputs
#inFile  <- args[1]   # List of data input name and data
#outPath <- args[2]   # Output path
inFile <- opt$f
outPath <- opt$o
delim <- opt$d
datapaths <- read.table(file=inFile, sep=delim, header=F, stringsAsFactors=F)
n <- nrow(datapaths)
df <- data.frame()

# Define colors
cbPalette <- c("#000000", "#CC79A7", "#0072B2", "#009E73", "#F0E442", "#56B4E9", "#E69F00", "#D55E00")
m <- length(cbPalette)
tPalette <- c(cbPalette[1], cbPalette[(m-(n-2)):m])
if(opt$c) {
	cbPalette <- c("#000000", "#56B4E9", "#E69F00", "#D55E00", "#CC79A7", "#0072B2", "#009E73", "#F0E442")
	tPalette <- c()
	for (i in 1:n) {
		j <- datapaths[i,3]
		if(is.null(j)) {
			j <- i
		}
		j <- j %% length(cbPalette)
		tPalette <- c(tPalette, cbPalette[j])
	}
}

# Define shapes
if(opt$s) {
	shapes <- datapaths[,4]
} else {
	shapes <- rep(1,n)
}

# Read Data
scene <- list()
for (i in 1:n) {
  tdf <- read.csv(file=datapaths[i,2], sep=",", header=T)
  tdf$scn <- datapaths[i,1]
  scene <- c(scene, datapaths[i,1])
  
  tdf <- melt(tdf, id=c("returnYr", "scn"))
  df <- rbind(df, tdf) 
}

df$scn <- factor(df$scn, levels=scene)
df$returnYr <- factor(df$returnYr)
nfac <- nlevels(df$returnYr)
w <- (n*.4)*(nfac*.7)

ymax=max(df$value)
ymin=min(df$value)
if(opt$z) {
	ymin=0
}

p <- ggplot(df, aes(ymax=max(value)*1.05))
p <- p + 
  geom_point(aes(returnYr, value, color=scn, shape=scn), alpha=0.7, size=4) +
  scale_y_continuous(limits = c(ymin, ymax)) +   
  scale_colour_manual(values=tPalette) + 
  scale_shape_manual(values=shapes) + 
  theme_bw() + theme(legend.title = element_blank()) + 
  theme(axis.title.x=element_text(vjust=-0.05), axis.title.y=element_text(vjust=1), 
        plot.title=element_text(hjust=0, vjust=2, size=10, face="bold"), 
		legend.position=c(0,0.85), legend.justification='left', legend.text=element_text(size=7), 
		legend.background = element_rect(colour = "black"), legend.key.size = unit(0.4, "cm")) + 
  labs(title=main.title, x="Return Interval (years)", y="Streamflow (cfs)")  
ggsave(file=outPath, plot=p, width=(w), height=5)



