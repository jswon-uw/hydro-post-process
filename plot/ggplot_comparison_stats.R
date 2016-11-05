args <- commandArgs(trailingOnly = TRUE)
options(scipen=10)
library(methods)
library(ggplot2)
library(reshape2)
library(optparse)

option_list <- list(
  make_option(c("-t", "--title"), action="store", default="", type='character', help="Option to specifiy a title."),
  make_option(c("-b", "--box"), action="store_true", default=FALSE, help="Enable boxplots."),
  make_option(c("-c", "--color"), action="store_true", default=FALSE, help="Fixes color to provided value"),
  make_option(c("-f", "--filePath"), action="store", default=NA, type='character', help="File with filepath, name and color (optional)"),
  make_option(c("-o", "--outPath"), action="store", default=NA, type='character', help="Output filepath"),
  make_option(c("-z", "--zero"), action="store_true", default=FALSE, type='character', help="Forces minimum to 0")
  )

opt <- parse_args(OptionParser(option_list=option_list))
main.title=opt$t

# Inputs
#inFile  <- args[1]   # List of data input name and data
#outPath <- args[2]   # Output path
inFile <- opt$f
outPath <- opt$o
datapaths <- read.table(file=inFile, sep=",", header=F, stringsAsFactors=F)
n <- nrow(datapaths)
df <- data.frame()
#cbPalette <- c("#56B4E9", "#E69F00", "#D55E00", "#000000", "#009E73", "#F0E442", "#0072B2", "#CC79A7")
cbPalette <- c("#000000", "#CC79A7", "#0072B2", "#009E73", "#F0E442", "#56B4E9", "#E69F00", "#D55E00")
#cbPalette <- c("#E69F00", "#CC79A7", "#0072B2", "#009E73", "#F0E442", "#56B4E9", "#000000", "#D55E00") # WRF Color Exception
m <- length(cbPalette)
tPalette <- c(cbPalette[1], cbPalette[(m-(n-2)):m])
if(opt$c) {
	cbPalette <- c("#56B4E9", "#000000", "#E69F00", "#D55E00", "#CC79A7", "#0072B2", "#009E73", "#F0E442")
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

# Read Data
scene <- list()
for (i in 1:n) {
  tdf <- read.csv(file=datapaths[i,2], sep=",", header=T)
  tdf$scn <- datapaths[i,1]
  scene <- c(scene, datapaths[i,1])
  #tdf$prob <- round(1/tdf$prob)
  #tdf <- melt(tdf, id=c("prob", "scn"))
  tdf <- melt(tdf, id=c("returnYr", "scn"))
  df <- rbind(df, tdf) 
}

df$scn <- factor(df$scn, levels=scene)
df$returnYr <- factor(df$returnYr)
nfac <- nlevels(df$returnYr)
w <- (n*.7)*(nfac*.6) + 3

ymax=max(df$value)
ymin=min(df$value)
if(opt$z) {
	ymin=0
}

p <- ggplot(df, aes(scn, value, color=scn, ymax=max(value)*1.05))

if(opt$b) {
  p <- p + 
  #stat_boxplot(geom ='errorbar') + 
  geom_boxplot(show.legend=TRUE)
}

p <- p + 
  geom_point(alpha=1) +
  #geom_text(aes(label=scn), size=2, vjust=-1) + 
  facet_grid(~returnYr) + 
  scale_y_continuous(limits = c(ymin, ymax)) +   
  #facet_grid( ~returnYr, switch='x') +  # switch command only works for ggplot2 2.0.0
  #stat_summary(fun.data = "mean_cl_normal", aes(shape="mean"), colour = "red", geom="point") +
  scale_colour_manual(values=tPalette) + 
  theme_bw() + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.title = element_blank()) + 
  theme(axis.title.x=element_text(vjust=-0.05), axis.title.y=element_text(vjust=1), plot.title=element_text(vjust=2, size=10, face="bold")) + 
  labs(title=main.title, x="Return Interval (years)", y="Streamflow (cfs)")  
ggsave(file=outPath, plot=p, width=(w), height=5)



