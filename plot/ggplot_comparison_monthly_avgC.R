args <- commandArgs(trailingOnly = TRUE)
options(scipen=10)
library(methods)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
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

##Functions
monthToWDate <- function(mo) {
  mo <- as.integer(mo)
  if(mo > 9) {
    return(as.Date(paste(1999,mo,1,sep="-")))
  }
  else {
    return(as.Date(paste(2000,mo,1,sep="-")))
  }
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

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
m <- length(cbPalette)
tPalette <- c(cbPalette[1], cbPalette[(m-(n-2)):m])

# Set color based on given value
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
  if(ncol(tdf)==2) {	
	data.max <- tdf
	data.min <- tdf
  } else {
    # Get Max
	data.max <- cbind(tdf[,1], apply(tdf[,-1], 1, function(x) max(x)))
	# Get Min
	data.min <- cbind(tdf[,1], apply(tdf[,-1], 1, function(x) min(x)))
  }
  colnames(data.max) <- c("Month", "max")
  colnames(data.min) <- c("Month", "min")
  
  # Melt and merge
  tdf$scn <- datapaths[i,1]
  scene <- c(scene, datapaths[i,1])
  tdf <- melt(tdf, id=c("Month", "scn"))  
  tdf <- merge(x=tdf, y=data.max, by="Month", all=TRUE)
  tdf <- merge(x=tdf, y=data.min, by="Month", all=TRUE)
  df <- rbind(df, tdf)
}

# Format factors and change months to dates
df$scn <- factor(df$scn, levels=scene)
temp <- lapply(df$Month, monthToWDate)
df$Month <- do.call("c", temp)

## Full Plots
g <- ggplot(df)
sPalette <- tPalette
p <- g + 
  geom_ribbon(aes(x=Month, ymin=min, ymax=max, group=interaction(variable, scn), fill=paste(scn, "range")), alpha=0.07, colour=NA) +
  stat_summary(aes(x=Month, y=value, group=scn, color=paste(scn, "mean")), fun.y=mean, geom="line", size=1.5) + 
  #geom_line(aes(x=Month, y=value, group=interaction(variable, scn), color=scn)) + 
  #geom_point(aes(x=Month, y=value, group=variable, median=median(value), color=scn)) +
  scale_fill_manual(values=sPalette) + 
  scale_colour_manual(values=sPalette) + 
  scale_x_date(labels=date_format("%b"), breaks=date_breaks("1 month")) + 
  theme_bw() + theme(legend.title = element_blank()) + 
  guides(fill = guide_legend(override.aes = list(alpha = 1))) + 		
  theme(axis.title.x=element_text(vjust=-0.5), axis.title.y=element_text(vjust=1), plot.title=element_text(vjust=2, size=10, face="bold")) + 		
  labs(x="Months", y="Streamflow (cfs)", title=main.title)
ggsave(file=outPath, plot=p, width=7, height=3)


