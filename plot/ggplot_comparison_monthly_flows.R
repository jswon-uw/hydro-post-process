args <- commandArgs(trailingOnly = TRUE)
options(scipen=10)
library(methods)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(optparse)
suppressMessages(library(hydroGOF))


option_list <- list(
  make_option(c("-t", "--title"), action="store", default="", type='character', help="Option to specifiy a title."),
  make_option(c("-c", "--color"), action="store_true", default=FALSE, help="Fixes color to provided value."),
  make_option(c("-k", "--korrelation"), action="store_true", default=FALSE, help="Draws correlation on plot."),
  make_option(c("-f", "--filePath"), action="store", default=NA, type='character', help="File with filepath, name and color (optional)."),
  make_option(c("-o", "--outPath"), action="store", default=NA, type='character', help="Output filepath."),
  make_option(c("-s", "--styear"), action="store", default=NA, type='character', help="Fix the plot starting year."),
  make_option(c("-e", "--edyear"), action="store", default=NA, type='character', help="Fix the plot ending year."),
  make_option(c("-z", "--zero"), action="store_true", default=FALSE, type='character', help="Forces minimum to 0.")
  )

opt <- parse_args(OptionParser(option_list=option_list))
main.title=opt$t

# Functions
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

# Function to generate correlation coefficient and NSE value for the charts
gof_eqn <- function(x,y, digits = 2) {
  n <- round(cor(x, y, method="pearson", use="complete"), digits=digits)
  corr_coef <- paste0(" R = ", n)
  n <- round(NSE(x,y, use="complete"), digits=digits)
  corr_coef <- c(corr_coef, paste0(" NSE = ", n))
  return(corr_coef)
}


# Inputs
#inFile  <- args[1]   
#outPath <- args[2]   
inFile <- opt$f			# List of data input name and data
outPath <- opt$o		# Output path
datapaths <- read.table(file=inFile, sep=",", header=F, stringsAsFactors=F)
n <- nrow(datapaths)
df <- data.frame()
#cbPalette <- c("#56B4E9", "#E69F00", "#D55E00", "#000000", "#009E73", "#F0E442", "#0072B2", "#CC79A7")
cbPalette <- c("#000000", "#D55E00" , "#E69F00", "#009E73", "#F0E442", "#56B4E9", "#CC79A7", "#0072B2")
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
  # Melt and merge
  tdf$scn <- datapaths[i,1]
  scene <- c(scene, datapaths[i,1])
  tdf$Date <- as.Date(paste(tdf$Year, tdf$Month, 1, sep="-"))
  tdf <- melt(tdf, id=c("Date", "Year", "Month", "scn"))  
  df <- rbind(df, tdf)
}

# Format factors and change months to dates
df$scn <- factor(df$scn, levels=scene)
minYear <- min(df$Year)
maxYear <- max(df$Year)
splice <- ceiling((maxYear-minYear)/10)
years <- seq(minYear, minYear+(10*splice)-1, 1)
#years <- split(years, ceiling(seq_along(years)/10))
ymax=max(df$value)
ymin=min(df$value)
if(opt$z) {
	ymin=0
}

pl = list()
# Create plots
for (i in 1:splice) {	
	yrs <- years[((i-1)*10+1):(i*10)]
	xmin <- as.Date(paste(yrs[1], 1, 1, sep="-"))
	xmax <- as.Date(paste(yrs[10], 12, 31, sep="-"))
	g <- ggplot(subset(df, Year %in% yrs), aes(x=Date, y=value, group=scn, color=factor(scn)))
	p <- g + 
		geom_line(alpha=0.7) + 
		geom_point(size=1, alpha=0.7) +
		scale_colour_manual(values=tPalette, limits=levels(df$scn)) + 
		scale_y_continuous(limits = c(ymin, ymax)) + 
		scale_x_date(labels=date_format("%Y"), breaks=date_breaks("1 year"), limits= c(xmin, xmax)) + 
		theme_bw() + theme(legend.title = element_blank()) + 
		guides(fill = guide_legend(override.aes = list(alpha = 1), order=1)) + 		
		theme(axis.title.x=element_text(vjust=-0.5), axis.title.y=element_text(vjust=1)) + 		
		labs(x="Months", y="Streamflow (cfs)")
	if(i == 1) {
		p <- p + 
			theme(plot.title=element_text(vjust=2, size=10, face="bold")) + 
			labs(title=main.title) 
		
		if(opt$k) {			
			x <- df[df$scn == datapaths[1,1],]
			y <- df[df$scn == datapaths[2,1],]
			x <- merge(x, y[,c("Date"),drop=F], by="Date", all=T)
			y <- merge(y, x[,c("Date"),drop=F], by="Date", all=T)			
			corr_text <- gof_eqn(x$value, y$value)
			p <- p + 
				geom_text(x=-Inf, y=Inf, hjust=0, vjust=1.1, size=3, color="black",
						label=paste0(corr_text[1], "\n", corr_text[2]), show_guide=F)
		}	
	}	
	pl[[i]] <- p
	print(pl[[i]])

}

# Save plots  
png(outPath, width=10, height=2*splice, units='in', res=600)
multiplot(plotlist=pl, cols=1)
invisible(dev.off())

