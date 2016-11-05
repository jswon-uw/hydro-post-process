args <- commandArgs(trailingOnly = TRUE)
options(scipen=10)
library(methods)
library(ggplot2)
library(optparse)
library(reshape2)
library(lubridate)
suppressMessages(library(dplyr))

option_list <- list(
  make_option(c("-s", "--simdata"), action="store", default=NA, type='character', help="Historical data set."),
  make_option(c("-f", "--futdata"), action="store", default=NA, type='character', help="Future data set."),
  make_option(c("-r", "--ref"), action="store", default=NA, type='character', help="Reference data set."),
  make_option(c("--pathCSV"), action="store", default=NA, type='character', help="Specify CSV output location. Supercedes --pathOut"), 
  make_option(c("--pathPNG"), action="store", default=NA, type='character', help="Specify PNG output location. Supercedes --pathOut"), 
  make_option(c("--pathOUT"), action="store", default="./", type='character', help="Specify all file output locations. Defaults to current directory."), 
  make_option(c("--outName"), action="store", default=NA, type='character', help="Filename descripter."), 
  make_option(c("--title"), action="store", default="", type='character', help="Option to specifiy a title."),
  make_option(c("--tpose"), action="store_true", default=FALSE, type='logical', help="Option to use transpose data."),
  make_option(c("--day"), action="store", default=NA, type='character', help="Day value in the form MM/DD"),
  make_option(c("--week"), action="store", default=-1, type='integer', help="Week Number"),
  make_option(c("--ed"), action="store", default=NA, type='character', help="Specify ending week number of day period."),  
  make_option(c("--useGCM"), action="store_true", default=FALSE, type='character', help="Use list of GCM as input data."),
  make_option(c("--groupby"), action="store", default=1, type='integer', help="Specify groupby direction. Use 1 for GCMs/Year and 2 for day/week."),
  make_option(c("--color1"), action="store", default="000000", type='character', help="Set reference hex color. #xxxxxx"),
  make_option(c("--color2"), action="store", default="D55E00", type='character', help="Set estimate hex color. #xxxxxx"), 
  make_option(c("--nameref"), action="store", default="TIR", type='character', help="Set reference data name."), 
  make_option(c("--namedel"), action="store", default="Estimate", type='character', help="Set estimate data name."), 
  make_option(c("--width"), action="store", default=10, type='double', help="Width of graph"),
  make_option(c("--height"), action="store", default=5, type='double', help="height of graph"),
  make_option(c("--xmin"), action="store", default=NA, type='double', help="Set minium x value"),
  make_option(c("--xmax"), action="store", default=NA, type='double', help="Set maximum x value"),
  make_option(c("--ymin"), action="store", default=NA, type='double', help="Set minium y value"),
  make_option(c("--ymax"), action="store", default=NA, type='double', help="Set maximum y value")
  )
opt <- parse_args(OptionParser(option_list=option_list))

# Argument check
if(is.na(opt$s) || is.na(opt$f) || is.na(opt$r)) {
	stop("Data set is missing.")
} else if((opt$week == -1)  && (is.na(opt$day))){
	stop("Invalid or missing both week and day options. Please specify one.")
} else if((opt$week != -1) && (!is.na(opt$day))) {
	stop("Cannot specify both week and day. Please choose one.")
} else if((opt$groupby != 1) && (opt$groupby!= 2)) {
	stop("Invalid groupby value. Select 1 or 2.")
}

## Initialization
cbPalette <- c()
df <- data.frame()
refname <- opt$nameref
delname <- opt$namedel
color1 <- paste0("#", opt$color1)
color2 <- paste0("#", opt$color2)
cbPalette <- c(color1, color2)
isDay <- !is.na(opt$day)
sel <- ifelse(isDay, opt$day, opt$week)
ed <- opt$ed
grp <- opt$groupby
useGCM <- opt$useGCM

##------------------------------------------------------------------------------
## Functions
##------------------------------------------------------------------------------

# Helper function to calculate y3 value given two reference points and the x3 that is being interpolated on
interpY <- function(x1, x2, y1, y2, x3) {
	y3 <- y1 + (y2 - y1) / (x2-x1) * (x3-x1)
	return(y3)
}

# Function to calculate the estimated future values by interpolating the values and adding the delta
interp <- function(ddf, rdf) {
	# Trim values
	rmin <- min(rdf$distance)
	rmax <- max(rdf$distance)
	ddf <- ddf[(ddf$distance >= rmin) & (ddf$distance <= rmax), ]

	# Initialize data frame
	edf <- ddf[,c("distance"),drop=F]
	edf$values <- 0
	
	# Interp y	
	n <- nrow(ddf)
	x1 <- rdf[1,1]
	y1 <- rdf[1,2]
	x2 <- rdf[2,1]
	y2 <- rdf[2,2]
	count <- 2
	for(i in 1:n) {
		x3 <- edf[i,1]
		while(x3 > x2) {
			count <- count + 1
			x2 <- rdf[count, 1]
			y2 <- rdf[count, 2]
		}
		if(x3 == x1) {
			edf[i,2] <- y1 
		} else {
			edf[i,2] <- interpY(x1, x2, y1, y2, x3) 		
		}		
	}
	
	# Apply delta
	ddf[,-1] <- ddf[,-1] + edf[,2]
	ddf[,1] <- edf[,1]
	
	return(ddf)
}

# Helper function to read in dataset
readSimData <- function(fPath, tpose, sel, isDay, ed=NA, useGCM=F) {
	df <- read.csv(file=fPath, sep=",", header=T, stringsAsFactors=F, check.names=F)
	if(is.na(ed)) {
		ed <- sel
	}	
	
	if(isDay) { # Daily		
		# Format Daily Transposed data
		if(tpose) {
			df <- t(df)
			colnames(df) <- df[1,]
			df <- df[-1,]
			df <- data.frame(date=rownames(df), df, check.names=F) 
		}
			# Format Daily Data
			df <- df[, !duplicated(colnames(df), fromLast = TRUE)]			
			df$date <- as.Date(df$date, "%m/%d/%Y")
			df <- data.frame(year=as.numeric(format(df$date, "%Y")), df, check.names=F)
			year(df$date) <- 2000
			st_dt <- as.Date(paste0("2000/", sel))
			ed_dt <- as.Date(paste0("2000/", ed))
			df <- filter(df, date >= st_dt & df$date <= ed_dt)
			#df <- select(df, -c(date))
	} else { # Weekly
		ed <- as.numeric(ed)
		# Format Transposed Weekly Data
		if(tpose) {		
			colnames(df) <- paste0(colnames(df), '_', df[1,])
			df <- df[!duplicated(df$year, fromLast = TRUE),]
			df <- melt(df[-1,], id.vars="year_week")
			df <- dcast(df, variable~year_week)
			df$variable <- as.character(df$variable)
			tmp <- (strsplit(df$variable,'_'))			
			df <- data.frame(year=as.numeric(sapply(tmp, "[[", 1)), week=as.numeric(sapply(tmp, "[[", 2)), df[,-1], check.names=F)
		}
	
		# Format Weekly Data
		df <- df[, !duplicated(colnames(df), fromLast = TRUE)]
		df <- filter(df, week >= sel, week <= ed)
		#df <- select(df, -c(week))		
	}
	
	colnames(df)[1] <- "f1"
	colnames(df)[2] <- "f2"
	if(useGCM) {
		df <- df %>% group_by(f2) %>% summarise_each(funs(mean)) %>% as.data.frame()
		df <- df[,-2]
	}
	return(df)
}

# Helper function to format data frame for final output
format_df <- function(df, grp, isDay) {
	# Aggregation - Group by year/gcm or week/day
	if(grp == 1) {
		df <- df %>% group_by(f1) %>% summarise_each(funs(median)) %>% as.data.frame()
	} else if(grp == 2) {
		df <- df %>% group_by(f2) %>% summarise_each(funs(median)) %>% as.data.frame()
	}
	
	# Data output formatting
	rownames(df) <- df[,1]
	df <- df[,-c(1,2)]
	df <- t(df)	
	df <- cbind(data.frame(as.numeric(rownames(df))), df)
	rownames(df) <- NULL
	
	# Extra formatting for day numbering
	if(isDay && opt$groupby == 2) {
		colnames(df) <- format(strptime(colnames(df), format="%Y-%m-%d"), "%m-%d")
	}
	
	colnames(df)[1] <- "distance"
	return(df)
}


# Helper function to plot graph
plot_graph <- function(df, outPath, w, h, cbPalette) {
	g <- ggplot(df, aes(x=distance, y=value, color=scn), na.rm=T)
	p <- g + 
		scale_colour_manual(values=cbPalette) +
		geom_ribbon(aes(ymin=lower, ymax=upper), linetype=2, alpha=0.2, fill=cbPalette[2], show_guide=F) + 
		geom_line(aes(group=interaction(scn, variable)), alpha=0.2, show_guide=F) + 
		stat_summary(aes(group=scn), fun.y=median, geom="line", size=1.3) + 
		stat_summary(aes(group=scn), fun.y=median, geom="line", size=1.3, color="#000000", alpha=0.2) + 
		scale_fill_manual(values=rev(cbPalette)) + 
		scale_x_continuous(limits=c(xmin, xmax)) + 
		scale_y_continuous(limits=c(ymin, ymax)) + 
		theme_bw() + theme(legend.title = element_blank(), axis.text=element_text(size=12), 
			axis.title.x=element_text(vjust=-0.5), axis.title.y=element_text(vjust=1), 
		    legend.position=c(1,1), legend.justification=c(1,1), 
			legend.background = element_rect(colour = "black")) +
		labs(title=main.title, x="Stream Distance (km)", y="Stream Temperature (C)")
	ggsave(file=outPath, plot=p, width=w, height=h)	 
}


##------------------------------------------------------------------------------
## Main
##------------------------------------------------------------------------------

## Read and pre-format historical simulation data
if(useGCM) {
	hispaths <- read.csv(file=opt$s, header=F, stringsAsFactors=F)
	n <- nrow(hispaths)
	hdf <- data.frame()
	for(i in 1:n) {
		tdf <- readSimData(hispaths[i,2], opt$tpose, sel, isDay, ed, useGCM)
		tdf[,1] <- hispaths[i,1]
		hdf <- rbind(hdf, tdf)
	}
} else {
	hdf <- readSimData(opt$s, opt$tpose, sel, isDay, ed, useGCM)
}

## Read and pre-format future simulation data
if(useGCM) {
	futpaths <- read.csv(file=opt$f, header=F, stringsAsFactors=F)
	n <- nrow(futpaths)
	fdf <- data.frame()
	for(i in 1:n) {
		tdf <- readSimData(futpaths[i,2], opt$tpose, sel, isDay, ed, useGCM)
		tdf[,1] <- futpaths[i,1]
		fdf <- rbind(fdf, tdf)
	}
} else {
	fdf <- readSimData(opt$f, opt$tpose, sel, isDay, ed, useGCM)
}

## Read and format reference data
rdf <- read.table(file=opt$r, sep=",", header=T, stringsAsFactors=F, check.names=F)
rdf <- rdf[!rev(duplicated(rev(rdf[,1]))),]
rdf <- rdf[, c("KM","Temp_C")]
colnames(rdf) <- c("distance", "value")
rdf$scn <- refname


## Calculate delta
ddf <- fdf - hdf
ddf[,1:2] <- hdf[,1:2]

# Format data frame
hdf <- format_df(hdf, grp, isDay)
fdf <- format_df(fdf, grp, isDay)
ddf <- format_df(ddf, grp, isDay)


## Interpolate the y values
edf <- interp(ddf, rdf)

## Write data out
if(is.na(opt$outName)) {
	period <- ifelse(isDay, "daily", "weekly")
	sel <- gsub("/", "-", sel)
	ed <- gsub("/", "-", ed)
	if(grp == 1) {
		grouped <- ifelse(useGCM, "byGCM", "byYear")
	} else {
		grouped <- ifelse(isDay, "byDaily", "byWeekly")
	}
	ed <- ifelse(is.na(ed), "", paste0("_", ed))
	outName <- paste0(period, "_", grouped, "_", sel, ed)
} else {
	outName <- opt$outName
}
csvOut <- ifelse(is.na(opt$pathCSV), opt$pathOUT, opt$pathCSV)
write.csv(hdf, file=paste0(csvOut, "/Historical_", outName, ".csv"))
write.csv(fdf, file=paste0(csvOut, "/Future_", outName, ".csv"))
write.csv(edf, file=paste0(csvOut, "/",delname, "_", outName, ".csv"), row.names=F)

## Combine and melt dataset
gcms <- colnames(edf)[-1]
edf$scn <- delname
edf$lower <- apply(edf[ ,gcms, drop=FALSE], 1, min)
edf$upper <- apply(edf[ ,gcms, drop=FALSE], 1, max)
rdf$lower <- rdf$value
rdf$upper <- rdf$value
cnames <- c("distance", "scn", "lower", "upper")
df <- rbind(melt(rdf, id=cnames), melt(edf, id=cnames))

## Data clean-up
df$scn <- factor(df$scn, levels=c(refname, delname))

## Plot Formatting
main.title=opt$title
pngPath <- ifelse(is.na(opt$pathPNG), opt$pathOUT, opt$pathPNG)
pngOut <- paste0(pngPath, "/Delta-Plot_", outName, ".png")
xmin=opt$xmin
xmax=opt$xmax
ymin=opt$ymin
ymax=opt$ymax

## Plot Data
plot_graph(df, pngOut, opt$width, opt$height, cbPalette)