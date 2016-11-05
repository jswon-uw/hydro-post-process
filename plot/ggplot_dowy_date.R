args <- commandArgs(trailingOnly = TRUE)
options(scipen=10)
library(methods)
library(ggplot2)
library(scales)

# Inputs
inFile  <- args[1]   # List of data input name and data
outPath <- args[2]   # Output path
datapaths <- read.table(file=inFile, sep=",", header=F, stringsAsFactors=F)
n <- nrow(datapaths)
df <- data.frame()
cbPalette <- c("#56B4E9", "#E69F00", "#D55E00", "#000000", "#009E73", "#F0E442", "#0072B2", "#CC79A7")

# Read Data
scene <- list()
for (i in 1:n) {
  tdf <- read.csv(file=datapaths[i,2], sep=",", header=T) 
  colnames(tdf) <- c("WYear", "Year", "Month", "Day", "Flow")
  tdf$Dates <- paste(2000, tdf$Month, tdf$Day, sep="-")
  tdf$Dates <- strftime(tdf$Dates, format="%j")
  tdf <- tdf[c("Dates", "Flow")]
  tdf$scn <- datapaths[i,1]
  scene <- c(scene, datapaths[i,1]) 
  df <- rbind(df, tdf)
}
df$scn <- factor(df$scn, levels=scene)
df$Dates <- as.numeric(df$Dates)

g <- ggplot(df)
p <- g + 
  geom_point(aes(x=Dates, y=Flow, group=scn, color=scn), size=2, alpha=0.7) + 
  scale_colour_manual(values=cbPalette) + 
  scale_x_continuous( breaks=seq(0,365,by=10),minor_breaks=seq(1,365,by=5) )+  
  theme_bw() + theme(legend.title = element_blank()) + 
  theme(axis.title.x=element_text(vjust=-0.5), axis.title.y=element_text(vjust=1), axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="Day of Year", y="Streamflow (cfs)")
print(p)
ggsave(file=outPath, plot=p, width=10, height=4)