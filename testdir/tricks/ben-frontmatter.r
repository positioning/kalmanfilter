    #-----------------------------------------------------------------------------#
     #       To make the color ramps used in this package, use the following
     #-----------------------------------------------------------------------------#
     
     # Mimics the jet colors from matlab. Good for SST
     jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
     
     # nice blue cascade for bathymetry
     bath.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan"))
     
     # nice earthy colors for land
     land.colors<-  colorRampPalette(c("darkgreen", "greenyellow","burlywood4"))
     
     # Colors to use in the plot.by.month (and related) functions
     # Also included via data(month.colors)
     make.month.colors <- function(month.seq=c(8:12,1:7))
        {
        month.colors <- cbind(month.seq, 
           c(rgb(115/255,0,76/255),
           rgb(0,76/255,115/255),
           rgb(0,92/255,230/255),
           rgb(115/255,223/255,1),
           rgb(190/255,232/255,1),
           rgb(1,1,190/255),
           rgb(230/255,230/255,0),
           rgb(1,170/255,0),
           rgb(1,120/255,0),
           rgb(1,0,197/255),
           rgb(1,0,0),
           rgb(168/255,0,0))
       )
       return(month.colors)
     }
     month.colors <- make.month.colors()
 
     # Also included via data(monames)
     monames=c('Jan','Feb','Mar','Apr','May','June','July','Aug','Sept','Oct','Nov','Dec')
     
     # Colour wheel --- great for legend!
     colorwheel <- function(){
     mm <- data.frame(month.colors)
     mseq <- as.numeric(as.vector(mm[,1]))
     mm <- mm[order(mseq),]
	 par(cex=2)
     pie(rep(1,12), col=as.vector(mm[,2]), labels=month.abb, radius=1, clockwise=T)
     }
     
	 colorwheel()
