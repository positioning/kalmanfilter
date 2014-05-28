tracks.kml <-
function (tracklist=c("fit"), name = "", description = "", 
	  folder = getwd(), kmlname = "doc.kml",
	  iconscale = 1, turnon = c(1,0,0,1),
          showpath = c(1,0,0,0), showpoint = c(1,1,1),
          npoints = 20, level = 0.95,
	  getimage = T, product = c("TBAssta"), interval = c(5), 
	  local = F, lon360 = T, 
	  coastwatch.erddap = F, variable=c("sst"), 
	  colorbar=NA, imgname=NA, palette="rainbow", 
	  height.data="nodata", scale.height = NA,
	  color.data="nodata", data.palette="Jet",
	  color.range=c(NA,NA), color.breaks=1000, color.log10=F, 
	  bubble.data="nodata", bubble.scale=1, alpha=1.0, ...) 
{
  ### Required packages: date, png
  ### ----------------------------
  ### Color matters
  options(warn=-1)
  if(data.palette=='Months') color.range <- c(1,12) # Special case for months
  ii <- 0
  mypalette <- rainbow(length(tracklist)+ii)
  if (palette!="rainbow") mypalette <- colorpalette(length(tracklist)+ii, pname=palette)
  rearrange <- function(s1) paste(substr(s1,8,9), substr(s1,6,7),
			        substr(s1,4,5), substr(s1,2,3), sep = "")
  lcolor <- unlist(lapply(mypalette, rearrange))
  ### Support functions
  `%+%` <- function(s1, s2) paste(s1, s2, sep = "")
  l <- function(x) length(x) + 1
  inlist <- function(what,from)
  {
            idx <- length(which(what==from))
            return(idx)}			
  substring.location <- function (text, string, restrict) 
  {
    if (length(text) > 1) 
        stop("only works with a single character string")
    l.text <- nchar(text)
    l.string <- nchar(string)
    if (l.string > l.text) 
        return(list(first = 0, last = 0))
    if (l.string == l.text) 
        return(if (text == string) list(first = 1, last = l.text) else list(first = 0, 
            last = 0))
    is <- 1:(l.text - l.string + 1)
    ss <- substring(text, is, is + l.string - 1)
    k <- ss == string
    if (!any(k)) 
        return(list(first = 0, last = 0))
    k <- is[k]
    if (!missing(restrict)) 
        k <- k[k >= restrict[1] & k <= restrict[2]]
    if (length(k) == 0) 
        return(list(first = 0, last = 0))
    list(first = k, last = k + l.string - 1)
  }
  dat.meta <- function(file)
  {
     beginopt <- which(file=="<!--BeginOptions-->")
     endopt <- which(file=="<!--EndOptions-->")
     list1 <- list()
     for (i in 1:(endopt - beginopt - 1))
     {
     starti <- substring.location(file[beginopt+i], "timePeriod=\">")$last+1
     endi <- substring.location(file[beginopt+i], "</a>")$first-1
     list1[i] <- substr(file[beginopt+i], starti, endi)
     }
     return(list(product=unlist(list1[seq(1,length(list1),2)]),
            meta=unlist(list1[-seq(1,length(list1),2)])))
  }
  .getCI <- function (x, level = 0.95, npoints = 100) 
  {
    t.quan <- sqrt(qchisq(level, 2))
    centre <- x[5:6]
    x <- matrix(x[1:4], 2, 2)
    r <- x[1, 2]
    scale <- sqrt(diag(x))
    if (scale[1] > 0) {
        r <- r/scale[1]
    }
    if (scale[2] > 0) {
        r <- r/scale[2]
    }
    r <- min(max(r, -1), 1)
    d <- acos(r)
    a <- seq(0, 2 * pi, len = npoints)
    matrix(c(t.quan * scale[1] * cos(a + d/2) + centre[1], t.quan * 
        scale[2] * cos(a - d/2) + centre[2]), npoints, 2)
  }
  ### Add a list of imagery to kml
  prep.line <- function(dinfo, folder, product, nday="", 
                        visible, nodata, cbpng=NA, searchfor=NA)
  {
   if (nodata!=1)
   {
    imglist <- dir(folder, pattern = paste("^", product, sep=""))
	imglist <- imglist[grep(".kml", imglist)]
    info <- "http://coastwatch.pfel.noaa.gov/infog/"
    href <- paste(info, substr(product,2,3), "_" , 
	          substr(product,4,7), "_", "las.html", sep="")
    subl <- list()
    subl[1] <- "<Folder>\n"
    subl[l(subl)] <- "<name>" %+% "Satellite product: " %+% product %+% "</name>\n"
    subl[l(subl)] <- "<visibility>" %+% ifelse(visible==1,1,0) %+% "</visibility>\n"
	if (!is.na(dinfo)){
		subl[l(subl)] <- "<description><![CDATA[ " %+% "Brief description: " %+% nday %+% "\n" %+%
				 dinfo$meta[which(dinfo$product==product)] %+% "\n"
		subl[l(subl)] <- "<br /><a href=\"" %+% href %+% "\">Click here for more dataset info</a>\n"
		subl[l(subl)] <- "]]></description>\n"
	}
	if (!is.na(cbpng)){
		subl[l(subl)] <- "<ScreenOverlay>\n"
		subl[l(subl)] <- "<name> Color scale </name>\n"
        subl[l(subl)] <- "<visibility>" %+% ifelse(visible==1,1,0) %+% "</visibility>\n"    
        subl[l(subl)] <- "<Icon>\n"
        subl[l(subl)] <- "<href>" %+% "images/" %+% cbpng %+% "</href>\n"
        subl[l(subl)] <- "</Icon>\n"
        subl[l(subl)] <- '<overlayXY x="0.995" y=".048" xunits="fraction" yunits="fraction"/>\n'
        subl[l(subl)] <- '<screenXY x="0.995" y=".048" xunits="fraction" yunits="fraction"/>\n'
        subl[l(subl)] <- '<rotationXY x="0" y="0" xunits="fraction" yunits="fraction"/>\n'
        subl[l(subl)] <- '<size x="0" y="0" xunits="fraction" yunits="fraction"/>\n'
		subl[l(subl)] <- "</ScreenOverlay>\n"
	}
	if (!is.na(searchfor)){
		imglist <- dir(folder, pattern = paste("^", searchfor, "_" , sep=""))
		imglist <- imglist[grep(".kml", imglist)]
	}	
    for (j in 1:length(imglist)) 
     {
     subl[l(subl)] <- "<NetworkLink>\n"
     subl[l(subl)] <- "<name>" %+% imglist[j] %+% "</name>\n"
     subl[l(subl)] <- "<visibility>" %+% ifelse(visible==1,1,0) %+% "</visibility>\n"     
     subl[l(subl)] <- "<refreshVisibility> icon" %+% 1 %+% "</refreshVisibility>\n"
     subl[l(subl)] <- "<flyToView> icon" %+% 1 %+% "</flyToView>\n"
     subl[l(subl)] <- "<Link>\n"
     subl[l(subl)] <- "<href>" %+% "images/" %+% imglist[j] %+% "</href>\n"
     subl[l(subl)] <- "</Link>\n"
     subl[l(subl)] <- "</NetworkLink>\n"
     }
    subl[l(subl)] <- "</Folder>\n"
    return(subl)
   }
  }
  ### Check a selected data product against catalogue
  dlink <- ifelse(lon360, "http://coastwatch.pfeg.noaa.gov/coastwatch/CWBrowserWW360.jsp?get=gridData&dataSet=",
				           "http://coastwatch.pfeg.noaa.gov/coastwatch/CWBrowserWW180.jsp?get=gridData&dataSet=")
  dinfo <- NA						   
  if (getimage & !coastwatch.erddap)
  {
   cat("\n\n--- Begins by getting the data listing --- \n\n")
   download.file(dlink, "products.html", mode = "wb")
   dfile <- readLines("products.html")
   unlink("products.html")
   dinfo <- dat.meta(dfile)
   nosupport <- c(which(is.na(sub("^[C]", NA, dinfo$product))), 
	       which(is.na(sub("^TC", NA, dinfo$product))),
               which(is.na(sub("cld", NA, dinfo$product))), 
	       which(is.na(sub("TATast", NA, dinfo$product))))
   cat("\n--- Checking your requested data product(s) against the listing --- \n\n")
   chkname1 <- unlist(lapply(product, inlist, dinfo$product))
   chkname2 <- unlist(lapply(product, inlist, dinfo$product[sort(nosupport)]))
   check1 <- length(product)== sum(chkname1)
   check2 <- length(product)== length(interval)
   check3 <- sum(chkname2)==0
   if (!check1|!check2|!check3)
   { 
    if (!check1){err.msg <- paste("\n Please check the name of your specified product. \n\n", 
              "Unknown name: ", product[which(chkname1==0)], "\n" )}
    if (!check2){err.msg <- "\n Mismatch in the number of items in the input arguments of product and interval.\n\n"}
    if (!check3){err.msg <- paste("\n Sorry, your specified product is not supported. \n\n", 
              "Unsupported dataset: ", product[which(chkname2==1)], "\n" )}
    stop(err.msg)
   }
  }
  ### Start concatenating lines for KML
  sl <- list()
  sl[1] <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  sl[l(sl)] <- "<kml xmlns=\"http://www.opengis.net/kml/2.2\"\n"
  sl[l(sl)] <- " xmlns:gx=\"http://www.google.com/kml/ext/2.2\">\n"
  sl[l(sl)] <- "<Document>\n"
  sl[l(sl)] <- "<name>" %+% name %+% "</name>\n"
  sl[l(sl)] <- "<description>" %+% description %+% "</description>\n"
  sl[l(sl)] <- "<open>1</open>\n"
  ### Generate icons here
  for (i in 1:length(tracklist))
  {
    png(filename = paste(folder, "/icon", i, ".png", sep=""), 
        width = 30*iconscale, height = 20*iconscale, bg = "transparent")
    par(mai=c(0,0,0,0))
    pie(1, labels="", col=mypalette[i], border=F)
    dev.off()
    # Write the style to kml
    sl[l(sl)] <- "<Style id=\"myicon" %+% i %+% "\">\n"
    sl[l(sl)] <- "<IconStyle>\n"
    sl[l(sl)] <- "<Icon>\n"
    sl[l(sl)] <- "<href> icon" %+% i %+% ".png </href>\n"
    sl[l(sl)] <- "</Icon>\n"
    sl[l(sl)] <- "</IconStyle>\n"
    sl[l(sl)] <- "</Style>\n"
  }
    # More icon styles
    sl[l(sl)] <- "<Style id=\"mydeploy\">\n"
    sl[l(sl)] <- "<IconStyle>\n"
    sl[l(sl)] <- "<color>ff31ff08</color>\n"
    sl[l(sl)] <- "<scale>1.2</scale>\n"
    sl[l(sl)] <- "</IconStyle>\n"
    sl[l(sl)] <- "</Style>\n"
	sl[l(sl)] <- "<Style id=\"mypopup\">\n"
    sl[l(sl)] <- "<IconStyle>\n"
    sl[l(sl)] <- "<color>ff0000ff</color>\n"
    sl[l(sl)] <- "<scale>1.2</scale>\n"
    sl[l(sl)] <- "</IconStyle>\n"
    sl[l(sl)] <- "</Style>\n"
  ### Download and add images here
  if (getimage == T) {
    pack.data  <- function (x){ 
       x <- get(x)
	   cls <- class(x)
	   kf <- c("trackit","kfsst","kftrack","ukfsst")
	   if (cls %in% kf){
		   if (is.null(x$decimal.date)){
		         xd <- as.data.frame(x$date)} else xd <- date.mdy(x$date)
		   subdata <- data.frame(xd$year, xd$month, xd$day, x$most.prob.track)	 
		} else{
		   names(x) <- tolower(names(x))
		   subdata <- data.frame(x$year, x$month, x$day, x$lon, x$lat)			
		}
		names(subdata) <- c("year", "month", "day", "lon", "lat")
		if (lon360==F) subdata$lon <- (subdata$lon + 180)%%360 - 180
		return(subdata)
    }
    for (k in 1:length(product))
     {	 
      cat(paste(rep("=", options()$width), collapse = ""), "\n\n")
      cat(paste(k,".", sep=""), "Getting the data product: ", product[k], "\n\n")
	  iname <- NA 	  
	  if (coastwatch.erddap){
	      if (!is.na(colorbar) && length(colorbar)==length(product)){
				cbar <- colorbar[k]
		  } else {
				cbar <- "|||||"
		  }
	      if (!is.na(imgname) && length(imgname)==length(product)) iname <- imgname[k]  
		  imgkml  <- get.erddap(lapply(tracklist, pack.data), 
			         folder=folder, product = product[k], variable = variable[k],
			         every.day = interval[k], kml.image=local, type=4, 
					 colorbar=cbar, name=iname, ...)					 
	  }
	  else {  
		  imgkml  <- satellite.kml(lapply(tracklist, pack.data), 
			   folder=folder, product = product[k],
			   interval = interval[k], local=local, server=dlink)
	  }
      imglist <- prep.line(dinfo, imgkml$folder, product[k], 
			   imgkml$nday, k, imgkml$nodataflag, imgkml$cbpng, iname)
      sl <- list(sl, imglist)
     }
  }
  ### Get value range for bar color and size
  circle.size <- 0.001
  ranc <- NA
  rans <- NA
  for (bb in 1:length(tracklist))
  {
    fit <- get(tracklist[bb])  
    jx <- sum(as.numeric(which(names(fit)==color.data)))
	if (jx>0){
	    if (is.data.frame(fit)){
			fj <- fit[,jx] } 
		else {
			fj <- fit[[color.data]]
		}
		fj[which(is.infinite(fj))] <- NA 					 
        if (color.log10) fj <- log10(fj)
        fj[which(is.infinite(fj))] <- NA 		
		ranc <- c(ranc, range(na.omit(fj)))
	}
	jx <- sum(as.numeric(which(names(fit)==bubble.data)))	
	if (jx>0){
	    if (is.data.frame(fit)){
			fj <- fit[,jx] } 
		else {
			fj <- fit[[bubble.data]]
		}		
		fj[which(is.infinite(fj))] <- NA		
		rans <- c(rans, range(na.omit(fj)))
		rans <- min(na.omit(rans))
	}
  } 
  if (sum(is.na(color.range))!=0) color.range <- range(na.omit(ranc))
  ### Prepare tracks from a fit
  for (tt in 1:length(tracklist))
  {
    print(paste("Preparing KML code for...", tracklist[tt]))
    fit <- get(tracklist[tt])
    cls <- class(fit)
	kf <- c("trackit","kfsst","kftrack","ukfsst")	
	lab <- "Most probable track"
        # Get height, color and size data
        jx <- sum(as.numeric(which(names(fit)==height.data)))
		if (jx>0){		
		   if (is.data.frame(fit)){
				hdat <- fit[,jx] } 
		   else {
				hdat <- fit[[height.data]]
		   }
		   if (is.na(scale.height)){  # Only doing this once
				hsa <- hdat
				hsa[which(is.infinite(hsa))] <- NA 
				hsa <- max(na.omit(hsa))
				hsa <- log(hsa/10000)
				hsa <- ifelse(hsa<0,abs(hsa)+0.6,1.3)
				scale.height <- 10^hsa
		   }
		}			
        jx <- sum(as.numeric(which(names(fit)==color.data)))
		if (jx>0){
	       if (is.data.frame(fit)){
			   fj <- fit[,jx] } 
		   else {
			   fj <- fit[[color.data]]
		   }		   
		   bcol <- color.lookup(fj, color.breaks, color.range, pname= data.palette,
		                        title=color.data, legend=tt, log10.transform = color.log10)
		   bcol <- unlist(lapply(bcol, rearrange))
		}
		jx <- sum(as.numeric(which(names(fit)==bubble.data)))	
		if (jx>0){
			if (is.data.frame(fit)){
				udat <- fit[,jx] } 
			else {
				udat <- fit[[bubble.data]]
			}		
		}
	    # Not KF fit, repackaging	
		if (cls %in% kf == F){	
				e3 <- new.env()
				names(fit) <- tolower(names(fit))
				e3$year <- fit$year
				e3$month <- fit$month
				e3$day <- fit$day
				e3$hour <- fit$hour
				e3$min <- fit$min
				e3$sec <- fit$sec
				e3$most.prob.track <- data.frame(fit$lon, fit$lat)
				e3$fix.first <- 1
				e3$fix.last <- 1
				fit <- as.list(e3)
				lab <- "Path"
	    }
		fit$most.prob.track[, 1] <- (fit$most.prob.track[, 1] + 180)%%360 - 180
		sl[l(sl)] <- "<Folder>\n"
		oname <- ifelse(is.null(fit$data.name),tracklist[tt],fit$data.name)
		sl[l(sl)] <- "<name>" %+% oname %+% "</name>\n"
		sl[l(sl)] <- "<Placemark>\n"
		sl[l(sl)] <- "<name>" %+% lab %+% "</name>\n"
		sl[l(sl)] <- "<description>" %+% lab %+% 
			"</description>\n"
		sl[l(sl)] <- "<LookAt>\n"
		sl[l(sl)] <- "<longitude>" %+% mean(fit$most.prob.track[1]) %+% "</longitude>\n"
		sl[l(sl)] <- "<latitude>" %+% mean(fit$most.prob.track[, 2]) %+% "</latitude>\n"
		sl[l(sl)] <- "<range>" %+% 2e+06 %+% "</range>\n"
		sl[l(sl)] <- "<tilt>" %+% 0 %+% "</tilt>\n"
		sl[l(sl)] <- "<heading>" %+% 0 %+% "</heading>\n"
		sl[l(sl)] <- "</LookAt>\n"
		sl[l(sl)] <- "<visibility>" %+% showpath[1] %+% "</visibility>\n"
		sl[l(sl)] <- "<open>" %+% 0 %+% "</open>\n"
		sl[l(sl)] <- "<Style>\n"
		sl[l(sl)] <- "<LineStyle>\n"
		sl[l(sl)] <- "<color>" %+% lcolor[tt] %+% "</color>\n"
		sl[l(sl)] <- "<width>" %+% 2 %+% "</width>\n"
		sl[l(sl)] <- "</LineStyle>\n"
		sl[l(sl)] <- "</Style>\n"
		sl[l(sl)] <- "<LineString>\n"
		sl[l(sl)] <- "<extrude>" %+% 1 %+% "</extrude>\n"
		sl[l(sl)] <- "<tessellate>" %+% 1 %+% "</tessellate>\n"
		sl[l(sl)] <- "<altitudeMode>" %+% "clampToGround" %+% 
			"</altitudeMode>\n"
		sl[l(sl)] <- "<coordinates>\n"
		sl[l(sl)] <- paste(fit$most.prob.track[, 1], fit$most.prob.track[, 2], rep("0\n", 
				           nrow(fit$most.prob.track)), sep = ",", collapse = "")
		sl[l(sl)] <- "</coordinates>\n"
		sl[l(sl)] <- "</LineString>\n"
		sl[l(sl)] <- "</Placemark>\n"
		## Not writing this track by default
		if ((turnon[2]==1) & !is.null(fit$pred.track)){
			fit$pred.track[, 1] <- (fit$pred.track[, 1] + 180)%%360 - 180
		    sl[l(sl)] <- "<Placemark>\n"			
			sl[l(sl)] <- "<name>" %+% "Predicted track" %+% "</name>\n"
			sl[l(sl)] <- "<description>" %+% "Predicted track" %+% "</description>\n"
			sl[l(sl)] <- "<visibility>" %+% showpath[2] %+% "</visibility>\n"
			sl[l(sl)] <- "<open>" %+% 0 %+% "</open>\n"
			sl[l(sl)] <- "<Style>\n"
			sl[l(sl)] <- "<LineStyle>\n"
			sl[l(sl)] <- "<color>" %+% "ffccffcc" %+% "</color>\n"
			sl[l(sl)] <- "<width>" %+% 2 %+% "</width>\n"
			sl[l(sl)] <- "</LineStyle>\n"
			sl[l(sl)] <- "</Style>\n"
			sl[l(sl)] <- "<LineString>\n"
			sl[l(sl)] <- "<extrude>" %+% 1 %+% "</extrude>\n"
			sl[l(sl)] <- "<tessellate>" %+% 1 %+% "</tessellate>\n"
			sl[l(sl)] <- "<altitudeMode>" %+% "clampToGround" %+% 
				"</altitudeMode>\n"
			sl[l(sl)] <- "<coordinates>\n"
			sl[l(sl)] <- paste(fit$pred[, 1], fit$pred[, 2], rep("0\n", 
				nrow(fit$pred)), sep = ",", collapse = "")
			sl[l(sl)] <- "</coordinates>\n"
			sl[l(sl)] <- "</LineString>\n"
			sl[l(sl)] <- "</Placemark>\n"
		}
		## Not writing this track by default
		if ((turnon[3]==1) & !is.null(fit$nominal.track)){
			fit$nominal.track[, 1] <- (fit$nominal.track[, 1] + 180)%%360 - 180
			sl[l(sl)] <- "<Placemark>\n"
			sl[l(sl)] <- "<name>" %+% "Raw geolocations" %+% "</name>\n"
			sl[l(sl)] <- "<description>" %+% "Raw geolocations" %+% "</description>\n"
			sl[l(sl)] <- "<visibility>" %+% showpath[3] %+% "</visibility>\n"
			sl[l(sl)] <- "<open>" %+% 0 %+% "</open>\n"
			sl[l(sl)] <- "<Style>\n"
			sl[l(sl)] <- "<LineStyle>\n"
			sl[l(sl)] <- "<color>" %+% "ff000033" %+% "</color>\n"
			sl[l(sl)] <- "<width>" %+% 2 %+% "</width>\n"
			sl[l(sl)] <- "</LineStyle>\n"
			sl[l(sl)] <- "</Style>\n"
			sl[l(sl)] <- "<LineString>\n"
			sl[l(sl)] <- "<extrude>" %+% 1 %+% "</extrude>\n"
			sl[l(sl)] <- "<tessellate>" %+% 1 %+% "</tessellate>\n"
			sl[l(sl)] <- "<altitudeMode>" %+% "clampToGround" %+% 
				"</altitudeMode>\n"
			sl[l(sl)] <- "<coordinates>\n"
			sl[l(sl)] <- paste(fit$nominal.track[, 1], fit$nominal.track[, 
				2], rep("0\n", nrow(fit$nominal.track)), sep = ",", collapse = "")
			sl[l(sl)] <- "</coordinates>\n"
			sl[l(sl)] <- "</LineString>\n"
			sl[l(sl)] <- "</Placemark>\n"
		}
		## Not writing this track by default
		if ((turnon[4]==1) & !is.null(fit$var.most.prob.track)){
			sl[l(sl)] <- "<Placemark>\n"		
			sl[l(sl)] <- "<name>" %+% "Confidence region" %+% "</name>\n"
			sl[l(sl)] <- "<description>" %+% "Confidence region" %+% 
				"</description>\n"
			sl[l(sl)] <- "<visibility>" %+% showpath[4] %+% "</visibility>\n"
			sl[l(sl)] <- "<open>" %+% 0 %+% "</open>\n"
			sl[l(sl)] <- "<Style>\n"
			sl[l(sl)] <- "<PolyStyle>\n"
			sl[l(sl)] <- "<color>" %+% "880000ff" %+% "</color>\n"
			sl[l(sl)] <- "<fill>" %+% 0 %+% "</fill>\n"
			sl[l(sl)] <- "<outline>" %+% 1 %+% "</outline>\n"
			sl[l(sl)] <- "</PolyStyle>\n"
			sl[l(sl)] <- "</Style>\n"
			sl[l(sl)] <- "<MultiGeometry>\n"
			cip <- apply(cbind(fit$var.most.prob.track, fit$most.prob.track), 
				         1, .getCI, level = level, npoints = npoints)
			cip <- round(cip, 3)
			for (i in 1:ncol(cip)) {
					sl[l(sl)] <- "<Polygon>\n"
					sl[l(sl)] <- "<altitudeMode>" %+% "relativeToGround" %+% 
						"</altitudeMode>\n"
					sl[l(sl)] <- "<outerBoundaryIs>\n<LinearRing>\n<coordinates>\n"
					sl[l(sl)] <- paste(cip[1:npoints, i], cip[(npoints + 
						1):(2 * npoints), i], rep("0\n", nrow(cip)), sep = ",", 
						collapse = "")
					sl[l(sl)] <- "</coordinates>\n</LinearRing>\n</outerBoundaryIs>\n</Polygon>\n"
				}
			sl[l(sl)] <- "</MultiGeometry>\n"
			sl[l(sl)] <- "</Placemark>\n"
		}
		if ((!is.null(fit$fix.first)&& fit$fix.first==1)|!is.null(fit$decimal.date)) {
			sl[l(sl)] <- "<Placemark>\n"
			sl[l(sl)] <- "<name>" %+% "Release point" %+% "</name>\n"
			sl[l(sl)] <- "<description>" %+% paste("Lon: ", fit$most.prob.track[1, 1], 
			                                       ", Lat: ", fit$most.prob.track[1, 2], sep="") %+% 
				"</description>\n"
			sl[l(sl)] <- "<styleUrl>#mydeploy</styleUrl>\n"				
			sl[l(sl)] <- "<visibility>" %+% showpoint[1] %+% "</visibility>\n"
			sl[l(sl)] <- "<open>" %+% 0 %+% "</open>\n"
			sl[l(sl)] <- "<Point>\n"
			sl[l(sl)] <- "<extrude>" %+% 1 %+% "</extrude>\n"
			sl[l(sl)] <- "<tessellate>" %+% 1 %+% "</tessellate>\n"
			sl[l(sl)] <- "<altitudeMode>" %+% "relativeToGround" %+% 
				"</altitudeMode>\n"
			sl[l(sl)] <- "<coordinates>\n"
			sl[l(sl)] <- paste(fit$most.prob.track[1, 1], fit$most.prob.track[1, 2], 
			                   "0\n", sep = ",")
			sl[l(sl)] <- "</coordinates>\n"
			sl[l(sl)] <- "</Point>\n"
			sl[l(sl)] <- "</Placemark>\n"
		}
		if ((!is.null(fit$fix.last)&& fit$fix.last==1)|!is.null(fit$decimal.date)) {
		    n <- nrow(fit$most.prob.track)
			sl[l(sl)] <- "<Placemark>\n"
			sl[l(sl)] <- "<name>" %+% "Recapture point" %+% "</name>\n"
			sl[l(sl)] <- "<description>" %+% paste("Lon: ", fit$most.prob.track[n, 1], 
			                                       ", Lat: ", fit$most.prob.track[n, 2], sep="") %+% 
				"</description>\n"
			sl[l(sl)] <- "<styleUrl>#mypopup</styleUrl>\n"					
			sl[l(sl)] <- "<visibility>" %+% showpoint[2] %+% "</visibility>\n"
			sl[l(sl)] <- "<open>" %+% 0 %+% "</open>\n"
			sl[l(sl)] <- "<Point>\n"
			sl[l(sl)] <- "<extrude>" %+% 1 %+% "</extrude>\n"
			sl[l(sl)] <- "<tessellate>" %+% 1 %+% "</tessellate>\n"
			sl[l(sl)] <- "<altitudeMode>" %+% "relativeToGround" %+% 
				"</altitudeMode>\n"
			sl[l(sl)] <- "<coordinates>\n"
			sl[l(sl)] <- paste(fit$most.prob.track[n, 1], fit$most.prob.track[n, 2], 
				               "0\n", sep = ",")
			sl[l(sl)] <- "</coordinates>\n"
			sl[l(sl)] <- "</Point>\n"
			sl[l(sl)] <- "</Placemark>\n"
		}
		flag <- ifelse(is.null(fit$decimal.date),0,1)
		sl[l(sl)] <- "<Folder>\n" 
		sl[l(sl)] <- "<name>" %+% "Individual points" %+% "</name>\n"
		for (n in 1:nrow(fit$most.prob.track)) 
		{
			  sl[l(sl)] <- "<Placemark>\n"
			  sl[l(sl)] <- "<visibility>" %+% showpoint[3] %+% "</visibility>\n"		  
			  if (flag==0) {
			        if (!is.null(fit$date)){					
							sl[l(sl)] <- "<description>" %+% oname %+% "\n" %+% as.character(strptime(paste(fit$date[n, 1], 
									fit$date[n, 2], fit$date[n, 3], sep = "/"), "%Y/%m/%d")) %+% "</description>\n"
					 } else {
					        stamp <- ISOdatetime(fit$year[n], fit$month[n], fit$day[n], 
							                     ifelse(is.null(fit$hour[n]),0, fit$hour[n]), 
												 ifelse(is.null(fit$min[n]),0, fit$min[n]), 
												 ifelse(is.null(fit$sec[n]),0, fit$sec[n]), tz = "GMT")
							sl[l(sl)] <- "<description>" %+% oname %+% "\n" %+% stamp %+% "</description>\n"					 }
			  } else {
			        # trackit 
					mdy <- date.mdy(fit$date[n])
					ddate <- fit$decimal.date[n]
					dhr <- ddate-trunc(ddate)
					hour <- trunc(dhr*24)
					min <- trunc((dhr*24-trunc(dhr*24))*60)
					stamp <- ISOdatetime(mdy$year, mdy$month, mdy$day, hour, min, 0, tz = "GMT") + 2*60 
					# need to add back 2 minutes
			        sl[l(sl)] <- "<description>" %+% oname %+% "\n" %+% stamp %+% "</description>\n"
			  }
			  sl[l(sl)] <- "<TimeStamp>\n"
			  sl[l(sl)] <- "<when>"
			  if ((flag==0) & (!is.null(fit$date))) {
				sl[l(sl)] <- as.character(strptime(paste(fit$date[n, 1], fit$date[n, 2],
						                  fit$date[n, 3], sep = "/"), "%Y/%m/%d"))
			  } else {		  
				sl[l(sl)] <- paste(sub(" ", "T", stamp), ifelse(is.null(fit$hour[n]),"","Z"), sep="") 
			  }
			  sl[l(sl)] <- "</when>\n"
			  sl[l(sl)] <- "</TimeStamp>\n"
			  sl[l(sl)] <- "<styleUrl> myicon" %+% tt %+% " </styleUrl>\n"
			  sl[l(sl)] <- "<Point>\n"
			  sl[l(sl)] <- "<coordinates>"
			  sl[l(sl)] <- paste(fit$most.prob.track[n, 1], fit$most.prob.track[n, 2], sep = ",")
			  sl[l(sl)] <- "</coordinates>\n"
			  sl[l(sl)] <- "</Point>\n"
			  sl[l(sl)] <- "</Placemark>\n"
			  
		}
		sl[l(sl)] <- "</Folder>\n"
		## Height bars
		if (exists("hdat")){
			flag <- ifelse(is.null(fit$decimal.date),0,1)
			if (is.na(rans)){
				bbb <- rep(circle.size,nrow(fit$most.prob.track))} 
			else {
			    bbb <- quick.n.dirty(udat, rans, circle.size*bubble.scale)
			}		
			vv <- data.frame(bbb,0,0,bbb)
			cip <- apply(cbind(vv, fit$most.prob.track), 
				         1, .getCI, level = level, npoints = npoints)
			cip <- round(cip, 3)
		    sl[l(sl)] <- "<Folder>\n" 
		    sl[l(sl)] <- "<name>" %+% "Bar chart" %+% "</name>\n"			
			for (i in 1:ncol(cip)) {
			  sl[l(sl)] <- "<Placemark>\n"		
			  sl[l(sl)] <- "<name>" %+% height.data %+% ": " %+% hdat[i] %+% "</name>\n"
			  sl[l(sl)] <- "<visibility>" %+% showpath[4] %+% "</visibility>\n"
			  sl[l(sl)] <- "<open>" %+% 0 %+% "</open>\n"	
			  if (flag==0) {
			        if (is.null(fit$date)) {
					        stamp <- ISOdatetime(fit$year[i], fit$month[i], fit$day[i], 
							                     ifelse(is.null(fit$hour[i]),0, fit$hour[i]), 
												 ifelse(is.null(fit$min[i]),0, fit$min[i]), 
												 ifelse(is.null(fit$sec[i]),0, fit$sec[i]), tz = "GMT")				
					 }
			  } else {
			        # trackit 
					mdy <- date.mdy(fit$date[i])
					ddate <- fit$decimal.date[i]
					dhr <- ddate-trunc(ddate)
					hour <- trunc(dhr*24)
					min <- trunc((dhr*24-trunc(dhr*24))*60)
					stamp <- ISOdatetime(mdy$year, mdy$month, mdy$day, hour, min, 0, tz = "GMT") + 2*60 
			  }
			  sl[l(sl)] <- "<TimeStamp>\n"
			  sl[l(sl)] <- "<when>"
			  if ((flag==0) & (!is.null(fit$date))) {
				ds <- as.character(strptime(paste(fit$date[i, 1], fit$date[i, 2],
						                  fit$date[i, 3], sep = "/"), "%Y/%m/%d"))
				sl[l(sl)] <- ds
			  } else {		  
				ds <- paste(sub(" ", "T", stamp), ifelse(is.null(fit$hour[i]),"","Z"), sep="") 
				sl[l(sl)] <- ds
			  }
			  sl[l(sl)] <- "</when>\n"
			  sl[l(sl)] <- "</TimeStamp>\n"
			  sl[l(sl)] <- "<description>" %+% oname %+% "\n" %+% ds %+% 
				"</description>\n"
			  sl[l(sl)] <- "<Style>\n"
			  sl[l(sl)] <- "<PolyStyle>\n"
              mycol <- 	ifelse(exists("bcol"), bcol[i], lcolor[tt])
			  sl[l(sl)] <- "<color>" %+% substring(rgb(0,0,0,alpha),8,9) %+% mycol %+% "</color>\n"
			  sl[l(sl)] <- "<fill>" %+% 1 %+% "</fill>\n"
			  sl[l(sl)] <- "<outline>" %+% 0 %+% "</outline>\n"
			  sl[l(sl)] <- "</PolyStyle>\n"
			  sl[l(sl)] <- "</Style>\n"
			  sl[l(sl)] <- "<MultiGeometry>\n"
              hh <- hdat[i] * scale.height
			  hh <- hh %+% "\n"
			  sl[l(sl)] <- "<Polygon>\n"
			  sl[l(sl)] <- "<extrude>1</extrude>\n"
			  sl[l(sl)] <- "<altitudeMode>" %+% "relativeToGround" %+% 
						"</altitudeMode>\n"
			  sl[l(sl)] <- "<outerBoundaryIs>\n<LinearRing>\n<coordinates>\n"
			  sl[l(sl)] <- paste(cip[1:npoints, i], cip[(npoints + 
						1):(2 * npoints), i], rep(hh, nrow(cip)), sep = ",", 
						collapse = "")
			  sl[l(sl)] <- "</coordinates>\n</LinearRing>\n</outerBoundaryIs>\n</Polygon>\n"
			  sl[l(sl)] <- "</MultiGeometry>\n"
			  sl[l(sl)] <- "</Placemark>\n"			  
			  }
			  sl[l(sl)] <- "</Folder>\n"
		}
	sl[l(sl)] <- "</Folder>\n"
    }
	### Add color scale for bar chart
	if (file.exists(paste(folder,'files','barcolors.png',sep="/"))){	
	sl[l(sl)] <- "<Folder>\n"
	sl[l(sl)] <- "<name> Bar legend </name>\n"
	lab <- "Color: " %+% color.data %+% "\n" %+% "Size: " 
	sl[l(sl)] <- "<description>" %+% lab %+% "</description>\n"		
	sl[l(sl)] <- "<ScreenOverlay>\n"
	sl[l(sl)] <- "<name> " %+% color.data %+% " </name>\n"
    sl[l(sl)] <- "<visibility>" %+% 0 %+% "</visibility>\n"    
    sl[l(sl)] <- "<Icon>\n"
    sl[l(sl)] <- "<href>" %+% "files/" %+% "barcolors.png" %+% "</href>\n"
    sl[l(sl)] <- "</Icon>\n"
    sl[l(sl)] <- '<overlayXY x="0.995" y=".448" xunits="fraction" yunits="fraction"/>\n'
    sl[l(sl)] <- '<screenXY x="0.995" y=".448" xunits="fraction" yunits="fraction"/>\n'
    sl[l(sl)] <- '<rotationXY x="0" y="0" xunits="fraction" yunits="fraction"/>\n'
    sl[l(sl)] <- '<size x="0" y="0" xunits="fraction" yunits="fraction"/>\n'
    sl[l(sl)] <- "</ScreenOverlay>\n"
	sl[l(sl)] <- "</Folder>\n"
    }	
	### Wrapping up!
    sl[l(sl)] <- "</Document>\n"
    sl[l(sl)] <- "</kml>\n"
    cat(unlist(sl), file = paste(folder, kmlname, sep = "/"))
	options(warn=1)
}

satellite.kml <- 
function (track, folder = getwd(), local = F, 
    product = "TBAssta", interval = 5, 
    server = "http://coastwatch.pfeg.noaa.gov/coastwatch/CWBrowserWW360.jsp?get=gridData&dataSet=")
{
    require(date)
	require(png)
    fmtDate <- function(date) {
        x <- date.mdy(date)
        paste(x$year, formatC(x$month, digits = 1, flag = "0", 
            format = "d"), formatC(x$day, digits = 1, flag = "0", 
            format = "d"), sep = "-")
    }
    fmtDay <- function(day) {
        formatC(day, digits = 2, flag = "0", format = "d")
    }
    fl <- dir(folder)
    if (length(fl) != 0) {
        folder <- paste(folder, "images", sep = "/")
        dir.create(folder)
    }
    substring.location <- function (text, string, restrict) 
    {
    if (length(text) > 1) 
        stop("only works with a single character string")
    l.text <- nchar(text)
    l.string <- nchar(string)
    if (l.string > l.text) 
        return(list(first = 0, last = 0))
    if (l.string == l.text) 
        return(if (text == string) list(first = 1, last = l.text) else list(first = 0, 
            last = 0))
    is <- 1:(l.text - l.string + 1)
    ss <- substring(text, is, is + l.string - 1)
    k <- ss == string
    if (!any(k)) 
        return(list(first = 0, last = 0))
    k <- is[k]
    if (!missing(restrict)) 
        k <- k[k >= restrict[1] & k <= restrict[2]]
    if (length(k) == 0) 
        return(list(first = 0, last = 0))
    list(first = k, last = k + l.string - 1)
    }
    dat.period <- function(file)
    {
     beginopt <- which(file=="<!--BeginOptions-->")
     endopt <- which(file=="<!--EndOptions-->")
     list1 <- list()
     list2 <- list()
     for (i in 1:(endopt - beginopt - 1))
     {
     starti <- substring.location(file[beginopt+i], "centeredTime=\">")$last+1
     endi <- substring.location(file[beginopt+i], "</a>")$first-1
     list1[i] <- substr(file[beginopt+i], starti, endi)
     list2[i] <- gsub("day", "", list1[i])
     list2[i] <- as.numeric(gsub("1month", "30", list2[i]))
     }
     if (length(which(is.na(list2)))>0) {list1 <- list1[-which(is.na(list2))]}
     list2 <- na.omit(list2)
     return(list(txt=unlist(list1), num=unlist(list2)))
    }
    dat.times <- function(file)
    {
     beginopt <- which(file=="<!--BeginOptions-->")
     endopt <- which(file=="<!--EndOptions-->")
     list1 <- list()
     for (i in 1:(endopt - beginopt - 1))
     {
     starti <- substring.location(file[beginopt+i], "minLon=null\">")$last+1
     endi <- substring.location(file[beginopt+i], "</a>")$first-1
     list1[i] <- substr(file[beginopt+i], starti, endi)
     dvec <- as.numeric(unlist(strsplit(unlist(strsplit(unlist(list1[i]), "T"))[1],"-")))
     if (i ==1)
      {list2 <- mdy.date(dvec[2], dvec[3], dvec[1])
      }else {list2[i] <- mdy.date(dvec[2], dvec[3], dvec[1])}
     }
     return(list(txt=unlist(list1), dates=list2))
    }
    server <- paste(server, product, sep="")   
    ### Check with server the available intervals
    dlink <- paste(server, "&timePeriod=", sep="")
    download.file(dlink, "interval.html", mode = "wb")
    dfile <- readLines("interval.html")
    unlink("interval.html")
    dinfo <- dat.period(dfile)
    nfreq <- dinfo$num
    cat("\n--- Querying the sampling time interval --- \n\n")
    cat("Sampling period available from server: ", dinfo$txt, "\n")
    deltat <- length(which(nfreq <= interval))
    deltat <- ifelse(deltat==0, 1, deltat)
    nday  <- dinfo$txt[deltat]
    cat("Your specified interval is:", interval, " day(s)\n")
    cat("\n--- Auto-selecting the sampling period:", nday, " ---\n\n")
    ### Check with server the available times
    dlink <- paste(dlink, nday, "&centeredTime=", sep="")
    download.file(dlink, "times.html", mode = "wb")
    dfile <- readLines("times.html")
    unlink("times.html")
    dinfo <- dat.times(dfile)   
    ### Get request range from track
    if (is.data.frame(track)) 
        track <- list(track)
    minDate <- min(unlist(lapply(track, function(x) mdy.date(x[1, 
        2], x[1, 3], x[1, 1]))))
    maxDate <- max(unlist(lapply(track, function(x) mdy.date(x[nrow(x), 
        2], x[nrow(x), 3], x[nrow(x), 1]))))		
    minLon <- min(unlist(lapply(track, function(x) min(x[, 4])))) - 
        5
    maxLon <- max(unlist(lapply(track, function(x) max(x[, 4])))) + 
        5
    minLat <- min(unlist(lapply(track, function(x) min(x[, 5])))) - 
        5
    maxLat <- max(unlist(lapply(track, function(x) max(x[, 5])))) + 
        5
    idxDmin <- length(which(dinfo$dates<= minDate))
    idxDmax <- length(which(dinfo$dates<= maxDate))+ 
               ifelse(((maxDate>=max(dinfo$dates))|
			(maxDate<min(dinfo$dates))|
			(minDate>max(dinfo$dates))),0,1)
    if (!(idxDmin==idxDmax)){ 
      if (minDate < min(dinfo$dates)) {
		ifelse(idxDmax>0, idxDmin <- 1)
		warning("Your track(s) start(s) before any
				satellite imagery is available, which begins at", 
				min(dinfo$dates))}
	  if (maxDate > max(dinfo$dates)) {
		warning("Your track(s) end(s) after the available
				satellite imagery series , which finishes at", 
				max(dinfo$dates))}
     }else warning("\n\n No satellite imagery is available for \"", product,
				   "\" during your data range!!\n\n  ",
				   "Please check CoastWatch website for listings at \n", 
				   "       http://coastwatch.pfeg.noaa.gov/coastwatch/CWBrowserWW360.jsp", "\n")
    dsteps <- seq(idxDmin, idxDmax+1, by = nfreq[deltat])
    if (idxDmin == idxDmax) {dsteps <- 0}
    missed <- which((c(dinfo$dates[-1],dinfo$dates[length(dinfo$dates)]) 
		     -dinfo$dates)>(nfreq[deltat]-1))
    missed <- missed[which(missed>idxDmin)]
    missed <- missed[which(missed<idxDmax)]
    all <- c(dsteps, missed)
    datesteps <- na.omit(dinfo$txt[sort(all)])
    ### Downloading imagery
    op <- "&timePeriod=NDAY&centeredTime=~DATE&maxLat=MAXLAT&minLon=MINLON&maxLon=MAXLON&minLat=MINLAT&filetype=GoogleEarth"
	uu <- 1	# dummy counter for getting image color bar/ legend
	uk <- NA # filename for legend .png
    for (d in datesteps) {
        opt <- sub("NDAY", nday, op)
        opt <- sub("DATE", d, opt)
        opt <- sub("MAXLAT", maxLat, opt)
        opt <- sub("MINLON", minLon, opt)
        opt <- sub("MAXLON", maxLon, opt)
        opt <- sub("MINLAT", minLat, opt)
        link <- paste(server, opt, sep = "")
		requestdate <- unlist(strsplit(d, "T"))[1]
        filename <- paste(product, requestdate, ".kml", sep = "")
        dest <- paste(folder, filename, sep = "/")
        download.file(link, dest, mode = "wb")
        kmlfile <- readLines(dest)
		unlink(dest)
        ### Add TimeSpan to kml
        idx1 <- which(kmlfile == "  <GroundOverlay>")+1
		idx2 <- which(kmlfile == "</kml>")
		cday<- as.Date(substr(kmlfile[6], 26, 35))
		begind <- cday - trunc(nfreq[deltat]/2) 
		endd <- (trunc(nfreq[deltat]/2) - ifelse(nfreq[deltat]%%2, 0, 1))*60*60*24
		ctime1 <- substr(kmlfile[6], 37, 44)
		ctime2 <- substr(kmlfile[6], 26, 44)
		ctime2 <- format(strptime(ctime2, "%Y-%m-%d %H:%M:%S", 
						  tz="GMT")+ endd + 60*60*24 -1, 
						  usetz=F, format="%Y-%m-%dT%H:%M:%SZ")
		addtag    <- "<TimeSpan>"
		addtag[2] <- paste("<begin>", begind, "T", ctime1, "Z</begin>", sep="")
		addtag[3] <- paste("<end>", ctime2, "</end>", sep="")
		addtag[4] <- "</TimeSpan>"
		idx3 <- which(kmlfile == "    <name>Logo</name>")+4
		kmlfile[idx3] <- gsub("0.005", "0.97", kmlfile[idx3])
		kmlfile[idx3+1] <- gsub("0.005", "0.97", kmlfile[idx3+1])
		kmlfile[idx3] <- gsub(".04", "0.1", kmlfile[idx3])
		kmlfile[idx3+1] <- gsub(".04", "0.1", kmlfile[idx3+1])
		### Download and crop color legend
		if (uu == 1){
			ulink <- gsub("GoogleEarth", "medium.png", link)
			uk <- paste("ColorBar-",product,".png",sep = "")
			ug <- paste(folder, uk, sep = "/")
			download.file(ulink, ug, mode = "wb")
			uu = 100
			um <- readPNG(ug)
			ux <- round(dim(um)[1]*0.815)
			uy <- dim(um)[1]
            writePNG(um[ux:uy,,], ug)
		}			
		### Download image png files to a local folder
		if (local==T) 
		{
			iname <- paste("img", substr(kmlfile[idx1], 
						   substring.location(kmlfile[idx1], "<name>")$last+1, 
						   substring.location(kmlfile[idx1], "_x")$first-1), ".png", sep="")
			idest <- paste(folder, iname, sep = "/")
			ilink <- gsub("GoogleEarth", "transparent.png", link)
			download.file(ilink, idest, mode = "wb")
			kmlfile[idx1+2] <- paste("<href>", iname, "</href>", sep="")
			} else{
			kmlfile[idx1+2] <- gsub("Time=", "Time=~", kmlfile[idx1+2])
		}
		kmlfile <- c(kmlfile[1:idx1], addtag, kmlfile[(idx1+1):idx2]) 
		dest <- paste(folder, "/", product, as.character(cday), 
					  ".kml", sep = "")
		cat("to ", dest,"\n\n")
		writeLines(kmlfile, dest)
    }
    cat(paste(rep(".", options()$width), collapse = ""), "\n\n")
    cat("Downloaded", length(dir(folder, pattern=product)), "files to:\n\n  ", 
        folder, "\n\n")
    return(list(folder=folder, nday=nday, nodataflag=ifelse((idxDmax==idxDmin),1,0), cbpng=uk))
}

get.erddap <- 
function (track, every.day=5, folder = tempdir(), 
          # change every.day to a smaller number to get SST images at a more frequent basis
          server = 'http://coastwatch.pfeg.noaa.gov/erddap', 
		  # server = 'http://upwell.pfeg.noaa.gov/erddap', for more choices
		  type=2, 
          # see "filetype" below for file formats:
		  # c('.csv', '.tsv', '.nc', '.kml', '.largePng')
          product = 'ncdcOisst2Agg', variable='sst', at=1, 
		  depth = c(0,0), yearless = F, 
		  kml.image = F, colorbar = "|||||", full.extent = F, 
		  # an example of color bar, Rainbow2|D|Linear|10|20|30
	      name = NA, repack = TRUE, trim = TRUE, mybox = NA) 
{
    ### More info on: http://coastwatch.pfeg.noaa.gov/erddap/info/index.html
    ### product <- c('ncdcOisst2Agg', 'ncdcOisst2AmsrAgg', 'erdG1ssta1day', 'erdBAssta5day')
    ### variable <- c('sst', 'anom', 'err')
    require(date)
	require(png)
    filetype <- c('.csv', '.tsv', '.nc', '.kml', '.largePng')
	link <- paste(server, "/info/", product, "/index.html", sep="")	
    server <- paste(server, "/griddap/", product, sep="")
    fmtDate <- function(date, yearless=F) {
        x <- date.mdy(date)
		yy <- x$year
		if (yearless) yy <- "0000"
        paste(yy, 
		    formatC(x$month, digits = 1, flag = "0", 
            format = "d"), formatC(x$day, digits = 1, flag = "0", 
            format = "d"), sep = "-")
    }
    fmtDay <- function(day) {
        formatC(day, digits = 2, flag = "0", format = "d")
    }
    testdir <- file.info(folder)$isdir
    if (is.na(testdir)) {
        dir.create(folder)
    }
    else {
        if (!testdir) 
            stop("The folder name supplied is in fact a filename")
    }
    fl <- dir(folder)
    if (length(fl) != 0) {
        folder <- paste(folder, "images", sep = "/")
        dir.create(folder)
    }
	### Turn all lon to 0-360
	track$lon[which(track$lon<0)] <- track$lon[which(track$lon<0)] + 360
	if (is.data.frame(track)) 
        track <- list(track)
	minLon <- min(unlist(lapply(track, function(x) min(x[, 4])))) - 
        5
    maxLon <- max(unlist(lapply(track, function(x) max(x[, 4])))) + 
        5
    minLat <- min(unlist(lapply(track, function(x) min(x[, 5])))) - 
        5
    maxLat <- max(unlist(lapply(track, function(x) max(x[, 5])))) + 
        5
    minDate <- min(unlist(lapply(track, function(x) mdy.date(x[1, 
        2], x[1, 3], x[1, 1]))))
    maxDate <- max(unlist(lapply(track, function(x) mdy.date(x[nrow(x), 
        2], x[nrow(x), 3], x[nrow(x), 1]))))
    minDate <- minDate - 10
    maxDate <- maxDate + 10	
    datesteps <- seq(minDate, maxDate, by = every.day)
	if (length(mybox)==4){
		minLon <- mybox[1]
		maxLon <- mybox[2]
		minLat <- mybox[3]
		maxLat <- mybox[4]
	}
	### Check metadata for lat/lon range
	dest <- paste(folder, "index.html", sep = "/")
	download.file(link, dest, mode = "wb")
	htm <- readLines(dest)
	unlink(dest)
	p1 <- '[(DATE):1:(DATE)]'
	p2 <- '[(DEPTH1):1:(DEPTH2)]'
	p3 <- '[(MINLAT):STRIDE:(MAXLAT)]'
	p4 <- '[(MINLON):STRIDE:(MAXLON)]'	
	rr <- htm[grep("actual_range", htm)+2]
	if (length(rr)==4){
		p1 <- paste(p1,p2,sep="")
	}
	rr <- tail(rr,2)
	rr <- gsub("<td>", "", rr)
    rr <- gsub("</td>", ",", rr)
	rr <- as.numeric(unlist(strsplit(rr, ",")))
	### lon goes from -180
	if (rr[3] < 0){
		minLon <- (minLon + 180)%%360 - 180
		maxLon <- (maxLon + 180)%%360 - 180
	}
	### download everything if it is out of range
	if (minLat < rr[1]) full.extent <- T
	if (maxLat > rr[2]) full.extent <- T	
	if (minLon < rr[3]) full.extent <- T
	if (maxLon > rr[4]) full.extent <- T
	if (maxLon < minLon) full.extent <- T
    op <- paste(server, filetype[type], sep="")
    op <- paste(op, "?", variable, sep="")
    opt <- paste(p1,p3,p4, sep="")
    if (full.extent) opt <- paste(p1,'[][]', sep="")	
    myurl <- opt
    uu <- 1	# dummy counter for getting image color bar/ legend
	uk <- NA # filename for legend .png
    for (d in datesteps) {
        opt <- myurl
        opt <- gsub("DATE", fmtDate(d, yearless), opt)
        opt <- sub("MAXLAT", maxLat, opt)
        opt <- sub("MINLON", minLon, opt)
        opt <- sub("MAXLON", maxLon, opt)
        opt <- sub("MINLAT", minLat, opt)	
	    opt <- gsub("STRIDE", at, opt)
		opt <- gsub("DEPTH1", depth[1], opt)
		opt <- gsub("DEPTH2", depth[2], opt)		
        link <- paste(op, opt, sep = "")
        name <- ifelse(is.na(name), product, name)
        ext <- sub('large', '', filetype[type])
        if ((repack==TRUE)&&(ext=='.tsv')){
	    ### !! The date constructs for the filename are different - the first date is the image date
        ### !! Unlike old code that centers the image date in between d1 and d2 (+/- days to position it)
	      y1 <- date.mdy(d)$year
              d1 <- d - mdy.date(month = 1, day = 1, year = y1) + 1
              y2 <- date.mdy(d + every.day - 1)$year
              d2 <- (d + every.day -1) - mdy.date(month = 1, 
                     day = 1, year = y2) + 1
              filename <- paste(substr(name,1,2), y1, fmtDay(d1), 
                         "_", y2, fmtDay(d2), "_", product, ".xyz", sep = "")}
        else {filename <- paste(name, "_", as.Date(as.date(d)), ext, sep="")}
        dest <- paste(folder, filename, sep = "/")
	    download.file(link, dest, mode = "wb")
        if ((repack==T)&&(ext=='.tsv')){
	    tmp <- read.table(dest, skip = 2)[, c(-1,-2)]
            if (trim) {tmp <- tmp[complete.cases(tmp), ]}
            write.table(tmp, file = dest, 
                        quote = FALSE, row.names = FALSE, col.names = FALSE)
	    }
		if (ext=='.kml'){
			kmlfile <- readLines(dest)
			unlink(dest)
			# Find insert point
			ix <- which(kmlfile=="  <GroundOverlay>")[1]
			# Add TimeSpan to kml
			idx1 <- which(kmlfile == "  <GroundOverlay>")+1
			idx2 <- which(kmlfile == "</kml>")	
			begind <- as.Date(as.date(d))
			ctime1 <-  format.Date(begind, "%H:%M:%S")					
			ctime2 <-  format.Date(begind, "%Y-%m-%dT%H:%M:%S")
			endd <- (every.day-1)*60*60*24
			ctime2 <- format(strptime(ctime2, "%Y-%m-%dT%H:%M:%S", 
							  tz="GMT")+ endd + 60*60*24 -1, 
							  usetz=F, format="%Y-%m-%dT%H:%M:%SZ")			
			addtag    <- "<TimeSpan>"
			addtag[2] <- paste("<begin>", begind, "T", ctime1, "Z</begin>", sep="")
			addtag[3] <- paste("<end>", ctime2, "</end>", sep="")
			addtag[4] <- "</TimeSpan>"
			kmlfile <- c(kmlfile[1:(ix-1)], addtag, kmlfile[ix:length(kmlfile)])
			# Download and crop color legend
			if (uu == 1){
					ulink <- gsub(".kml", ".png", link)
					ulink <- paste(ulink, "&.size=500", sep="")
					ulink <- paste(ulink, "&.colorBar=", colorbar, sep="")
					uk <- paste("ColorBar-",product,
					            ifelse(nchar(colorbar)>5,gsub("[|]", "-", colorbar),""),
					            ".png",sep = "")
					ug <- paste(folder, uk, sep = "/")
					download.file(ulink, ug, mode = "wb")
					uu = 100
                    writePNG(readPNG(ug)[327:440,,], ug)
			}			
			# Download the image
			if (kml.image){
			    gg <- grep("transparent", kmlfile)
				ilink <- kmlfile[gg]
				ilink <- gsub("<href>", "", ilink)
				ilink <- gsub("</href>", "", ilink)
				ilink <- gsub("\\s","", ilink)
				for (qq in 1:20) ilink <- gsub(paste(":",qq,":", sep=""), ":1:", ilink)
				ilink <- paste(ilink, "&.colorBar=", colorbar, sep="")
				idest <- gsub(".kml", ".png", dest)
				download.file(ilink, idest, mode = "wb")
				kmlfile[gg] <- paste("<href>", tail(unlist(strsplit(idest, "/")), 1) , "</href>", sep="")
				kmlfile <- c(kmlfile[1:(gg-3)], 
                             paste("<name>", variable, " at ", depth[1], " meters", "</name>", sep=""),
				             paste("<description>", colorbar, "</description>", sep=""), 
				             kmlfile[(gg-2):length(kmlfile)])

			}
			writeLines(kmlfile, dest)
		}
    }
return(list(folder=folder, nday=every.day, datesteps=datesteps, nodataflag=0, cbpng=uk))
}

colorpalette <- function(n, pname="PantoneFall", 
                         myown=NA, alpha=1, reverse=F)
{
 `%+%` <- function(s1, s2) paste(s1, s2, sep = "")
 ### Palettes from Pantone - http://www.pantone.com/pages/MYP_myPantone/mypMemberProfileView.aspx?uID=536369
 PantoneFall <- c("dcb445","eb5153","ea4975","5f245c","868e5b","12373f","bd9e81","5e4739", "c7c8dc","789ea7")
 # https://www.pantone.com/pages/MYP_myPantone/mypPaletteDetail.aspx?p_id=1617&p_type=p_detail
 PantoneHappy <- c("D33479","F3BF08","6DCE87")
 Pastiche <- c("EFC050","E3868F","73B881","F79256","966E4B","BBA0CE","6CA0DC","355E91","383F36") 
 ### Palettes from the R package, RColorBrewer
 Spectral <- c("9E0142","D53E4F","F46D43","FDAE61","FEE08B","FFFFBF","E6F598","ABDDA4",
               "66C2A5","3288BD","5E4FA2")
 Accent <- c("D53E4F","F46D43","FDAE61","FEE08B","E6F598","ABDDA4","66C2A5","3288BD")
 Paired <- c("A6CEE3","1F78B4","B2DF8A","33A02C","FB9A99","E31A1C","FDBF6F","FF7F00","CAB2D6","6A3D9A")
 Set1 <- c("E41A1C","377EB8","4DAF4A","984EA3","FF7F00","FFFF33","A65628","F781BF")
 Set2 <- c("66C2A5","FC8D62","8DA0CB","E78AC3","A6D854","FFD92F","E5C494","B3B3B3")
 Set3 <- c("8DD3C7","FFFFB3","BEBADA","FB8072","80B1D3","FDB462","B3DE69","FCCDE5","D9D9D9","BC80BD","CCEBC5","FFED6F")
 Dark2 <- c("1B9E77","D95F02","7570B3","E7298A","66A61E","E6AB02","A6761D","666666")
 ### Other palettes from the web
 ColorBlindSafe <- c("999999", "E69F00", "56B4E9", "009E73", "F0E442", "0072B2", "D55E00", "CC79A7")
 Jet <- c("00007F","0000FF","007FFF","00FFFF","7FFF7F","FFFF00","FF7F00","FF0000","7F0000")
 ### LPRC
 Months <- c("FFFFBE","E6E600","FFAA00","FF7800","FF00C5","FF0000","A80000","73004C","004C73","005CE6","73DFFF","BEE8FF")
 #------------------------------------------------
 if (!is.na(myown)) assign(pname, myown)
 if (!exists(pname)) pname = "PantoneFall"
 pal <- get(pname)
 if (reverse) pal <- rev(pname)
 pal <- "#" %+% pal
 myRamp <- colorRampPalette(pal)
 ### Add a fixed value for alpha
 if (alpha) mycol <- myRamp(n) %+% substring(rgb(0,0,0,alpha),8,9)
 return(mycol)
}

color.lookup <- function(z, n, color.range=c(NA,NA),
                         pname="Jet", legend=1, log10.transform=FALSE, ...){
  z[which(is.infinite(z))] <- NA						 
  if (log10.transform) z <- log10(z)
  z[which(is.infinite(z))] <- NA
  z[which(is.nan(z))] <- NA
  if (sum(is.na(color.range)) == 0){
    sl <- diff(color.range)
	range01 <- function(z)(z-color.range[1])/sl
  } else {
    sl <- diff(range(na.omit(z)))	
    range01 <- function(z)(z-min(na.omit(z)))/sl
	color.range=range(na.omit(z))
  }
  rr <- range01(z)
  rr[which(rr <0)] <- 0
  rr[which(rr >1)] <- 1
  ndx <- which(is.na(rr))
  rr[ndx] <- 0
  cols <- colorRamp(colorpalette(n, pname))(rr)
  mycol <- apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
  # Assign NA/inf to white
  mycol[ndx] <- "#FFFFFF"
  if (legend == 1){
  if (pname!='Months'){
    tks <- pretty(color.range, 6)
	tp <- ifelse(tks[3]<0,2,0)
	if (log10.transform){
       tlb <- round(10^tks,tp)} 
	else {tlb <- tks}
	continuous.colorbar(n, pname, color.range[1], color.range[2], 
                 ticks=tks, labels=tlb, ...)
  } else {
	discrete.colorbar()
  }			 
  fl <- getwd()
  if (length(fl) != 0) {
        folder <- paste(fl, "files", sep = "/")
        dir.create(folder)
  }				 
  dev.copy(png, file=paste(folder,'barcolors.png',sep="/"), width=120, height=360)
  dev.off()}
  return(mycol)
} 

continuous.colorbar <- function(n=1000, pname="Jet", zmin, zmax=-zmin, 
                                nticks=11, ticks=seq(zmin, zmax, len=nticks), 
								title='', labels=ticks) {
    cp <- colorpalette(n, pname)
	sc = (n-1)/(zmax-zmin)
	par(las=1, mar=c(1,3,3,0.5)+0.1)
    plot(c(0,10), c(zmin,zmax), type='n', bty='n', xaxt='n', 
	     xlab='', yaxt='n', ylab='', xaxs = 'i', yaxs= 'i')
    axis(2, at=ticks, labels=labels, cex.axis=0.8)
	title(main=title, adj=0)
    for (i in 1:(n-1)) {
    	y = (i-1)/sc + zmin
    	rect(0,y,10,y+1/sc, col=cp[i], border="transparent")}
	box(lwd=2)
} 

discrete.colorbar <- function(n=12, pname="Months", title=pname, 
                              labels=month.abb){
	cp <- colorpalette(n, pname)
	par(las=1, mar=c(1,3,3,0.5)+0.1, 
	    col="white", col.axis="white", col.lab="white", col.main="white")
	plot(c(0,10), c(1, n + 1), type='n', bty='n', xaxt='n', 
		 xlab='', yaxt='n', ylab='', xaxs = 'i', yaxs= 'i')
	ticks <- c(1:n)+0.5	 
	axis(2, at = ticks, labels = labels, col="white")
	title(main=title, adj=0)
	for (i in 1:n) rect(0, i, 10, i+1, col=cp[i], border="transparent")
	box(lwd=2)
}

quick.n.dirty <- function(z, mi=NA, start.value = 0.001){  
  z[which(is.infinite(z))] <- NA
  if (is.na(mi)) mi <- min(na.omit(z))
  if (mi < 0) z <- z - mi
  #z <- log2(z+1) + 1
  z <- z * start.value
  z[which(is.na(z))] <- start.value
  return(z)
}


### Not run: color check
# n = 10; pie(rep(1, n), col = colorpalette(n, "Paired"), radius = 1)
### Not run: color assignment, color bar (colors are approx., not 100% mapping)
# xx <- c(-1:6)
# cols <- color.lookup(xx, n = 1000, color.range=c(-1,6)) 
# cols <- color.lookup(xx, n = length(seq(-1,4)), color.range=c(4,-1)) # reverse colors
# pie(1:length(xx), col = cols, radius = 1)
# continuous.colorbar(colorpalette(1000,"Jet"), -10, 60, ticks=xx)
# discrete.colorbar()