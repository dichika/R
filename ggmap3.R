ggmap3 <- function(locationdata, mapname, num){
	data <-  NULL
	for(count in 1:nrow(locationdata)){
		location <- locationdata[count, num]
			if(Sys.getlocale("LC_CTYPE")=="Japanese_Japan.932"){
				Encodelocation<-paste(c("",charToRaw(iconv(location,"CP932","UTF-8"))),collapse="%")
				}else{Encodelocation<-paste(c("",charToRaw(location)),collapse="%")
					}
		url <- paste("http://maps.google.com/maps/api/geocode/xml?address=",Encodelocation,"&sensor=false", sep="")
		xml <- getURL(url)
		lat <-as.numeric(xmlValue(xmlRoot(xmlTreeParse(xml))[["result"]][["geometry"]][["location"]][["lat"]]))
		lon <-as.numeric(xmlValue(xmlRoot(xmlTreeParse(xml))[["result"]][["geometry"]][["location"]][["lng"]]))
		data0 <- data.frame(lat=lat, lon=lon, size="mid", col="red", label=toupper(letters[count%%26]), stringsAsFactors=FALSE)
		data <- rbind(data, data0)
		}

	mymarkers <- ""
	for(i in 1:nrow(data)){
		col <- paste("color:", data[i,"col"], sep="")
		size <- paste("size:",data[i,"size"],  sep="")
		lab <- paste("label:",data[i,"label"], sep="")
		loc <- paste(data[i,"lat"], data[i,"lon"], sep=",")
		m1 <- paste(col, size, lab, loc, sep="|")
		mymarkers <- paste(mymarkers, m1, sep="&markers=")
		}
	mymarkers <- substr(mymarkers, start=10, stop=nchar(mymarkers))

	bb <- qbbox(lat = data$lat, lon = data$lon)
	MyMap <- GetMap2.bbox(bb$lonR, bb$latR, destfile = mapname, markers = mymarkers)
}

GetMap2 <- function (center, zoom = 12, markers, path, span, frame, hl, 
    sensor = "true", maptype = c("roadmap", "mobile", "satellite", 
        "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")[4], 
    format = c("gif", "jpg", "jpg-baseline", "png8", "png32")[5], 
    size = c(640, 640), destfile = "MyTile.png", RETURNIMAGE = TRUE, 
    GRAYSCALE = FALSE, verbose = 1) 
{
    stopifnot(all(size <= 640))
    fileBase <- substring(destfile, 1, nchar(destfile) - 4)
    fileExt <- substring(destfile, nchar(destfile) - 2, nchar(destfile))
    if (!missing(center) & !missing(zoom)) {
        MyMap <- list(lat.center = center[1], lon.center = center[2], 
            zoom = zoom)
        BBOX <- list(ll = XY2LatLon(MyMap, -size[1]/2 + 0.5, 
            -size[2]/2 - 0.5), ur = XY2LatLon(MyMap, size[1]/2 + 
            0.5, size[2]/2 - 0.5))
        MetaInfo <- list(lat.center = center[1], lon.center = center[2], 
            zoom = zoom, url = "google", BBOX = BBOX)
        save(MetaInfo, file = paste(destfile, "rda", sep = "."))
    }
    else {
        print("Note that when center and zoom are not specified, no meta information on the map tile can be stored. This basically means that R cannot compute proper coordinates. You can still download the map tile and view it in R but overlays are not possible. Do you want to proceed ? (y/n)")
        ans <- readLines(n = 1)
        if (ans != "y") 
            return()
    }
    if (length(size) < 2) {
        s <- paste(size, size, sep = "x")
    }
    else {
        s <- paste(size, collapse = "x")
    }
    if (!missing(center)) 
        center <- paste(center, collapse = ",")
    if (missing(format)) {
        if (fileExt == "jpg") 
            format <- "jpg"
        if (fileExt == "png") 
            format <- "png32"
    }
    googleurl <- "http://maps.google.com/maps/api/staticmap?"
    if (!missing(span)) {
        span <- paste(span, collapse = ",")
        url <- paste(googleurl, "center=", center, "&span=", 
            span, "&size=", s, "&maptype=", maptype, "&format=", 
            format, "&sensor=", sensor, sep = "")
    }
    else if (missing(center) & missing(zoom)) {
        stopifnot(!missing(markers))
        url <- paste(googleurl, "size=", s, "&maptype=", maptype, 
            "&format=", format, "&sensor=", sensor, sep = "")
    }
    else {
        stopifnot(!missing(center), !missing(zoom))
        url <- paste(googleurl, "center=", center, "&zoom=", 
            zoom, "&size=", s, "&maptype=", maptype, "&format=", 
            format, "&sensor=", sensor, sep = "")
    }
    if (!missing(markers)) {
        if (is.character(markers)) {
            markers.string <- markers
        }
        else if (is.data.frame(markers)) {
            for (i in 1:nrow(markers)) {
                m1 <- ""
                m <- paste(markers[i, c("lat", "lon")], collapse = ",")
                if (all(c("size", "col", "char") %in% colnames(markers))) {
                  m1 <- paste(markers[i, c("size", "col", "char")], 
                    collapse = "")
                  m <- paste(m, m1, sep = ",")
                }
                if (i == 1) {
                  markers.string <- m
                }
                else {
                  markers.string <- paste(markers.string, m, 
                    sep = "")
                }
                if (i < nrow(markers)) 
                  markers.string <- paste(markers.string, "|", 
                    sep = "")
            }
        }
        if (verbose) 
            print(markers.string)
        url <- paste(url, "&markers=", markers.string, sep = "")
    }
    if (verbose) 
        print(url)
    if (verbose == -1) 
        browser()
    if (verbose < 2) 
        download.file(url, destfile, mode = "wb", quiet = TRUE)
    if (RETURNIMAGE) {
        myMap <- ReadMapTile(destfile)
        if (GRAYSCALE) 
            myMap$myTile <- RGB2GRAY(myMap$myTile)
        return(myMap)
    }
    invisible(url)
}

GetMap2.bbox <- function (lonR, latR, center, size = c(640, 640), destfile = "MyTile.png", 
    MINIMUMSIZE = FALSE, RETURNIMAGE = TRUE, GRAYSCALE = FALSE, 
    NEWMAP = TRUE, zoom, verbose = 1, ...){
    if (missing(zoom)) 
        zoom <- min(MaxZoom(latR, lonR, size))
    if (missing(center)) {
        lat.center <- mean(latR)
        lon.center <- mean(lonR)
    }
    else {
        lat.center <- center[1]
        lon.center <- center[2]
    }
    if (MINIMUMSIZE) {
        ll <- LatLon2XY(latR[1], lonR[1], zoom)
        ur <- LatLon2XY(latR[2], lonR[2], zoom)
        cr <- LatLon2XY(lat.center, lon.center, zoom)
        ll.Rcoords <- Tile2R(ll, cr)
        ur.Rcoords <- Tile2R(ur, cr)
        if (verbose > 1) {
            cat("ll:")
            print(ll)
            print(ll.Rcoords)
            cat("ur:")
            print(ur)
            print(ur.Rcoords)
            cat("cr:")
            print(cr)
        }
        size[1] <- 2 * max(c(ceiling(abs(ll.Rcoords$X)), ceiling(abs(ur.Rcoords$X)))) + 
            1
        size[2] <- 2 * max(c(ceiling(abs(ll.Rcoords$Y)), ceiling(abs(ur.Rcoords$Y)))) + 
            1
        if (verbose) 
            cat("new size: ", size, "\n")
    }
    return(GetMap2(center = c(lat.center, lon.center), zoom = zoom, 
        size = size, destfile = destfile, RETURNIMAGE = RETURNIMAGE, 
        verbose = verbose, ...))
	}