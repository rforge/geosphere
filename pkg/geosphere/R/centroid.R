# Author: Robert J. Hijmans
# April 2010
# version 0.1
# license GPL3

# See http://local.wasp.uwa.edu.au/~pbourke/geometry/polyarea/

.basiccentroid <- function(p) {
	p2 = rbind(p[-1,], p[1,])
	P = p[,1] * p2[,2] - p2[,1] * p[,2]
	area6 <- 6 * sum(P) / 2
    lon <- sum((p[,1] + p2[,1]) * P)
    lat <- sum((p[,2] + p2[,2]) * P)
	return(cbind(lon, lat) / area6 )
}

if (!isGeneric("centroid")) {
	setGeneric("centroid", function(x, ...)
		standardGeneric("centroid"))
}	


setMethod("centroid", signature(x='data.frame'), 
function(x) {
	centroid(as.matrix(x))
})



setMethod("centroid", signature(x='matrix'), 
function(x) {

	x <- .pointsToMatrix(x, poly=TRUE)

	dif1 <- max(x[,1]) - min(x[,1])
	rotated <- FALSE
	if (dif1 > 180) {
		x2 <- x
		x2[,1] <- x2[,1]%%(360) - 180
		dif1 <- max(x[,1]) - min(x[,1])
		dif2 <- max(x2[,1]) - min(x2[,1]) 
		if (dif2 < dif1) {
			rotated <- TRUE
			x <- x2 
		}
	}
	
	x <- mercator(x, r=1)
	cenM <- .basiccentroid(x)
	cenG <- mercator(cenM, r=1, inverse=TRUE)
	
	if (rotated) {
		cenG[,1] <- cenG[,1] + 180
		cenG[,1] <- .normalizeLonDeg(cenG[,1])
	}
	
	rownames(cenG) <- NULL
	return(cenG)
}
)



setMethod("centroid", signature(x='SpatialPolygons'), 
function(x) {

	if ( isTRUE(is.projected(x)) ) {
		return( coordinates(x)) 
	}

	x <- x@polygons
	n <- length(x)
	res <- matrix(nrow=n, ncol=2, dimnames=list(NULL, c("lon", "lat")))
	for (i in 1:n) {
		# TODO: Calculate area on an equal area projection
		parea <- sapply(x[[i]]@Polygons, function(y){slot(y, "area")} )
		hole <- sapply(x[[i]]@Polygons, function(y){slot(y, "hole")} )
		parea[hole] <- -1
		selPol<- which(parea > 0)
		pol <- x[[i]]@Polygons[selPol]
		polarea <- parea[selPol]

		cen <- t(sapply(pol, function (p) centroid(p@coords)))
		cen<- as.data.frame(cen) # avoid matrix -> vector when there is only one polygon
		# some Inf value (eg: presence1_origin1_seasonal3/Brachyramphus_marmoratus_3309_BL.shp)
		selOK<- which(apply(cen, 1, function(z) all(is.finite(z))))
		cen <- cen[selOK,]
		polarea<- polarea[selOK]

		dif1 <- max(cen[,1]) - min(cen[,1])
		rotated <- FALSE
		if (dif1 > 180) {
		  x2 <- cen
		  x2[,1] <- (x2[,1] %% 360) - 180
		  dif1 <- max(cen[,1]) - min(cen[,1])
		  dif2 <- max(x2[,1]) - min(x2[,1])
		  if (dif2 < dif1) {
		    rotated <- TRUE
		    cen <- x2
		  }
		}

		cen <- cbind(weighted.mean(cen[,1], polarea), weighted.mean(cen[,2], polarea))

		if (rotated) {
		  cen[,1] <- cen[,1] + 180
		  cen[,1] <- .normalizeLonDeg(cen[,1])
		}

# 		j <- which.max(parea) # Select the polygon with larger area
# 		crd <- x[[i]]@Polygons[[j]]@coords
# 		res[i,] <- centroid(crd)
		res[i,] <- cen
	}
	return(res)
} )

