# LS Factor Calculator
# Creation Date: November 22, 2021
# Last Updated Date: Mar 03, 2022
# Created by: Mitch Krafczek
# Calculates the slope length calculation
# NOTE: This is a legacy file that will be updated in the future to match the package.

lsFactorFunction <- function(DEM,counter){

  # DEM <- reclassify(DEM, cbind(NaN, NA, 0), right = FALSE)

  units <- "Meter"

  less5 <- 0.5*100
  great5 <- 0.5*100
  threshold <- 0.5

  # Define constants
  rasterlist <- array()
  rasterName <- array()
  baseRaster <- raster(DEM)
  values(baseRaster) <- NA
  nRows <- nrow(DEM)
  nCols <- ncol(DEM)
  cTotal <- nCols*nRows
  cSize <- res(DEM)
  cSize <- cSize[1]
  cSize[cSize < 1] <- 250
  fDirc <- c(1,2,4,8,16,32,64,128)
  rookDirc <- c(1,4,16,64)
  diagDirc <- c(2,8,32,128)
  fdCol = c(1, 1, 0, -1, -1, -1, 0, 1)
  fdRow = c(0, 1, 1, 1, 0, -1, -1, -1)

  # Calculate the slope
  slope <- terrain(DEM, opt = "slope", neighbors = 8, unit="degrees")
  # print(slope)

  # Calculate the flow direction
  flowdir <- terrain(DEM, opt = "flowdir")
  flowdir <- array(flowdir,dim=c(nRows,nCols))

  writeTempData(slope,paste0("temp_",counter),paste0('slope_',counter),'raster')

  cellsList <- array(c(c("tCell","rCell","bCell","lCell","trCell","tlCell","brCell","blCell"),
                       c("tFD","rFD","bFD","lFD","trFD","tlFD","brFD","blFD"),
                       c(-1,0,1,0,-1,-1,1,1),
                       c(0,1,0,-1,1,-1,1,-1),
                       c("tFDerror","rFDerror","bFDerror","lFDerror",
                         "trFDerror","tlFDerror","brFDerror","blFDerror"),
                       c(4,8,16,32,64,128,1,2),
                       c("tCDHSL","rCDHSL","bCDHSL","lCDHSL","trCDHSL","tlCDHSL","brCDHSL","blCDHSL"),
                       c("tSlope","rSlope","bSlope","lSlope","trSlope","tlSlope","brSlope","blSlope")),
                     dim = c(8,8))

  # Step: Define Functions

  # Buffer edges cells to nodata
  noDataBuff <- function(noArray,topLeft){
    noArray[1:nrow(noArray),topLeft] <- -999
    noArray[topLeft,1:ncol(noArray)] <- -999
    noArray[1:nrow(noArray),ncol(noArray)] <- -999
    noArray[nrow(noArray),1:ncol(noArray)] <- -999
  }

  # Get cell number
  nbrCell <- function(originRow, originCol, nbrRow, nbrCol){
    varR <- originRow + nbrRow
    varC <- originCol + nbrCol
    if(varR < 0 || varC < 0 || varR == nRows || varC == nCols || varR > nRows || varC > nCols)
    {return(c("Invalid","Invalid"))}
    return(c(varR,varC))
  }

  # Radiant and degree calculation without need for additional toolboxes.
  rad2deg <- function(rad){(rad * 180) / (pi)}
  deg2rad <- function(deg){(deg * pi) / (180)}


  # Calc S Factor

  sCalc <- function(slope, percent){
    if(percent < 9){
      return(10.8 * (sin(deg2rad(slope))) + 0.03)
    } else if ( percent >= 9){
      return(16.8 * (sin(deg2rad(slope))) - 0.50)
    }
  }

  # Calc L Factor
  lCalc <- function(slopeVar, lengthVar){
    beta <- ((sin(deg2rad(slopeVar))) / 0.0896) / (3*((sqrt(sin(deg2rad(slopeVar))^2))**0.8)+0.56)
    m <- (beta/(1+beta))
    if(units == "Meter"){
      return((lengthVar/22.13)**m)
    } else if(units == "Feet"){
      return((lengthVar/72.6)**m)
    }
  }

  # Calculate slope percent used in "Step: Calc S factor" and "Step: Calc CDHSL"
  slopePerc <- function(value){
    return(tan(deg2rad(value))*100)
  }

  # Step: Calc max downhill slope angle

  values(baseRaster) <- array(flowdir, dim = c(nRows, nCols))
  fDircArray <- baseRaster
  elevArray <- DEM
  maxDHSArray <- as.data.frame(matrix(rep(NA), nrow = nRows, ncol = nCols))
  pb <- txtProgressBar(title = "Progress bar",min = 0, max = nRows, style = 3)

  errorFunction <- function(x){
    if(is.na(x) || is.null(x) || x == -999 || x == 0){
      return(0)
    } else(return(1))
  }

  errorFilter <- function(x,y){
    if(x == 1){
      return(y)
    }else{return(0)}
  }

  newElevPixelFunction <- function(x,y){
    if(is.null(x) || is.null(y) || is.na(x) || is.na(y)){
      return(NA)
    }
    else if(lengths(x) == 0 || lengths(y) == 0){
      return(NA)
    }
    else if(x <= nRows && y <= nCols){
        return(elevArraytemp[x,y])
      }
    else{
      # Else clause is if it is only 1 longer than the x or y coordinates, not a catch all
      return(elevArraytemp[x-1,y-1])
    }
  }

  slopeCalcFunction <- function(x,y){
    if(x == 1){
      return(rad2deg(atan(y/(1.4142*cSize))))
    } else if(x == 2){
      return(rad2deg(atan(y/(cSize))))
    } else{return(-999)}}

  # maxDHSArrayFunction <- function(nRow){
  for(nRow in 1:nRows){
    setTxtProgressBar(pb,nRow)
    if(nRow != 1 && nRow != nRows){

      elevPixel <- elevArray[nRow,1:ncol(elevArray)]
      fDircPixel <- fDircArray[nRow, 1:ncol(fDircArray)]



      error <- mapply(function(x,y) errorFunction(x) + errorFunction(y), elevPixel, fDircPixel)
      error[error == 1] <- 0
      error[error == 2] <- 1

      fDircPixel <- mapply(errorFilter,error,fDircPixel)
      elevPixel <- mapply(errorFilter,error,elevPixel)
      # Get pixel index
      # i <- mapply(function(x,y) if(x == 0){return(0)}else{match(y,fDirc)},elevPixel,fDircPixel)
      i <- sapply(fDircPixel, function(x) match(x,fDirc))
      # Get location of the comparing cell (the cell in the flow direction)
      temp <- fdRow[i]
      newRow <- mapply(function(x,y) x + y,nRow, temp)
      newRow[newRow == nRows] <- NA
      temp <- fdCol[i]
      newCol <- mapply(function(x,y) x + y,sequence(nCols),temp)
      newCol[newCol == nCols] <- NA
      # now the elevation of that comparing cell can be referenced
      if(nRow != 0 || nRow != nRows){
        elevArraytemp <- array(elevArray[(nRow-1):(nRow+1),],dim=c(3,nCols))}

      newRow<- newRow-nRow+2
      newElevPixel <- mapply(newElevPixelFunction,newRow,newCol)
      # Calculate the difference
      elevDiff <- mapply(function(x,y) x - y, elevPixel, newElevPixel)

      initdia <- as.integer(fDircPixel %in% diagDirc)
      initroo <- as.integer(fDircPixel %in% rookDirc)

      init <- mapply(function(x,y) if(x == 1) {return(1)} else if(y == 1){return(2)} else {return(0)},initdia,initroo)

      init <- mapply(slopeCalcFunction, init, elevDiff)
      # init <- unlist(init)
      init[lengths(init) == 0] <- NA
      # init <- matrix(init,nrow=1)
      maxDHSArray[nRow,] <- init

    } else {
      temp <- rep(-999,nCols)
      maxDHSArray[nRow,] <- temp
    }
  }

  #Value = 0 change to 0.1. this allows for erosion in every cell without altering flow paths (following the GC method)
  maxDHSArray[maxDHSArray == 0] <- 0.1
  # Add to list of rasters to save
  # values(baseRaster) <- maxDHSArray
  # rasterlist <- append(rasterlist,baseRaster)
  # rasterName <- append(rasterName,"maxDHSArray")
  print(paste0("Done the maxDHSArray!"))


  # Step: Calc S Factor # following RUSLE guidelines
  sFactorArray <- array(c(-999), dim = c(nRows, nCols))
  # sFactorArrayFunction <- function(nRow){
  for(nRow in 1:nRows){
    setTxtProgressBar(pb,nRow)
    slopetemp <- maxDHSArray[nRow,1:ncol(maxDHSArray)]
    slopeP <- sapply(maxDHSArray[nRow,1:ncol(maxDHSArray)], function(x) if(x != -999 && !is.na(x)){
      slopePerc(x)
    } else(return(x)))

    error <- as.integer(slopetemp != -999)
    sFactorArray[nRow,] <- mapply(function(x,y,z) if(z == 1 && !is.na(z)){
      sCalc(x,y)
    } else { return(-999)},slopetemp,slopeP,error)

  }

  ## Add to raster list
  # values(baseRaster) <- sFactorArray
  # rasterlist <- append(rasterlist,baseRaster)
  # rasterName <- append(rasterName,"sFactor")
  sFactorArray <- NULL
  print(paste0("Done the sFactor!"))

  # Step: Calc L Factor Components (GC method) #
  # Step: Calc NCSL #
  # NCSL for each cell calculated by flow direction and designation as a high point or flat area
  NCSLArray <- array(c(-999), dim = c(nRows, nCols))
  temp <- NULL

  cellFunction <- function(w,x,y,z){
    temp <- mapply(function(i) nbrCell(as.numeric(x),i, as.numeric(y), as.numeric(z)), sequence(nCols))
    temp[temp[1:2,] == c("Invalid","Invalid")] <- as.numeric(-999)
    temp <- mapply(function(i,j) flowdir[i,j], as.numeric(temp[1,]),as.numeric(temp[2,]))
    return(temp)
  }

  ## Diagonal  == 1.
  ## Rook == 2.

  cardinalFunction <- function(x){
    if(x == 1){
      return(1.4142 * cSize)
    } else if(x == 2){
      return(cSize)
    } else if(x == 3){
      return(0.5 * 1.4142 * cSize)
    } else if(x == 4){
      return(0.5 * cSize)
    } else{return(-999)}}

  # NCSLArrayFunction <- function(nRow){
  for(nRow in 1:nRows){
    setTxtProgressBar(pb,nRow)
    if(nRow != 1 && nRow != nRows){
      elevPixel <- elevArray[nRow,1:ncol(elevArray)]
      fDircPixel <- fDircArray[nRow, 1:ncol(fDircArray)]



      error <- mapply(function(x,y) errorFunction(x) + errorFunction(y), elevPixel, fDircPixel)
      error[error == 1] <- 0
      error[error == 2] <- 1

      fDircPixel <- mapply(errorFilter,error,fDircPixel)

      # set row and column locations for all cell neighbours around current cell.
      # This helps determine those cells that have no inflow from their surrounding
      # neighbours, according to the D8 algorithm

      for(k in 1:nrow(cellsList)){
        temp <- mapply(cellFunction,cellsList[k,1],nRow,cellsList[k,3],
                       cellsList[k,4])
        assign(cellsList[k,2],temp)}

      # apply NCSL rules to write NSCLArray values
      # if cell high point (no flow into it) then multiply by 0.5 to account for
      # only length downhill from centre
      fDircPixel[sapply(fDircPixel,is.na)] <- -999

      for(k in 1:nrow(cellsList)){
        # print(cellsList[k,2])
        templist <- array(get(as.name(cellsList[k,2])),dim=c(1,nCols))
        # templist <- t(templist)
        temp <- mapply(function(x,y) as.integer(x == y),templist, as.numeric(cellsList[k,6]))
        assign(cellsList[k,5],temp)}

      initdia <- as.integer(fDircPixel %in% diagDirc)
      initroo <- as.integer(fDircPixel %in% rookDirc)
      initHighPoint <- mapply(function(s,t,u,v,w,x,y,z) if(length(s) == 0||
                                                           length(t) == 0||
                                                           length(u) == 0||
                                                           length(v) == 0||
                                                           length(w) == 0||
                                                           length(x) == 0||
                                                           length(y) == 0||
                                                           length(z) == 0||
                                                           is.na(s)||
                                                           is.na(t)||
                                                           is.na(u)||
                                                           is.na(v)||
                                                           is.na(w)||
                                                           is.na(x)||
                                                           is.na(y)||
                                                           is.na(z)){
                                              return(-999)
                            } else if(s != 1 &&
                                      t != 1 &&
                                      u != 1 &&
                                      v != 1 &&
                                      w != 1 &&
                                      x != 1 &&
                                      y != 1 &&
                                      z != 1){return(1)}else{return(0)}
                              ,tFDerror,rFDerror,bFDerror,lFDerror,trFDerror,tlFDerror,brFDerror,blFDerror)

      initHighPoint <- mapply(function(x,y,z)
        if(x == 1){
          if(y == 1){
            return(1)
          } else if(z == 1){
            return(2)
          } else (return(0))
        } else (return(0))
        , initHighPoint,initdia,initroo)

      init <- mapply(function(x,y,z) if(z == 1){return(3)}else if(z == 2){return(4)} else if(x == 1) {return(1)} else if(y == 1){return(2)}else {return(0)},initdia,initroo,initHighPoint)

      NCSLcardinal <- sapply(init,cardinalFunction)

      NCSLArray[nRow,] <- NCSLcardinal
    } else {
      temp <- rep(-999,nCols)
      NCSLArray[nRow,] <- temp
    }}

  # NCSLArray <- sapply(sequence(nRows), NCSLArrayFunction)
  # NCSLArray <- t(NCSLArray)
  # Add to raster list for saving later
  # values(baseRaster) <- NCSLArray
  # rasterlist <- append(rasterlist,baseRaster)
  # rasterName <- append(rasterName,"NCSLArray")
  print(paste0("Done the NCSLArray!"))

  ElevFunction <- function(x,y,z){
    if(!is.na(y) && !is.na(z) && length(y) != 0 && length(z) != 0 && y == z){
      if(x == 1){
        return(1.4142 * cSize * 0.5)
      } else if(x == 2){
        return(cSize * 0.5)
      }} else (return(0))}

  # NCSLArrayElevFunction <- function(nRow){
  for(nRow in 1:nRows){

    setTxtProgressBar(pb,nRow)
    if(nRow != 1 && nRow != nRows){


      elevPixel <- elevArray[nRow,1:ncol(elevArray)]
      fDircPixel <- fDircArray[nRow, 1:ncol(fDircArray)]



      error <- mapply(function(x,y) errorFunction(x) + errorFunction(y), elevPixel, fDircPixel)
      error[error == 1] <- 0
      error[error == 2] <- 1

      fDircPixel <- mapply(errorFilter,error,fDircPixel)
      elevPixel <- mapply(errorFilter,error,elevPixel)

      # Get pixel index
      i <- sapply(fDircPixel, function(x) match(x,fDirc))
      # Get location of the comparing cell (the cell in the flow direction)
      temp <- fdRow[i]
      newRow <- mapply(function(x,y) x + y,nRow, temp)
      newRow[newRow == nRows] <- -999
      temp <- fdCol[i]
      newCol <- mapply(function(x,y,z) x + y,sequence(nCols),temp)
      newCol[newCol == nCols] <- -999
      # now the elevation of that comparing cell can be referenced
      if(nRow != 0 || nRow != nRows){
        elevArraytemp <- array(elevArray[(nRow-1):(nRow+1),],dim=c(3,nCols))
        NCSLArraytemp <- array(NCSLArray[nRow,],dim=c(1,nCols))}

      newRow<- newRow-nRow+2
      newElevPixel <- mapply(newElevPixelFunction,newRow,newCol)

      initdia <- as.integer(fDircPixel %in% diagDirc)
      initroo <- as.integer(fDircPixel %in% rookDirc)

      init <- mapply(function(x,y) if(x == 1) {return(1)} else if(y == 1){return(2)} else {return(0)},initdia,initroo)

      init <- mapply(ElevFunction, init, elevPixel,newElevPixel)

      init <- mapply(function(x,y) if(x == 0){
        return(NCSLArraytemp[,y])
      } else {return(x)}, init,sequence(nCols))

      # init[lengths(init) == 0] <- NA
      # init <- matrix(init,nrow=1)
      NCSLArray[nRow,] <- init

    } else {
      temp <- rep(-999,nCols)
      NCSLArray[nRow,] <- temp
    }
  }

  # NCSLArray <- sapply(sequence(nRows), NCSLArrayElevFunction)
  # NCSLArray <- t(NCSLArray)
  # Add to raster list for saving later
  # values(baseRaster) <- NCSLArray
  # rasterlist <- append(rasterlist,baseRaster)
  # rasterName <- append(rasterName,"NCSLArray2")
  print(paste0("Done the NCSLArray2!"))

  ## Step: Calc cumulative downhill slope length (CDHSL)

  compareFunction <- function(x,y){
    if(x == (0.5 * cSize) || x == (0.5 * cSize * 1.4142)){
      return(NCSLArray[nRow,y])
    } else {return(0)}
  }

  CDHSLArray <- array(c(0), dim = c(nRows, nCols))
  # CDHSLArrayFunction <- function(nRow){
  for(nRow in 1:nRows){
    setTxtProgressBar(pb,nRow)
    NCSLPixel <- NCSLArray[nRow,1:ncol(NCSLArray)]

    CDHSLArraytemp <- mapply(compareFunction, NCSLPixel,sequence(nCols))
    CDHSLArray[nRow,] <- CDHSLArraytemp

  }

  # CDHSLArray <- sapply(sequence(nRows), CDHSLArrayFunction)
  # CDHSLArray <- t(CDHSLArray)
  # values(baseRaster) <- (CDHSLArray)
  # rasterlist <- append(rasterlist,baseRaster)
  # rasterName <- append(rasterName,"CDHSLArray")
  print(paste0("Done the CDHSLArray!"))


  ## Step: Calc cumulative downhill slope length (CDHSL)

  neighboursFunction <- function(w,x,y){
    if(is.na(w) || is.null(w) ||
       is.na(x) || is.null(x) ||
       is.na(y) || is.null(y)){
      return(NA)
    } else if(w >= 5){
      if(((y - w) / (y - w) * 100) >= (great5 * 100)){
        return(NA)
      }
      else{return(x)}
    } else if (w < 5){
      if(((y - w) / (y - w) * 100) >= (less5 * 100)){
        return(NA)
      }
      else{return(x)}
    }
  }

  CDHSLArraytemp <- list()

  for(nRow in 1:nRows){
    setTxtProgressBar(pb,nRow)
    if(nRow != 1 && nRow != nRows){
      CDHSLPixel <- CDHSLArray[nRow,1:ncol(CDHSLArray)]
      # must start where a CDHSL value exists, this means first iteration at high points
      elevPixel <- elevArray[nRow,1:ncol(elevArray)]
      fDircPixel <- fDircArray[nRow, 1:ncol(fDircArray)]



      error <- mapply(function(x,y) errorFunction(x) + errorFunction(y), elevPixel, fDircPixel)
      error[error == 1] <- 0
      error[error == 2] <- 1

      fDircPixel <- mapply(errorFilter,error,fDircPixel)
      # Get pixel index
      i <- sapply(fDircPixel, function(x) match(x,fDirc))
      # Get location of the comparing cell (the cell in the flow direction)
      temp <- fdRow[i]
      newRow <- mapply(function(x,y) x + y,nRow, temp)
      newRow[newRow >= nRows] <- -999
      newRow[is.na(newRow)] <- -999
      temp <- fdCol[i]
      newCol <- mapply(function(x,y,z) x + y,sequence(nCols),temp)
      newCol[newCol >= nCols] <- -999
      newCol[is.na(newCol)] <- -999

      NCSLPixel <- NCSLArray[nRow,1:ncol(NCSLArray)]
      newNCSL <- mapply(function(x,y) return(NCSLArray[x,y]),newRow,newCol)
      newCDHSL <- mapply(function(x,y) return(CDHSLArray[x,y]),newRow,newCol)
      newSlope <- mapply(function(x,y) if(is.na(x) || is.na(y)){
        return(-999)
      } else if(x != -999 && y != -999){
        if(is.na(maxDHSArray[x,y]) || maxDHSArray[x,y] == -999 || x < 1 || y < 1){
          return(-999)
        } else (return(tan(deg2rad(maxDHSArray[x,y]))*100))}
      else{return(-999)}, newRow,newCol)

      # set row and column locations for all the neighbours around the receiving
      # cell (cell in the flow direction)
      cellFunction <- function(v,w,x,y,z){
        temp <- mapply(nbrCell,as.numeric(w),as.numeric(x), as.numeric(y), as.numeric(z))
        temp[temp[1:2,] == c("Invalid","Invalid")] <- as.numeric(-999)
        tempflow <- mapply(function(i,j) if(i != -999 && j != -999){return(flowdir[i,j])}, as.numeric(temp[1,]),as.numeric(temp[2,]))

        tempCDHSL <- mapply(function(i,j) if(i != -999 && j != -999){return(CDHSLArray[i,j])}, as.numeric(temp[1,]),as.numeric(temp[2,]))
        tempSlope <- mapply(function(i,j) if(i != -999 && j != -999){
          return(slopePerc(maxDHSArray[i,j]))}, as.numeric(temp[1,]),as.numeric(temp[2,]))
        return(c(tempflow,tempCDHSL,tempSlope))
      }

      for(k in 1:nrow(cellsList)){
        temp <- mapply(cellFunction,cellsList[k,2],newRow,newCol,cellsList[k,3],cellsList[k,4])
        assign(cellsList[k,2],temp[1,])
        assign(cellsList[k,7],temp[2,])
        assign(cellsList[k,8],temp[3,])}

      # if empty receiving cell then continue. Check other neighbours if
      # they also flow to the same receiving cell

      slopeFunction <- function(w,x,y,z){
        if(length(w) == 0 || length(x) == 0 || length(y) == 0 || length(z) == 0){
          return(-999)
        }
        else if(!is.na(x) && !is.null(x) && !is.na(y) && !is.null(y) && !is.na(z) && !is.null(z)){
          if(x == w && y != -999 && z != -999){
            return(c(y,z))
          } else(return(-999))
        } else(return(-999))}

      for(k in 1:nrow(cellsList)){
        temp <- mapply(slopeFunction,as.numeric(cellsList[k,6]),get(as.name(cellsList[k,2])),get(as.name(cellsList[k,7])),get(as.name(cellsList[k,8])))
        assign(cellsList[k,2],temp)
      }

      compareFunction <- function(a,b,c,d,e,f,g,h){
        templist <- list()
        if(is.null(a[1]) && is.null(a[2]) &&
           is.null(b[1]) && is.null(b[2]) &&
           is.null(c[1]) && is.null(c[2]) &&
           is.null(d[1]) && is.null(d[2]) &&
           is.null(e[1]) && is.null(e[2]) &&
           is.null(f[1]) && is.null(f[2]) &&
           is.null(g[1]) && is.null(g[2]) &&
           is.null(h[1]) && is.null(h[2])){
          return(NULL)
        }
        if(!is.null(a[1]) && !is.na(a[2]) && a[1] != -999){
          templist <- c(templist,"tFD")
        }
        if(!is.null(b[1]) && !is.na(b[2]) && b[1] != -999){
          templist <- c(templist,"rFD")
        }
        if(!is.null(c[1]) && !is.na(c[2]) && c[1] != -999){
          templist <- c(templist,"bFD")
        }
        if(!is.null(d[1]) && !is.na(d[2]) && d[1] != -999){
          templist <- c(templist,"lFD")
        }
        if(!is.null(e[1]) && !is.na(e[2]) && e[1] != -999){
          templist <- c(templist,"trFD")
        }
        if(!is.null(f[1]) && !is.na(f[2]) && f[1] != -999){
          templist <- c(templist,"tlFD")
        }
        if(!is.null(g[1]) && !is.na(g[2]) && g[1] != -999){
          templist <- c(templist,"brFD")
        }
        if(!is.null(h[1]) && !is.na(h[2]) && h[1] != -999){
          templist <- c(templist,"blFD")
        } else(templist <- c(templist,NULL))
        return(templist)}


      temppFunction <- function(k){
        x <- unlist(temppNeighbours[k])
        templist <- list()
        if(is.null(x)){
          return(-999)
        } else {
          for(j in 1:length(x)){
            if(x[j] == "tFD"){
              templist <- c(templist,"tFD",get(as.name(cellsList[1,7]))[k],get(as.name(cellsList[1,8]))[k])
            } else if(x[j] == "rFD"){
              templist <- c(templist,"rFD",get(as.name(cellsList[2,7]))[k],get(as.name(cellsList[2,8]))[k])
            } else if(x[j] == "bFD"){
              templist <- c(templist,"bFD",get(as.name(cellsList[3,7]))[k],get(as.name(cellsList[3,8]))[k])
            } else if(x[j] == "lFD"){
              templist <- c(templist,"lFD",get(as.name(cellsList[4,7]))[k],get(as.name(cellsList[4,8]))[k])
            } else if(x[j] == "trFD"){
              templist <- c(templist,"trFD",get(as.name(cellsList[5,7]))[k],get(as.name(cellsList[5,8]))[k])
            } else if(x[j] == "tlFD"){
              templist <- c(templist,"tlFD",get(as.name(cellsList[6,7]))[k],get(as.name(cellsList[6,8]))[k])
            } else if(x[j] == "brFD"){
              templist <- c(templist,"brFD",get(as.name(cellsList[7,7]))[k],get(as.name(cellsList[7,8]))[k])
            } else if(x[j] == "blFD"){
              templist <- c(templist,"blFD",get(as.name(cellsList[8,7]))[k],get(as.name(cellsList[8,8]))[k])
            }}
          return(templist)}}

      temppNeighbours <- mapply(compareFunction,
                                get(as.name(cellsList[1,2])),
                                get(as.name(cellsList[2,2])),
                                get(as.name(cellsList[3,2])),
                                get(as.name(cellsList[4,2])),
                                get(as.name(cellsList[5,2])),
                                get(as.name(cellsList[6,2])),
                                get(as.name(cellsList[7,2])),
                                get(as.name(cellsList[8,2])))

      CDHSLArraytemp <- c(CDHSLArraytemp,mapply(temppFunction, sequence(nCols)))
    } else {
      temp <- rep(-999,nCols)
      CDHSLArraytemp <- c(CDHSLArraytemp,temp)
    }

  }

  NCSLArray <- NULL
  CDHSLArraytemp <- t(array(CDHSLArraytemp,c(nCols,nRows)))
  CDHSLArrayDirection <- array(NA,c(nRows,nCols))
  CDHSLArray <- array(c(0), dim = c(nRows, nCols))
  nRow <- 1
  nCol <- 1
  for(i in 1:nRows){
    nRow <- i
    setTxtProgressBar(pb,nRow)
    if(nRow == 1){
      next
    } else if(nRow == nRows){
      next
    }
    for(j in 1:nCols){
      nCol <- j
      if(nCol == 1){
        next
      }
      else if(nCol == nCols){
        next
      } else {
        if(lengths(CDHSLArraytemp[nRow,nCol]) < 3){
          next
        }
        temp <- array(unlist(CDHSLArraytemp[nRow,nCol]),c(3,lengths(CDHSLArraytemp[nRow,nCol])/3))
        ## Create the variables
        x <- temp[1]
        y <- temp[1]
        match <- match(x,cellsList[,2])
        uint <-  as.numeric(cellsList[match,3])
        vint <-  as.numeric(cellsList[match,4])
        u <-  uint
        v <-  vint
        ## Get forward and backwards cells
        int <- 1
        while(x == y){
          if(length(CDHSLArraytemp[nRow+u,nCol+v]) < 3){
            y <- 0
          }
          else if(lengths(CDHSLArraytemp[nRow+u,nCol+v]) >= 3){
            tempcell <- array(unlist(CDHSLArraytemp[nRow+u,nCol+v]),c(3,lengths(CDHSLArraytemp[nRow+u,nCol+v])/3))
          } else {
            y <- 0
          }
          if(y != 0){
            if(as.numeric(max(tempcell[2,])) == (cSize * 0.5) || as.numeric(max(tempcell[2,])) == (cSize * 0.5 * 1.4142)){
              int <- int + 1
              y <- 0}

            else if(tempcell[1] == x){
              int <- int + 1
              u <-  u + uint
              v <-  v + vint

            }else(y <- 0)} else(y <- 0)

        }
        CDHSLArray[nRow,nCol] <- int
        CDHSLArrayDirection[nRow,nCol] <- x
        # }
      }

    }
  }

  CDHSLArray <- CDHSLArray * cSize
  CDHSLArrayDirection <- mapply(function(x) if(is.na(x)){
    return(NA)
  } else if(x == "tFD"){
    return(4)
  } else if(x == "rFD"){
    return(8)
  } else if(x == "bFD"){
    return(16)
  } else if(x == "lFD"){
    return(32)
  } else if(x == "trFD"){
    return(64)
  } else if(x == "tlFD"){
    return(128)
  } else if(x == "brFD"){
    return(1)
  } else if(x == "blFD"){
    return(2)
  }, CDHSLArrayDirection)

  CDHSLArrayDirection <- array(CDHSLArrayDirection,dim = c(nRows,nCols))

  # values(baseRaster) <- (CDHSLArray)
  # rasterlist <- append(rasterlist,baseRaster)
  # rasterName <- append(rasterName,"CDHSLArray2")
  print(paste0("Done the CDHSLArray2!"))

  # Step: L factor calculation following RUSLE guidelines (Agricultural Handbook No. 703)

  lFactorArray <- array(c(0), dim = c(nRows, nCols))

  lFactorFunction <- function(x,y){
    if(x == -999 || y == -999 || is.na(x) || is.na(y)){
      return(-999)
    } else{
      return(lCalc(x,y))
    }
  }

  for(nRow in 1:nRows){
    setTxtProgressBar(pb,nRow)
    slopetemp <- maxDHSArray[nRow,1:ncol(maxDHSArray)]
    length <- CDHSLArray[nRow,1:ncol(CDHSLArray)]

    lFactorArraytemp <- mapply(lFactorFunction, slopetemp,length)
    lFactorArray[nRow,] <- lFactorArraytemp
  }

  lFactorArray[lFactorArray == Inf] <- NA

  values(baseRaster) <- (lFactorArray)
  lFactorArray <- baseRaster
  # print(lFactorArray)
  print(paste0("Done the lFactor!"))
  writeTempData(lFactorArray,paste0("temp_",counter),paste0('lFactor_',counter),'raster')
  listFilesTemp <- list.files(FFP(paste0('/data/temp/temp_',counter,"/")))
  for(tempcounter in 1:length(listFilesTemp)){
    if(str_contains(listFilesTemp[tempcounter],"DEM")){
      file.remove(FFP(paste0('/data/temp/temp_',counter,"/",listFilesTemp[tempcounter])))
    }
  }




  # if(!is_empty(rasterlist)){
  #   for(ii in 1:length(rasterlist)){
  #     # Need to change this so that a raster is passed instead of just the data.
  #     # Use the "baseRaster" as the raster base and just apply the values.
  #     print(rasterName[ii])
  #     print(rasterlist[ii])
  #     writeTempData(rasterlist[ii],paste0("temp_",counter),rasterName[ii],'raster')
  #   }
  # }


}



# Create a temp list of the number of files in the folder
lsFunction <- function(dem,nn){

  dem <- loadRaster(dem)
  print(dem)
  dem[is.na(dem[[1]])] <- 0
  print(dem)
  dem <- raster(dem, layer=1)
  print(dem)

  if(any(getValues(dem) > 0)){
    print(paste0("Starting LS function for temp DEM number ", nn))
    lsFactorFunction(dem,nn)
  } else {
    writeTempData(dem,paste0("/temp_",nn),paste0("lfactor"),"GTiff")
    writeTempData(dem,paste0("/temp_",nn),paste0("slope"),"GTiff")
  }
}
