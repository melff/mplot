prepanel.areaplot <- function(x,y,groups,subscripts,stack=TRUE,...){
  if(stack){
    grp <- groups[subscripts]
    
    ux <- sort(unique(x))
    u.grp <- sort(unique(grp))
    m <- length(ux)
    n <- length(u.grp)
    Y <- matrix(0,nrow=m,ncol=n)
    
    for(j in 1:n){
      g <- u.grp[j]
      x.g <- x[grp==g]
      y.g <- y[grp==g]
      i <- match(x.g,ux)
      Y[i,j] <- y.g
    }
    Y <- apply(Y,1,cumsum)
    ylim <- range(c(0,Y))
  }
  else
    ylim <- range(y)
  
  list(
    xlim=range(x),
    ylim=ylim,
    dx=1,
    dy=1
  )
}

panel.areaplot <- function(x,y,groups,subscripts,stack=TRUE,...){
  grp <- groups[subscripts]
  
  ux <- sort(unique(x))
  u.grp <- sort(unique(grp))
  m <- length(ux)
  n <- length(u.grp)
  Y0 <- Y1 <- matrix(0,nrow=m,ncol=n)
  
  for(j in 1:n){
    g <- u.grp[j]
    x.g <- x[grp==g]
    y.g <- y[grp==g]
    i <- match(x.g,ux)
    Y1[i,j] <- y.g
  }
  if(stack){
    Y1 <- t(apply(Y1,1,cumsum))
    Y0[,-1] <- Y1[,-n,drop=FALSE]
  }
  xx <- c(ux,rev(ux))
  Y1 <- Y1[m:1,,drop=FALSE]
  
  superpose.polygon <- trellis.par.get("superpose.polygon")
  supp.names <- names(superpose.polygon)
  dots <- list(...)
  argn <- intersect(names(dots),supp.names)
  superpose.polygon[argn] <- dots[argn]
  
  for(sn in supp.names){
    tmp <- superpose.polygon[[sn]]
    length(tmp) <- n
    suppressWarnings(tmp[] <- superpose.polygon[[sn]])
    superpose.polygon[[sn]] <- tmp
  }
  
  for(j in 1:n){
    yy <- c(Y0[,j],Y1[,j])
    lpolygon(xx,yy,
      border=superpose.polygon$border[j],
      col=superpose.polygon$col[j]
      )
  }
}

areaplot <- function (x, data = NULL, 
     ...)
{
    ocall <- sys.call(sys.parent())
    ocall[[1]] <- quote(areaplot)
    ccall <- match.call()
    ccall$data <- data
    ccall$prepanel <- prepanel.areaplot
    ccall$panel <- panel.areaplot
    ccall[[1]] <- quote(lattice::xyplot)
    ans <- eval.parent(ccall)
    ans$call <- ocall
    ans
}
