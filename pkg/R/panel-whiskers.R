panel.vwsk <- function(x,y,groups=NULL,
                       subscripts=NULL,
                       col = superpose.symbol$col,
                       lty = superpose.line$lty,
                       width=0,
                            ...){
  
  superpose.symbol <- trellis.par.get("superpose.symbol")
  superpose.line <- trellis.par.get("superpose.line")
  
  if(!length(groups))
    panel.whiskers1(x,y,,
                    col=col[1],
                    lty=lty[1],
                    width=width,...)
  else{
    
    if(length(subscripts))
      groups <- groups[subscripts]
    x <- split(x,groups)
    y <- split.intv(y,groups)

    col <- rep(col,len=length(x))
    lty <- rep(lty,len=length(x))
    
    for(g in seq_along(x)){
      panel.vwsk1(x[[g]],y[[g]],
                  col=col[g],
                  lty=lty[g],
                  width=width,...)
    }
  }
}

panel.vwsk1 <- function(x,y,
                        col,lty,
                        width,...){

  if(!length(x)) return(NULL)
  
  xunit <- diff(range(x))/length(unique(x))
  width <- width*xunit
  
  y0 <- attr(y,"lwr")
  y1 <- attr(y,"upr")

  panel.segments(x0=x,x1=x,y0=y0,y1=y1,col=col,lty=lty,...)

  offs <- width/2
  panel.segments(x0=x-offs,x1=x+offs,y0=y0,y1=y0,col=col,lty=lty,...)
  panel.segments(x0=x-offs,x1=x+offs,y0=y1,y1=y1,col=col,lty=lty,...)

}



panel.hwsk <- function(x,y,groups=NULL,
                       subscripts=NULL,
                       col = superpose.symbol$col,
                       lty = superpose.line$lty,
                       lwd = superpose.line$lwd,
                       box.ratio = 1, box.width = box.ratio/(1 + box.ratio),
                       width=0,
                       ...){
  
  superpose.symbol <- trellis.par.get("superpose.symbol")
  superpose.line <- trellis.par.get("superpose.line")
  
  if(!missing(box.width) || !missing(box.ratio)){
    superpose.polygon <- trellis.par.get("superpose.polygon")
    if(missing(col)) col <- superpose.polygon$border
    if(missing(lty)) lty <- superpose.polygon$lty
    if(missing(lwd)) lwd <- superpose.polygon$lwd
  }
  
  y <- as.numeric(y)
  
  if(!length(groups))
    panel.hwsk1(x,y,
                col=col[1],
                lty=lty[1],
                width=width,...)
  else{
    
    if(length(subscripts))
      groups <- groups[subscripts]
    if (!is.factor(groups)) 
      groups <- factor(groups)
    ngrps <- nlevels(groups)
    
    x <- split.intv(x,groups)
    y <- split(y,groups)
    
    col <- rep(col,len=length(y))
    lty <- rep(lty,len=length(y))
    ltw <- rep(lty,len=length(y))
    
    for(g in seq_along(x)){
      y.g <- y[[g]]
      
      if(!missing(box.width) || !missing(box.ratio)){
        height <- box.width/ngrps
        y.g <- y.g + height*(g - (ngrps + 1)/2)
      }
      
      panel.hwsk1(x[[g]],y.g,
                  col=col[g],
                  lty=lty[g],
                  lwd=lwd[g],
                  width=width,...)
    }
  }
}

panel.hwsk1 <- function(x,y,
                        col,lty,
                        lwd,
                        width,...){
  
  if(!length(x)) return(NULL)
  
  yunit <- diff(range(y))/length(unique(y))
  width <- width*yunit
  
  x0 <- attr(x,"lwr")
  x1 <- attr(x,"upr")
  
  panel.segments(x0=x0,x1=x1,y0=y,y1=y,col=col,lty=lty,lwd=lwd,...)
  
  offs <- width/2
  panel.segments(x0=x0,x1=x0,y0=y-offs,y1=y+offs,col=col,lty=lty,lwd=lwd,...)
  panel.segments(x0=x1,x1=x1,y0=y-offs,y1=y+offs,col=col,lty=lty,lwd=lwd,...)
  
}

