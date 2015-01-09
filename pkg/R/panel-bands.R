panel.bands <- function(x,y,groups=NULL,subscripts=NULL,
                            col=superpose.line$col,
                            border=NA,
                            lty = superpose.line$lty,
                            alpha=.2,
                        ...){
  
  superpose.line <- trellis.par.get("superpose.line")
  
  if(!length(groups))
    panel.bands1(x,y,
        col=col,
        border=border,
        lty=lty,
        alpha=alpha,
        ...)
  else{
    
    if(length(subscripts))
      groups <- groups[subscripts]
    x <- split(x,groups)
    y <- split.intv(y,groups)

    col <- rep(col,len=length(x))
    border <- rep(border,len=length(x))
    lty <- rep(lty,len=length(x))
    
    for(g in seq_along(x)){
      panel.bands1(x[[g]],y[[g]],
        col=col[g],
        border=border[g],
        lty=lty[g],
        alpha=alpha,
        ...)
    }
  }
}

panel.bands1 <- function(x,y,col,
          border,
          lty,
          alpha,
          ...){
  
  y1 <- attr(y,"lwr")
  y2 <- attr(y,"upr")

  x0 <- c(x,rev(x))
  y0 <- c(y1,rev(y2))
  panel.polygon(x0,y0,col=col,alpha=alpha,border=NA,...)

  if(!is.na(col)){

    panel.lines(x,y1,col=border,lty=lty)
    panel.lines(x,y2,col=border,lty=lty)
  }
}
