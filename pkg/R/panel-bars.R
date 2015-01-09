panel.vbars <- function(x,y,groups=NULL,subscripts=NULL,
                        col=superpose.symbol$col,
                        border=NA,
                        lty = superpose.line$lty,
                        alpha=.2,...){
  
  superpose.symbol <- trellis.par.get("superpose.symbol")
  superpose.line <- trellis.par.get("superpose.line")
  
  if(!length(groups))
    panel.vbars1(x,y,
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
      panel.vbars1(x[[g]],y[[g]],
                   col=col[g],
                   border=border[g],
                   lty=lty[g],
                   alpha=alpha,
                   ...)
    }
  }
}


panel.vbars1 <- function(x,y,width=1,col,
                         border,
                         lty,
                         alpha,...){

  x <- as.numeric(x)
  if(!length(x)) return(NULL)
  
  xunit <- diff(range(x))/length(unique(x))
  width <- width*xunit

  if(inherits(y,"intv")) {
    ybottom <- attr(y,"lwr")
    ytop <- attr(y,"upr")
  }
  else {
    ytop <- as.numeric(y)
    ybottom <- numeric(length=length(ytop))
  }
  
  xleft <- x - width/2
  xright <- x + width/2
  panel.rect(xleft=xleft,xright=xright,
             ybottom=ybottom,ytop=ytop,
             col=col,
             border=border,
             lty=lty,
             alpha=alpha,...)
}


