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
                    col=col,
                    lty=lty,
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
