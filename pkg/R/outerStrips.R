withOuterStrips <- function(x,...){

  x <- update(x,...)
  x <- update(x,
              ylab.right=x$ylab,
              ylab=NULL,
              scales=list(
                x=list(alternating=1,tck=c(1,0)),
                y=list(alternating=2,tck=c(0,1))
              ))
  useOuterStrips(x)
}