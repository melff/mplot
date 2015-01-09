
intv <- function(lwr,upr){
  
  stopifnot(length(upr)==length(lwr))
  
  structure(upr-lwr,
            upr=upr,
            lwr=lwr,
            class="intv")
}

"[.intv" <- function(x,i,...){
  structure(as.numeric(x)[i],
            upr=attr(x,"upr")[i],
            lwr=attr(x,"lwr")[i],
            class="intv")
}
  
split.intv <- function(x,f,drop=FALSE,...){

  upr <- split.default(attr(x,"upr"),f,drop,...)
  lwr <- split.default(attr(x,"lwr"),f,drop,...)
  x <- split.default(as.numeric(x),f,drop,...)
  
  mapply(structure,x,upr=upr,lwr=lwr,MoreArgs=list(class="intv"), SIMPLIFY = FALSE)
}