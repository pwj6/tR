#' How to make herb to symbol and fullname
#'
#' @param x is the information adout herb
#' @param type is the type of the x
#' @param output is the type of return
#' @return symbol and fullname
#' @export
#'
#' @examples
#' .h2t(x='Ziziphi Spinosae Semen',type='latin',output='symbol')
#' .h2t(x='huangqi',type='pinyin',output='both')
#' h2t(x=c('Ziziphi Spinosae Semen','Abri Herba'),type='latin',output='symbol')
#' h2t(x=c('Ziziphi Spinosae Semen','Abri Herba'),type='latin',output='both')
h2t<-function(x,type="latin",output="symbol")
{
  y<-lapply(x,.h2t,type=type,output=output)
  names(y)<-(data.frame(x))$x
  y
}
#' @export
.h2t<-function(x,type="latin",output="symbol"){
  {
    type <- match.arg(type,c("latin","pinyin","chinese"))
    output <- match.arg(output,c("symbol","both"))
    if(length(x)>1)
      stop("Length of x must be 1!")
    else if(type=="pinyin")
      x<-drug[pinyin%in%tolower(x),]$latin
    else if(type=="chinese")
      x<-drug[chinese%in%x,]$latin
  }
  {
    if(length(x)==1)
    {
      if(output=="symbol")
        y <- drugtarget[herb==x,][,c(1,21)]
      else
        y <- drugtarget[herb==x,][,c(1,4,21)]
    }
    else
      y<-NA
  }
  y<-y[!duplicated(y$fullname),]
  y[y==""]<-NA
  y
}
