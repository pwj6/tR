#' how to make molecule to symbol and fullname
#'
#' @param x is the name or cid of molecule
#' @param type is the type of x
#' @param tSource is the source of target
#' @return symbol and fullname
#' @export
#'
#' @examples
#' .m2t(x='FER',type='molecule',tSource="tcmsp")
#' .m2t(x='445858',type='cid')
#'  m2t(x=c('FER','pyrene'),type='molecule')
#'  m2t(x=c('445858','31423'),type='cid')
m2t<-function(x,type="cid",tSource="tcmsp")
{
  y<-lapply(x,.m2t,type=type,tSource=tSource)
  names(y)<-(data.frame(x))$x
  y
}
#' @export
.m2t<-function(x,type="cid",tSource="tcmsp"){
  {
    type <- match.arg(type,c("cid","molecule"))
    tSource <- match.arg(tSource,c("tcmsp","pubchem"))
    if(length(x)>1)
      stop("Length of x must be 1!")
  }
  {
    if(type=="cid")
    {
      if(tSource=="tcmsp")
        y<-chemtarget[cid==x,][,c(1,20)]
      else
        y <- NA
    }
      else
    {
      if(tSource=="tcmsp")
        y<-chemtarget[molecule==x,][,c(1,20)]
      else
        y <- NA
    }
  }
  {
  if(tSource=="tcmsp")
    y<-y[!duplicated(y$fullname),]
    else
      y <- y
  }
  y[y==""]<-NA
  y
}

