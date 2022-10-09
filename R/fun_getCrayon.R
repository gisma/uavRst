#' colorize the cat outputs
#'@description colorize the cat outputs
#'@name getCrayon
#'@export getCrayon
#'@keywords internal
getCrayon<-function(){
  head <- crayon::black $ bgGreen
  err  <- crayon::red $ bold
  note <- crayon::blue $ bold
  ok   <- crayon::green $ bold
  return(list(note,err,ok,head))
}