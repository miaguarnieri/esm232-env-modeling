#' Lot. Voltera Model
#'
#' function computes the rate of change of populations in a predictor prey interaction
#' @param t  time (days)
#' @param pop datatype list initial conditions; list with two values prey=number of prey and pred=number of predictor
#' @param pars datatype list  coefficient in Lotka-Voltera pars$rprey, pars$alpha, pars$eff, par$pmort
#'  \emph{rprey} is growth rate of prey population;
#'  \emph{eff} is the rate of ingestion of prey by predators
#'  \emph{alpha} is a interaction coefficient (higher values greater interaction
#’  \emph{pmort}  mortality rate of predictor population
#' @examples
#' lotvod(t=1, pop=list(1,2), pop=list(0.5,0.3,0.2,0.2))
#'
#' pars = c(rprey=0.5, alpha=0.3, eff=0.2, pmort=0.2)
#' currpop  = c(prey = 1, pred=1)
#  days = seq(from=1,to=20)
#' res = ode(func=lotvmod, y=currpop, times=days, parms=pars)
#'
#' @return  lotvmod returns a list containing the following components
#' \describe{
#' \item{dprey}{rate of change of prey populutation}
#' \item{dpred}{rate of change of preditor populutation}
#'}

lotvmodK_try2 = function(t, pop, pars, thresh) {
  
  dprey <- ifelse(pop$prey > thresh, 
                  (pars$rprey*(1-pop$prey/pars$K)*pop$prey -  pars$alpha*pop$prey*pars$pred) - pars$harvest, 
                  pars$rprey*(1-pop$prey/pars$K)*pop$prey -  pars$alpha*pop$prey*pars$pred)
  
  dpred = pars$eff*pars$alpha*pop$prey*pop$pred - pars$pmort*pop$pred
  
  return(list(c(dprey,dpred)))}




