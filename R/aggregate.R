##Copyright R. Gentleman, 2001
##All rights reserved

##a simple aggregator
##data are aggregated in the environment env
##if they are not there then the get assigned with
##initfun, if they are there they get aggregated with
##agfun

aggregate <- function(x, env, agfun, initfun)
 {
   if( !is.environment(env) )
     stop("second argument must be an environment")
   if( !is.function(agfun) || !is.function(initfun) )
     stop("agfun and initfun must be functions")

   if(is.character(x)) {
     for( i in 1:length(x) ) {
       nm <- x[i]
       if( !exists(nm, env=env) )
          assign(nm, env=env, initfun(nm, x))
       else {
          v1 <- get(nm, env=env)
          assign(nm, agfun(nm, v1), env=env)
       }
     }
   }
   else if(is.list(x)) {
     nms <- names(x)
     for( i in 1:length(x) ) {
        nm <- nms[i]
        if( !exists(nm, env=env) )
          assign(nm, env=env, initfun(nm, x[[i]]))
        else {
           v1 <- get(nm, env=env)
           assign(nm, env=env, agfun(nm, v1, x[[i]]))
        }
     }
   }
   else stop("bad type for aggregate")
 }
