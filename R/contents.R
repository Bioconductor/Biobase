  if( !isGeneric("contents") && !exists("contents", mode="function") )
       setGeneric("contents", function(object)
                  standardGeneric("contents"))

  setMethod("contents", "environment",
     function(object)
         mget(ls(env=object), env=object, ifnotfound=NA))
