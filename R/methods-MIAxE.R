# ==================================
# MIAxE Class Methods 
# ----------------------------------
setMethod("show",
          signature=signature(object="MIAxE"),
          function(object) {
              cat(class( object ), "\n", sep="")
          })

