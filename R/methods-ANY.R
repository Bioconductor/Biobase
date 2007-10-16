setMethod("combine",
          signature=signature(
            x="ANY", y="missing"),
          function(x, y, ...) x)
