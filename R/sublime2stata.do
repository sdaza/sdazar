  z <- apply(dat[, vars, with = FALSE], 1, function(x) sum(!is.na(x)) / length(x)) 
