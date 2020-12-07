
chucu <- function(data, margen, funcion = NULL, name.var = funcion, ...){


    if(is.null(funcion)){
        funcion <- c("median", "mean")
    } else {
        funcion <- c("median", "mean", funcion)
    }

    mi_apply <- function(data, margen, funcion, name.var = "V1", ...){
        data <- data[, sapply(data, is.numeric)]
        app <- apply(X = data,
                     MARGIN = margen,
                     FUN = funcion,
                     ...)
        app <- as.data.frame(app)
        names(app) <- name.var
        return(app)
    }
    funciones <- funcion
    resumenes <- list()
    for(i in 1:length(funciones)){
        resumenes[[i]] <- round(mi_apply(data = data,
                                         margen = 2,
                                         funcion = funciones[i],
                                         na.rm = TRUE,
                                         name.var = funciones[i]),
                                2)
    }
    tabla <- do.call(cbind, resumenes)
    return(tabla)
}
