function_time <- function(x, ...) {
        start_time <- Sys.time()
        x
        end_time <- Sys.time()
        
        result <- end_time - start_time
        print(result)
}