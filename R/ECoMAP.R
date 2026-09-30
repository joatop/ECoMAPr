# 1.1 ====  ECoMAP Implementation Function (version 1) ====
# ECoMAP_bruBackbone <- function(modForm, occData, covarRast, outLoc, numCores) {
#   # 1.1.1 ---- Process the input arguments ----
#   # Helper function to sanity check model formulas
#   formParam <- function(inA) {
#     inForm <- tryCatch(as.formula(inA), error = function(err) {
#       stop("invalid model formulation: ", err)
#     })
#     inForm
#   }
#   if(is.list(modForm)) {
#     
#   }
#}