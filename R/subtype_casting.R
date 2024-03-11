#' @name subtype_casting
#' @rdname subtype_casting
#' @title casting subtypes into another model
#'
#' @aliases subtype_casting
#'
#' @importFrom pamr pamr.predict pamr.train
#' @export

subtype_casting = function(data, ref) {

  meta_df = as.data.frame(ref[[1]])

  # Normalization
  return_combat = combat_correction(data, meta_df)

  # Remove NA terns from combat results
  return_combat[[1]] <- na.omit(return_combat[[1]])
  return_combat[[2]] <- na.omit(return_combat[[2]])

  # Reconstructing training dataframe
  data = return_combat[[1]]
  trainData = list(x=as.matrix(return_combat[[2]]),
                   y=ref[[2]],
                   genenames=rownames(return_combat[[2]]),
                   samplelabels=ref[[3]])

  # Training
  trainedData <- pamr.train(trainData)

  # Predicting subtypes
  classPredict <- pamr.predict(trainedData, as.matrix(data), threshold=0, type="class")
  probPredict <- pamr.predict(trainedData, as.matrix(data), threshold=0, type="posterior")

  prediction <- cbind(data$samplelabels, classPredict, probPredict)
  prediction <- as.data.frame(prediction)

  prediction$classPredict <- paste0('CS',prediction$classPredict)
  colnames(prediction)[1] <- 'clust'

  return(prediction)
}
