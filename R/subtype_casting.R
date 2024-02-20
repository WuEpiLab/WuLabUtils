#' @name subtype_casting
#' @rdname subtype_casting
#' @title casting subtypes into another model
#'
#' @aliases subtype_casting
#'
#' @importFrom pamr pamr.predict pamr.train
#' @export

subtype_casting = function(testData, him) {

  meta_df = as.data.frame(him[[1]])

  # Normalization
  return_combat = combat_correction(testData, meta_df)

  # Reconstructing training dataframe
  testData = return_combat[[1]]
  trainData = list(x=as.matrix(return_combat[[2]]),
                   y=him[[2]],
                   genenames=rownames(return_combat[[2]]),
                   samplelabels=him[[3]])

  # Training
  trainedData <- pamr.train(trainData)

  # Predicting subtypes
  classPredict <- pamr.predict(trainedData, as.matrix(testData), threshold=0, type="class")
  probPredict <- pamr.predict(trainedData, as.matrix(testData), threshold=0, type="posterior")

  prediction <- cbind(testData$samplelabels, classPredict, probPredict)
  prediction <- as.data.frame(prediction)

  prediction$classPredict <- paste0('CS',prediction$classPredict)
  colnames(prediction)[1] <- 'clust'

  return(prediction)
}
