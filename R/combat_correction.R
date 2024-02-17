#' @name combat_correction
#' @rdname combat_correction
#' @title casting subtypes into another model
#'
#' @aliases combat_correction
#'
#' @importFrom sva ComBat
#' @export

combat_correction = function(a, b) {
  a = as.data.frame(a)
  b = as.data.frame(b)
  
  # filters so that a and b contains the same rownames
  a = a %>% filter(row.names(a) %in% rownames(b))
  b = b %>% filter(row.names(b) %in% rownames(a))
  
  a = as.data.frame(t(a))
  b = as.data.frame(t(b))
  
  data = as.data.frame(t(rbind(a, b)))
  
  a$label = 'a'
  b$label = 'b'
  
  batch = rbind(a, b)
  data_combat = as.data.frame(ComBat(dat=data, batch=batch$label))
  
  print(nrow(a))
  print(ncol(data_combat))
  
  new_a = data_combat[,c(1:nrow(a))]
  new_b = data_combat[,c((nrow(a)+1):ncol(data_combat))]
  
  return(list(new_a, new_b))
}