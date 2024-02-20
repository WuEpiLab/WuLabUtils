#' @name subtype_pam50
#' @rdname subtype_pam50
#' @title predict expression data into pam50 subtype
#'
#' @aliases subtype_pam50
#'
#' @import genefu
#' @import org.Hs.eg.db
#' @export

subtype_pam50 = function(expression) {

  rownames(expression) <- expression[,1]

  symbols <- expression[,1]##提取基因symbol
  ##id 转换 to entrz

  s2g <- toTable(org.Hs.egSYMBOL)
  ids <- s2g[match(symbols, s2g$symbol),1]
  ##构造注释文件
  #  probe Gene.symbol Gene.ID
  id_df <- data.frame(probe = symbols,
                      "Gene.Symbol" = symbols,
                      "EntrezGene.ID" = ids)

  ##保留已注释的基因及注释文件
  expression <- expression[!is.na(id_df$EntrezGene.ID),]
  id_df <- id_df[!is.na(id_df$EntrezGene.ID),]
  head(id_df)

  expression <- expression[,-1]
  expression <- as.data.frame(t(expression))

  ##使用genefu包进行分子分型
  data(pam50.robust)
  subtypes <- molecular.subtyping(sbt.model = "pam50", data=expression,
                                  annot=id_df, do.mapping=TRUE)

  return(data.frame(id=names(subtypes[[1]]), subtype=subtypes[[1]]))
}
