# Taken from:
# <http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram>


# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}



# matrix of the p-value of the correlation
# p.mat.euro <- cor.mtest(bcn_full %>% select(European_incoming, 7:24))
# p.mat.latino <- cor.mtest(bcn_full %>% select(Latino_incoming, 7:24))

#corrplot(cor(as.matrix(bcn_full %>% select(European_incoming, 7:24))), 
#         type="upper", p.mat = p.mat.euro, sig.level = 0.01, insig = "blank")


#corrplot(cor(as.matrix(bcn_full %>% select(Latino_incoming, 7:24))), 
#         type="upper", p.mat = p.mat.latino, sig.level = 0.01, insig = "blank")


