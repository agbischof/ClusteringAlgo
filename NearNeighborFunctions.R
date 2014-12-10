# Nearest Neighbors Classification

#takes a vector x and performs Standarad min-max normalziation 
#returns normalized vector min 0, max 1 
Std_normalize = function(x)
{return ((x-min(x)) / (max(x) - min(x)))}



knn = function (train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE) 
{
  train <- as.matrix(train)
  if (is.null(dim(test))) 
    dim(test) <- c(1, length(test))
  test <- as.matrix(test)
  if (any(is.na(train)) || any(is.na(test)) || any(is.na(cl))) 
    stop("no missing values are allowed")
  p <- ncol(train)
  ntr <- nrow(train)
  if (length(cl) != ntr) 
    stop("'train' and 'class' have different lengths")
  if (ntr < k) {
    warning(gettextf("k = %d exceeds number %d of patterns", 
                     k, ntr), domain = NA)
    k <- ntr
  }
  if (k < 1) 
    stop(gettextf("k = %d must be at least 1", k), domain = NA)
  nte <- nrow(test)
  if (ncol(test) != p) 
    stop("dims of 'test' and 'train' differ")
  clf <- as.factor(cl)
  nc <- max(unclass(clf))
  Z <- .C(VR_knn, as.integer(k), as.integer(l), as.integer(ntr), 
          as.integer(nte), as.integer(p), as.double(train), as.integer(unclass(clf)), 
          as.double(test), res = integer(nte), pr = double(nte), 
          integer(nc + 1), as.integer(nc), as.integer(FALSE), as.integer(use.all))
  res <- factor(Z$res, levels = seq_along(levels(clf)), labels = levels(clf))
  if (prob) 
    attr(res, "prob") <- Z$pr
  res
}