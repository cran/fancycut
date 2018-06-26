#' Like \code{cut}, turn a vector of numbers into a factor
#'
#' @param x           a numeric vector
#' @param na.bucket   what level should NA values be given?
#' @param unmatched.bucket
#'   what level should numbers not covered by an interval be given?
#' @param out.as.factor
#'   default is TRUE
#'   Should the resulting vector be a factor?
#'   If FALSE will return a character vector.
#' @param ...
#'   These take the form \code{tag = value}.
#'   Tags become the bucket names and values the interval definitions.
#'
#' @examples
#' fancycut(
#'   x = -10:10,
#'   Zero = 0,
#'   Small = '[0,2)',
#'   Medium = '[2,5]',
#'   Large = '(5,10]'
#' )
#'
#' # The following examples are from Richie Cotton via
#' # https://www.rdocumentation.org/packages/fancycut/versions/0.1.1/topics/fancycut
#'
#' # The tag = value syntax is useful.
#' x <- seq.int(0, 1, 0.25)
#' fancycut(x, low = '[0, 0.5]', high = '(0.5, 1]')
#'
#' # Not all the values have to live in a bucket.
#' x <- seq.int(0, 1, 0.25)
#' fancycut(x, low = '(0.2, 0.3]', high = '(0.7, 0.8)')
#'
#' # You can use unmatched.bucket to deal with these other intervals.
#' x <- seq.int(0, 1, 0.25)
#' fancycut(x, low = '(0.2, 0.3]', high = '(0.7, 0.8)', unmatched.bucket = 'other')
#'
#' # To match a specific value, make the lower and upper bound the same number.
#' x <- seq.int(0, 1, 0.25)
#' fancycut(x, low = '[0, 0.5)', half = '[0.5,0.5]', high = '(0.5, 1]')
#'
#' # To match NA values, use na.bucket.
#' x2 <- c(seq.int(0, 1, 0.25), NA)
#' fancycut(x2, low = '[0, 0.5)', high = '[0.5, 1]', na.bucket = 'missing')
#'
#' @export
fancycut <- function(x, na.bucket = NA, unmatched.bucket = NA,
                     out.as.factor = TRUE, ...) {

  # Handle dots first
  dots <- as.list(substitute(list(...)))[-1L]

  if (length(dots) > 0) {
    buckets <- names(dots)
    intervals <- as.character(dots)
  }

  return(wafflecut(
    x = x,
    intervals = intervals,
    buckets = buckets,
    na.bucket = na.bucket,
    unmatched.bucket = unmatched.bucket,
    out.as.factor = out.as.factor
  ))

}



#' Like \code{cut}, turn a vector of numbers into a factor
#'
#' @param x           a numeric vector
#' @param intervals   a character vector of intervals
#' @param buckets     a character vector of levels for the new factor
#'   these have a 1-1 correspondence with \code{intervals}
#' @param na.bucket   what level should NA values be given?
#' @param unmatched.bucket
#'   what level should numbers not covered by an interval be given?
#' @param out.as.factor
#'   default is TRUE
#'   Should the resulting vector be a factor?
#'   If FALSE will return a character vector.
#'
#' @examples
#'
#'
#' wafflecut(-10:10, c('[0,2)','[2,5)','[5,10]'), c('Small','Medium','Large'))
#'
#' wafflecut(-10:10, c('[0,0]','(0,2]','(2,5)','[5,10]'), c('Zero','Small','Medium','Large'))
#'
#' wafflecut(-10:10, c('[0,2)','[2,5)','[5,10]'), c('Small','Medium','Large'))
#'
#' wafflecut(-10:10, c('[0,0]','[0,2]','(2,5)','[5,10]'), c('Zero','Small','Medium','Large'))
#'
#'
#' # The following examples are from Richie Cotton via
#' # https://www.rdocumentation.org/packages/fancycut/versions/0.1.1/topics/fancycut
#'
#' # Not all the values have to live in a bucket.
#' x <- seq.int(0, 1, 0.25)
#' wafflecut(x, c('(0.2, 0.3)', '(0.7, 0.8)'), c('low', 'high'))
#'
#' # You can use unmatched.bucket to deal with these other intervals.
#' x <- seq.int(0, 1, 0.25)
#' wafflecut(x, c('(0.2, 0.3)', '(0.7, 0.8)'), c('low', 'high'), unmatched.bucket = 'other')
#'
#' # To match NA values, use na.bucket.
#' x2 <- c(seq.int(0, 1, 0.25), NA)
#' wafflecut(x2, c('[0, 0.5)', '[0.5, 1]'), c('low', 'high'), na.bucket = 'missing')
#'
#'
#' @export
wafflecut <- function(x, intervals, buckets = intervals,
                     na.bucket = NA, unmatched.bucket = NA,
                     out.as.factor = TRUE) {

  # Make sure that intervals and buckets are the same length
  l <- length(intervals)
  if(l != length(buckets)) {
    stop('FancyCut requires a 1-1 map from intervals to buckets')
  }

  # Make sure that x is numeric
  if (!is.numeric(x))
    stop("'x' must be numeric")


  out <- rep(NA, length(x))

  intervals_df <- parse_intervals(intervals)

  for(index in 1:l) {

    b <- buckets[index]
    lower <- intervals_df$left[index]
    upper <- intervals_df$right[index]
    left <- intervals_df$left_strict[index]
    right <- intervals_df$right_strict[index]

    mask <- rep(FALSE, length(x))
    if(left  & right)  {mask <- x >= lower & x <= upper}
    if(left  & !right) {mask <- x >= lower & x <  upper}
    if(!left & right)  {mask <- x >  lower & x <= upper}
    if(!left & !right) {mask <- x >  lower & x <  upper}

    out[mask] <- b
  }

  if (sum(is.na(x)) == 0L) {
    na.bucket <- NULL
  } else {
    out[is.na(x)]  <- na.bucket
  }

  if (sum(is.na(out)) == 0L) {
    unmatched.bucket <- NULL
  } else {
    out[is.na(out)]  <- unmatched.bucket
  }

  levels <- unique(c(buckets, na.bucket, unmatched.bucket))

  if(out.as.factor) {
    return(factor(
      out,
      levels = levels,
      exclude = NULL
    ))
  } else {
    return(out)
  }
}



# Based on suggestions from Richie Cotton
#   https://github.com/adamleerich/fancycut/issues/4

parse_intervals <- function(intervals) {
  rx <- "^\\s*(\\(|\\[)\\s*((?:[-+]?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?)|(?:[-+]?Inf))\\s*,\\s*((?:[-+]?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?)|(?:[-+]?Inf))\\s*(\\)|\\])\\s*$"
  lindex <- regexec(rx, intervals)
  lmatch <- regmatches(intervals, lindex)
  nrows <- length(lmatch)
  ncols <- sapply(lmatch, length)
  mmatch <- matrix(NA_character_, nrow = nrows, ncol = 5)

  for (x in 1:nrows) {
    row <- lmatch[[x]]
    n <- length(row)
    if (n > 0) {
      mmatch[x, 1:n] <- lmatch[[x]][1:n]
    }
  }

  intervals_df <- data.frame(
    interval = intervals,
    left = as.numeric(mmatch[, 3]),
    right = as.numeric(mmatch[, 4]),
    left_strict = (mmatch[, 2] == '['),
    right_strict = (mmatch[, 5] == ']'),
    match_count = ncols,
    stringsAsFactors = FALSE
  )

  # Fix if point values
  rx <- "^[-+]?\\d*\\.?\\d+(?:[eE][-+]?\\d+)?$"
  points <- grepl(rx, intervals)
  intervals_df$interval[points] <- intervals[points]
  intervals_df$left[points] <- as.numeric(intervals[points])
  intervals_df$right[points] <- as.numeric(intervals[points])
  intervals_df$right_strict[points] <- TRUE
  intervals_df$left_strict[points] <- TRUE
  intervals_df$match_count[points] <- 5


  for (x in 1:nrows) {

    if (intervals_df$match_count[x] != 5) {
      warning(paste0('The interval "',intervals_df$interval[x],'" is malformed.'))
      next
    }

    if (intervals_df$right[x] < intervals_df$left[x]) {
      warning(paste0('The interval "',intervals_df$interval[x],'" has right < left.'))
    }

    if (intervals_df$right[x] == intervals_df$left[x] &
        (!intervals_df$left_strict[x] | !intervals_df$right_strict[x])) {
      warning(paste0('The interval "',intervals_df$interval[x],'" is malformed.'))
    }

  }

  return(intervals_df)

}




