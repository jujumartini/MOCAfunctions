# nest_sojourn
#
#
#
# Library dependencies:

nest_sojourn <- function(sojourn,
                         orig_soj_length_min = 180,
                         nest_length         = 60,
                         step_by             = .00001) {

  if (length(sojourn) > orig_soj_length_min) {

    n <-
      ceiling(length(sojourn) / nest_length) - 1

    additives <-
      seq(from = 0,
          to   = n / 10,
          by   = step_by)
    additives <-
      rep(additives,
          length.out = length(sojourn),
          each = nest_length)

    if (length(sojourn) %% nest_length > 0) {

      short_window <-
        length(sojourn) %% nest_length
      additives[(length(additives) - short_window):length(additives)] <-
        additives[(length(additives) - short_window)]

    }

    sojourn <-
      sojourn + additives

  }

  return(sojourn)

}
