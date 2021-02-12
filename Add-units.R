addUnits <- function(n) {
  labels <- ifelse((n < 100) & (n > 0), paste0(round(n, digits = 2), '%'),  # percentages
                          ifelse((n < 1e9) & (n > 0), paste0(round(n/1e6), ' Million'),  # in millions
                                 ifelse((n < 1e12) & (n > 0), paste0(round(n/1e6)/1000, ' Billion'), '' # in Billions
                                        )))
  return(labels)
}