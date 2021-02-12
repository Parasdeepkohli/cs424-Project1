addUnits <- function(n) {
  print(n)
  labels <- ifelse(n < 100, paste0(round(n, digits = 2), '%'),  # percentages
                          ifelse(n < 1e9, paste0(round(n/1e6), ' Million'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e6)/1000, ' Billion'), '' # in Billions
                                        )))
  return(labels)
}