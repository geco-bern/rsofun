identify_pattern <- function( vec ){

  eps <- 1e-4

  vec <- as.numeric(as.character(vec))
  
  ## identify all numbers that appear more than once (already suspicious)
  counts <- as.data.frame( table( vec ) )
  counts <- counts[ order(-counts$Freq),  ]
  counts <- counts[ which(counts$Freq>2), ]

  ## convert factors to numeric
  counts$vec  <- as.numeric(levels(counts$vec))[counts$vec]

  for (idx in 1:nrow(counts)){

    ## find where this value appears
    pos <- which( abs(vec-counts$vec[idx])<eps )

    ## replace all numbers that appear more than twice with NA (assuming they are suspicious/wrong)
    vec[ pos ] <- NA

  }

  return( vec )

}
