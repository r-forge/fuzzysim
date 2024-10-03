modOverlap <- function(pred1, pred2, na.rm = TRUE) {

  # version 2.0 (3 Oct 2024)

  if (inherits(pred1, "SpatRaster"))
    pred1 <- terra::values(pred1, mat = FALSE, dataframe = FALSE)
  if (inherits(pred2, "SpatRaster"))
    pred2 <- terra::values(pred2, mat = FALSE, dataframe = FALSE)

  pred1 <- unlist(pred1)
  pred2 <- unlist(pred2)  # can't remember what this was for...

  stopifnot(length(pred1) == length(pred2),
            #pred1 >= 0 && pred1 <=1,
            #pred2 >= 0 && pred2 <=1
            min(c(pred1, pred2), na.rm = TRUE) >= 0,
            max(c(pred1, pred2), na.rm = TRUE) <= 1
            )

  p1 <- pred1 / sum(pred1, na.rm = na.rm)
  p2 <- pred2 / sum(pred2, na.rm = na.rm)

  SchoenerD <- 1 - 0.5 * sum(abs(p1 - p2), na.rm = na.rm)
  HellingerDist <- sqrt(sum((sqrt(p1) - sqrt(p2))^2, na.rm = na.rm))
  WarrenI <- 1 - ((HellingerDist^2) / 2)

  list(SchoenerD = SchoenerD,
       WarrenI = WarrenI,
       HellingerDist = HellingerDist
       )
}  # end modOverlap function
