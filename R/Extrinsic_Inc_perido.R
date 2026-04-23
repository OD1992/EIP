T_vals <- seq(0, 45, by = 1)
T_min <- 16

# Extrinsic Incubation Period function depending on T
# Below T_min the parasite cannot complete sporogony; return Inf so
# downstream p^n collapses to 0 (VC -> 0) instead of going negative.
EIP_safe <- function(T, T_min = 16) {
  denom <- T - T_min

  eip <- ifelse(
    is.na(T), NA,
    ifelse(denom <= 0, Inf, 111 / denom)
  )

  return(eip)
}
