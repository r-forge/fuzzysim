prevalence <-
function (obs, event = 1) {
  # version 1.0
  # calculates the prevalence (proportion of occurrences) of a value (event) in a vector
  # obs: a vector of binary observations (e.g. 1 vs. 0, male vs. female, disease vs. no disease, etc.)
  # event: the value whose prevalence we want to calculate (e.g. 1, "present", etc.)
  sum(obs == event) / length(obs)
}
