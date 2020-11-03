#' Uniform Random Numbers
#'
#' This function is just a trivial wrapper for `runif`.
RandomNumbers <- function(n) {
  return(runif(n = n))
}

#' Coin Tosses
#'
#' This function generates a sequence of `n` coin tosses with `prob_heads`,
#' encoded as a character vector of length `n` containing only `'H'` and `'T'`.
#'
#' The R way to do this is:
#' `sample(c("H", "T"), n, replace = TRUE,`
#' ` prob = c(prob_heads, 1 - prob_heads))`.
CoinTosses <- function(n, prob_heads = 0.5) {
  sample_space <- c("H", "T")
  rn <- RandomNumbers(n)
  random_index <- ifelse(rn < prob_heads, 1, 2)
  return(sample_space[random_index])
}

#' Summarize Coin Tosses
#'
#' Generate a named vector of summary statistics of a coin toss vector (above).
SummarizeCoinTosses <- function(coin_tosses) {
  c(n = length(coin_tosses),
       n_heads = sum(coin_tosses == "H"),
       n_tails = sum(coin_tosses == "T"));
}

#' Die Rolls
#'
#' Uses `RandomNumbers` directly to generate `n` six-sided die rolls.
#' The R way to do this is:
#' `sample(1:6, n, replace = TRUE)`
DieRolls <- function(n) {
  rn <- RandomNumbers(n)
  return(floor(6 * rn) + 1) ## Formula on pg. 3
}

#' DeMere Simulation One
#'
#' Implements the simulation of DeMere's first wager, q.v.
DeMere1 <- function(m) {
  die_roll_matrix <- matrix(nrow = m, ncol = 4,
                           data = DieRolls(m * 4))
  roll_is_6_matrix <- die_roll_matrix == 6
  row_has_six <- rowSums(roll_is_6_matrix) >= 1
  return(mean(row_has_six))
}

#' DeMere Simulation Two
#'
#' Implements the simulation of DeMere's second wager, q.v.
DeMere2 <- function(m, n = 24) {
  first_die_roll_matrix <- matrix(nrow = m, ncol = n,
                                  data = DieRolls(m * n))
  second_die_roll_matrix <- matrix(nrow = m, ncol = n,
                                   data = DieRolls(m * n))
  both_rolls_are_6_matrix <- first_die_roll_matrix == 6 &
                             second_die_roll_matrix == 6
  row_has_two_sixes <- rowSums(both_rolls_are_6_matrix) >= 1
  return(mean(row_has_two_sixes))
}

#' Heads and Tails Simulation
#'
#' Simulates and summarizes the distribution of a trajectory of a trivial game
#'  based on `n` coin tosses, interpreted as `H`eads winning $1 and `T` losing $1.
#'
#' The distribution is estimated by sampling `m` trajectories.
#'
#' Returns a `tibble` with two columns each of length `m`:
#'  winnings: the total winnings at the end of the `n`th round.
#'  ahead: the total number of rounds during which the player was ahead.
HTSimulation <- function(m, n, prob_heads = 0.5) {
  outcomes <- matrix(nrow = m, ncol = n,
                     data = ifelse(CoinTosses(m * n, prob_heads) == "H",
                                   1L, -1L))
  winnings <- rowCumsums(outcomes)

  ## Apply "the convention that, when Peter’s winnings are 0, he is in the lead if
  ## he was ahead at the previous toss and not if he was behind at the previous toss."
  ## -- ItP, pg. 6
  ahead <- ifelse(winnings == 0, NA, winnings > 0)
  ahead_which_na <- which(is.na(ahead), arr.ind = TRUE)
  ahead[ahead_which_na] <- ahead[cbind(ahead_which_na[,1],
                                       ahead_which_na[,2]-1)]

  return(tibble(winnings = winnings[,n], ahead = rowSums(ahead)))
}

#' Heads and Tail Simulation Hexplot
#'
#' Instead of SpikeGraph, visualize the outcome of `HTSimulation`
#'  by using a hexplot. This requires a fairly large `m`.
HTSimulationHex <- function(hts) {
  maxahead <- 2 * ceiling(max(hts$ahead) / 2)
  hts %>% mutate(winnings =
                   pmax(-maxahead, pmin(maxahead,
                     winnings + runif(n(), min = -1, max = 1))),
                 ahead =
                   pmax(0, pmin(maxahead,
                     ahead + runif(n(), min = -1, max = 1)))
                ) %>%
          ggplot(aes(x=winnings, y=ahead)) +
          geom_hex() + geom_vline(xintercept = 0) + geom_hline(yintercept = maxahead / 2)
}

#' Heads and Tail Simulation Hexplot
#'
#' Instead of SpikeGraph, visualize the outcome of `HTSimulation`
#'  by using two marginal histograms, one for each column.
HTSimulationHist <- function(hts) {
  hts %>% pivot_longer(everything()) %>%
    ggplot(aes(x=value)) +
    geom_histogram(bins = nrow(hts)^(1/3)) +
    facet_grid(cols = vars(name), scales = "free", labeller = label_value) +
    xlab(list(ahead = "a", winnings = "b"))
}

#' SpikeGraph, Not Implemented
SpikeGraph <- function(...) {
  stop(paste("SpikeGraph is not implemented. For HTSimulation, try",
             "HTSimulationHex(htsim) or HTSimulationHist(htsim)."))
}

#' Heads and Tails Simulation Trace
#'
#' Simulate and plot `m` trajectories of the coin toss game.
#' NOTE: this is run on its own, without input from `HTSimulation`.
HTSimulationTrace <- function(m, n, prob_heads = 0.5) {
  winnings <- rowCumsums(matrix(nrow = m, ncol = n,
                                data = ifelse(CoinTosses(m * n, prob_heads) == "H", 1L, -1L)))
  liln <- sqrt(2 * 2 * n * log(log(n)))
  plot(1:n, rep_len(c(-liln, liln), length.out = n) , type = "n",
       xlab = "Round", ylab = "Winnings",
       main = paste(m, " Winnings Trajectories (p = ", prob_heads,")", sep = ""))
  for (i in 1:m) {
    lines(winnings[i,], col = "#000000", lwd=1)
    ## lines(winnings[i,], col = "#00000020", lwd=1) ## translucent.
  }
}
