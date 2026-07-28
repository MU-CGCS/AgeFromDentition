#' Reference completion threshold for terminal (Ac) teeth
#'
#' A tooth at stage `Ac` (apex closure) is complete. That observation cannot
#' be inverted into an age, because apex closure is terminal: it says only
#' that the individual is older than the completion age. This function
#' reports the alternative — a descriptive percentile of the reference
#' distribution of age at completion.
#'
#' @param sex character, `"F"` or `"M"`.
#' @param teeth character vector of tooth names observed at stage `Ac`. May
#'   be length 0, in which case the threshold is `NA`.
#' @param q lower-tail probability, in `(0, 0.5)`. Defaults to `0.025`.
#' @param method `"predictive"` (default) or `"plugin"`. See Details.
#'
#' @return A list with components:
#' \describe{
#'   \item{threshold}{numeric, the largest per-tooth threshold, or
#'     `NA_real_` if `teeth` is empty}
#'   \item{binding_tooth}{character, the tooth that produced it}
#'   \item{q}{the lower-tail probability used}
#'   \item{method}{the method used}
#'   \item{per_tooth}{data frame of the per-tooth calculation}
#'   \item{low_precision}{logical, binding tooth poorly estimated}
#'   \item{tied_transition}{logical, binding tooth's `Ac` is tied with the
#'     preceding stage}
#' }
#'
#' @details
#' # What the number means
#'
#' For a tooth \eqn{t}, the threshold is the \eqn{q}-th percentile of the
#' sex-specific reference distribution of age at *entering* `Ac`:
#'
#' \deqn{L(t, q) = \exp(\mu_t + z_q \sigma_t)}
#'
#' It says that \eqn{q} of the reference sample had completed that tooth by
#' that age. It is a **descriptive completion threshold for the reference
#' sample**. It is *not* a \eqn{1 - q} lower confidence or credible limit
#' for the individual being assessed, and must never be reported as one.
#'
#' # Predictive versus plug-in
#'
#' `method = "plugin"` takes \eqn{\sigma_t} to be `log_sd`, treating the
#' fitted `log_mu` as known.
#'
#' `method = "predictive"`, the default, uses
#' \eqn{\sigma_t = \sqrt{\mathrm{log\_sd}^2 + \mathrm{se\_log\_mu}^2}},
#' giving the percentile of the predictive distribution for a further
#' individual drawn from the reference population. `log_mu` is estimated,
#' and for the terminal transitions it is sometimes estimated from very few
#' individuals.
#'
#' The choice is immaterial for most teeth and decisive for one. Ten of the
#' twelve `Ac` transitions differ by less than 0.06 years between methods;
#' female M3 differs by 0.489 and male M3 by 0.304, because those two rest
#' on n = 1 and n = 5. Since M3 attains `Ac` latest of any tooth, it sets
#' the threshold whenever it is scored — so the two methods disagree most
#' in exactly the case this function exists to serve.
#'
#' Both are reported in the return value, so a stated convention always
#' reproduces a specific number.
#'
#' # Combining several teeth
#'
#' With more than one `Ac` tooth the largest per-tooth threshold is
#' reported. This is a transparent **reporting convention**, not a
#' statistical combination: it ignores the joint probability of the observed
#' completion pattern and any correlation between teeth within a person.
#' The full `per_tooth` table is always returned so that the convention can
#' be inspected rather than trusted.
#'
#' # Flags
#'
#' `low_precision` marks a binding tooth estimated from few individuals
#' (n < 10) or with a large standard error (`se_log_mu` > 0.04). Under
#' `method = "predictive"` the imprecision is already reflected in the
#' number; the flag drives reporting.
#'
#' `tied_transition` marks a binding tooth whose `Ac` transition is tied
#' with the stage below it in [StageTies]. For female M3 this is the case:
#' `A.5` and `Ac` are fitted at the same age, so observing apex closure
#' supplies no information beyond `A.5`.
#'
#' No tooth is ever dropped on account of either flag. Discarding the
#' binding tooth would understate the threshold and throw away the very
#' observation being reported on.
#'
#' @seealso [AttainmentTables], [StageTies]
#'
#' @export
#'
#' @examples
#' # A single completed second molar, in a female
#' ac_completion_threshold("F", "M2")$threshold
#'
#' # The plug-in convention gives a slightly different number
#' ac_completion_threshold("F", "M2", method = "plugin")$threshold
#'
#' # With several teeth complete, the latest-forming one binds
#' res <- ac_completion_threshold("F", c("Canine", "P3", "P4", "M1"))
#' res$binding_tooth
#' res$per_tooth
#'
ac_completion_threshold <- function(sex, teeth, q = 0.025,
                                    method = c("predictive", "plugin")) {
  method <- match.arg(method)

  if (length(sex) != 1L || is.na(sex) || !(sex %in% c("F", "M"))) {
    stop("`sex` must be a single value, either \"F\" or \"M\".",
         call. = FALSE)
  }

  if (!is.numeric(q) || length(q) != 1L || is.na(q) || q <= 0 || q >= 0.5) {
    stop("`q` must be a single number in (0, 0.5).", call. = FALSE)
  }

  teeth <- as.character(teeth)
  if (anyNA(teeth)) {
    stop("`teeth` must not contain NA.", call. = FALSE)
  }
  if (anyDuplicated(teeth) > 0L) {
    stop("`teeth` must not contain duplicates: ",
         paste(unique(teeth[duplicated(teeth)]), collapse = ", "), ".",
         call. = FALSE)
  }

  known_teeth <- unique(AttainmentTables$Tooth)
  unknown <- setdiff(teeth, known_teeth)
  if (length(unknown) > 0L) {
    stop("Unknown tooth name: ", paste(unknown, collapse = ", "),
         ". Expected one of ", paste(known_teeth, collapse = ", "), ".",
         call. = FALSE)
  }

  # Rows come out in AttainmentTables order rather than the order `teeth`
  # was supplied in, so the result does not depend on how the caller
  # happened to list the teeth.
  per_tooth <- AttainmentTables |>
    dplyr::filter(Stage == "Ac", Sex == sex, Tooth %in% teeth) |>
    dplyr::mutate(
      sd_eff = if (method == "predictive") {
        sqrt(log_sd^2 + se_log_mu^2)
      } else {
        log_sd
      },
      threshold = exp(log_mu + stats::qnorm(q) * sd_eff)
    ) |>
    dplyr::select(Tooth, log_mu, log_sd, se_log_mu, sd_eff, n, threshold)

  if (nrow(per_tooth) == 0L) {
    return(list(
      threshold = NA_real_,
      binding_tooth = NA_character_,
      q = q,
      method = method,
      per_tooth = per_tooth,
      low_precision = FALSE,
      tied_transition = FALSE
    ))
  }

  binding <- which.max(per_tooth$threshold)
  binding_tooth <- per_tooth$Tooth[binding]

  low_precision <- per_tooth$n[binding] < 10L ||
    per_tooth$se_log_mu[binding] > 0.04

  tied <- StageTies |>
    dplyr::filter(
      Sex == sex,
      Tooth == binding_tooth,
      Stage_hi == "Ac",
      tie_class == "terminal"
    )

  return(list(
    threshold = per_tooth$threshold[binding],
    binding_tooth = binding_tooth,
    q = q,
    method = method,
    per_tooth = per_tooth,
    low_precision = low_precision,
    tied_transition = nrow(tied) > 0L
  ))
}
