# Documentation of the datasets.
# All datasets come from the MSQC package (Santos-Fernandez, 2012),
# distributed under GPL-2 | GPL-3, which was archived on CRAN in 2024.

#' Bimetal thermostats: Phase I and Phase II
#'
#' Measurements on brass and steel bimetal thermostats: deflection,
#' curvature, resistivity and hardness on the low and high expansion sides.
#' `bimetal1` is used as Phase I (the process is assumed to be in control)
#' and `bimetal2` as Phase II.
#'
#' @format Numeric matrices of 28 rows (individual observations) and 5
#'   columns:
#' \describe{
#'   \item{deflection}{deflection, in \eqn{10^{-6}}/K}
#'   \item{curvature}{curvature, in \eqn{10^{-6}}/K}
#'   \item{resistivity}{resistivity, in \eqn{10^{-1}\Omega \cdot}mm\eqn{^2}/m}
#'   \item{Hardness low side}{hardness of the low expansion side, in 10 N/mm\eqn{^2}}
#'   \item{Hardness high side}{hardness of the high expansion side, in 10 N/mm\eqn{^2}}
#' }
#' @source Santos-Fernandez, E. (2012). *Multivariate Statistical Quality
#'   Control Using R*. Springer. Data distributed in the MSQC package.
#' @examples
#' data(bimetal1)
#' summary(bimetal1)
#' pairs(bimetal1)
"bimetal1"

#' @rdname bimetal1
"bimetal2"

#' Carbon fiber tubes: Phase I and Phase II
#'
#' Three quality characteristics (inner diameter, thickness and length)
#' measured on carbon fiber tubes, in subgroups of 8 tubes. `carbon1` (30
#' subgroups) is used as Phase I, the process being assumed stable;
#' `carbon2` (25 subgroups) is used as Phase II.
#'
#' @format Numeric arrays of dimensions \eqn{30 \times 3 \times 8}
#'   (`carbon1`) and \eqn{25 \times 3 \times 8} (`carbon2`): subgroups
#'   \eqn{\times} characteristics (`inner`, `thickness`, `length`)
#'   \eqn{\times} observations.
#' @source Santos-Fernandez, E. (2012). *Multivariate Statistical Quality
#'   Control Using R*. Springer. Data distributed in the MSQC package.
#' @examples
#' data(carbon1)
#' dim(carbon1)
#' Moy_X(carbon1)[1:5, ]
"carbon1"

#' @rdname carbon1
"carbon2"

#' Target archery: ranking round and elimination stage
#'
#' Coordinates of arrows shot by an archer, in subgroups of 3 arrows.
#' `archery1` (ranking round, 72 arrows) is used as Phase I and `archery2`
#' (elimination stage) as Phase II.
#'
#' @format Numeric arrays of dimensions \eqn{24 \times 2 \times 3}
#'   (`archery1`) and \eqn{18 \times 2 \times 3} (`archery2`), with
#'   characteristics `x-coordinate` and `y-coordinate`.
#' @source Santos-Fernandez, E. (2012). *Multivariate Statistical Quality
#'   Control Using R*. Springer. Data distributed in the MSQC package.
#' @examples
#' data(archery1)
#' dim(archery1)
"archery1"

#' @rdname archery1
"archery2"

#' Dowel pins: Phase I and Phase II
#'
#' Diameter and length of dowel pins (individual observations). `dowel1`
#' is used as Phase I and `dowel2` as Phase II.
#'
#' @format Data frames with 40 rows (`dowel1`) and 32 rows (`dowel2`) and 2
#'   numeric columns, `diameter` and `length`.
#' @source Santos-Fernandez, E. (2012). *Multivariate Statistical Quality
#'   Control Using R*. Springer. Data distributed in the MSQC package.
#' @examples
#' data(dowel1)
#' plot(dowel1)
"dowel1"

#' @rdname dowel1
"dowel2"

#' Glass manufacturing process: Phase I and Phase II
#'
#' Three quality characteristics of a glass manufacturing process, measured
#' in subgroups of 5. `glass1` is used to establish the in-control situation
#' (Phase I) and `glass2` as Phase II.
#'
#' @format Numeric arrays of dimensions \eqn{32 \times 3 \times 5}
#'   (`glass1`) and \eqn{25 \times 3 \times 5} (`glass2`), with
#'   characteristics `var1`, `var2` and `var3`.
#' @source Santos-Fernandez, E. (2012). *Multivariate Statistical Quality
#'   Control Using R*. Springer. Data distributed in the MSQC package.
#' @examples
#' data(glass1)
#' dim(glass1)
"glass1"

#' @rdname glass1
"glass2"

#' Bivariate industrial process: Phase I and Phase II
#'
#' Individual observations of a bivariate industrial process. `indust1` is
#' used as Phase I and `indust2` as Phase II.
#'
#' @format Numeric matrices with 28 rows (`indust1`) and 35 rows
#'   (`indust2`) and 2 columns, `var1` and `var2`.
#' @source Santos-Fernandez, E. (2012). *Multivariate Statistical Quality
#'   Control Using R*. Springer. Data distributed in the MSQC package.
#' @examples
#' data(indust1)
#' plot(indust1)
"indust1"

#' @rdname indust1
"indust2"

#' Mechanical process: Phase I and Phase II
#'
#' Seven variables measured on a mechanical process (individual
#' observations). `mech1` is used as Phase I and `mech2` as Phase II.
#'
#' @format Numeric matrices with 45 rows (`mech1`) and 50 rows (`mech2`) and
#'   7 columns, `var1` to `var7`.
#' @source Santos-Fernandez, E. (2012). *Multivariate Statistical Quality
#'   Control Using R*. Springer. Data distributed in the MSQC package.
#' @examples
#' data(mech1)
#' round(cor(mech1), 2)
"mech1"

#' @rdname mech1
"mech2"

#' Water quality tests: Phase I and Phase II
#'
#' Five variables measured in water quality tests: pH, phosphates (mg/L),
#' nitrates (mg/L), dissolved oxygen and total solids (mg/L). `water1` is
#' used as Phase I and `water2` as Phase II.
#'
#' @format Numeric matrices with 30 rows (`water1`) and 25 rows (`water2`)
#'   and 5 columns, `pH`, `phosph`, `nitrates`, `oxygen` and `solids`.
#' @source Santos-Fernandez, E. (2012). *Multivariate Statistical Quality
#'   Control Using R*. Springer. Data distributed in the MSQC package.
#' @examples
#' data(water1)
#' summary(water1)
"water1"

#' @rdname water1
"water2"

#' Pitching logs of C.C. Sabathia (2011)
#'
#' Pitch data of the baseball pitcher C.C. Sabathia in two games against
#' Tampa Bay. `sabathia1` (July 10, 2011) and `sabathia2` (August 12, 2011)
#' contain the means of rational subgroups; `sabathia.ind` contains the
#' individual pitches of the July 10, 2011 game.
#'
#' @format
#' \describe{
#'   \item{`sabathia1`}{numeric matrix with 23 rows and 3 columns: `px`
#'     (horizontal position), `pz` (vertical position) and `start speed`
#'     (starting speed of the fastball).}
#'   \item{`sabathia2`}{numeric matrix with 26 rows and the same 3 columns.}
#'   \item{`sabathia.ind`}{numeric matrix with 51 rows and 2 unnamed columns
#'     (horizontal and vertical positions).}
#' }
#' @source Santos-Fernandez, E. (2012). *Multivariate Statistical Quality
#'   Control Using R*. Springer. Data distributed in the MSQC package.
#' @examples
#' data(sabathia1)
#' summary(sabathia1)
"sabathia1"

#' @rdname sabathia1
"sabathia2"

#' @rdname sabathia1
"sabathia.ind"

#' Strike calls of umpire Ron Kulpa
#'
#' Coordinates of the pitches called "strike" by umpire Ron Kulpa in a Tampa
#' Bay game on July 10, 2011.
#'
#' @format A numeric matrix with 113 rows and 2 unnamed columns: horizontal
#'   (x) and vertical (z) coordinates.
#' @source Santos-Fernandez, E. (2012). *Multivariate Statistical Quality
#'   Control Using R*. Springer. Data distributed in the MSQC package.
#' @examples
#' data(kulpa)
#' plot(kulpa, xlab = "x", ylab = "z")
"kulpa"

#' Right-skewed bivariate dataset
#'
#' A simulated right-skewed bivariate dataset, useful to illustrate the
#' effect of non-normality on multivariate control charts.
#'
#' @format A numeric matrix with 30 rows and 2 columns, `var1` and `var2`.
#' @source Santos-Fernandez, E. (2012). *Multivariate Statistical Quality
#'   Control Using R*. Springer. Data distributed in the MSQC package.
#' @examples
#' data(rskewed)
#' hist(rskewed[, 1])
"rskewed"
