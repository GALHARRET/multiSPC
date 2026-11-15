#' multiSPC: Univariate and Multivariate Statistical Process Control
#'
#' Tools for teaching and applying statistical process control (SPC):
#' estimation of process parameters, Shewhart and EWMA control charts,
#' capability indices, and multivariate control charts (Hotelling \eqn{T^2}
#' for subgrouped and individual data, MYT decomposition, MEWMA, generalized
#' variance and charts on principal components).
#'
#' @section Data structure:
#' Subgrouped multivariate data are stored in three-way arrays of dimensions
#' \eqn{k \times p \times n}: \eqn{k} subgroups, \eqn{p} quality
#' characteristics and \eqn{n} observations per subgroup (see [carbon1]).
#' Individual multivariate data are stored in \eqn{k \times p} matrices (see
#' [bimetal1]).
#'
#' @section Main functions:
#' \itemize{
#'   \item Constants and estimation: [c4()], [d2()], [d3()], [sigma_I()].
#'   \item Univariate charts: [plot_chart()], [efficacite_Shewart_moy()],
#'     [ewma_cart()], [capability()].
#'   \item Multivariate charts: [T2_Hotelling_kn()], [LSC_T2_Hotelling_kn()],
#'     [T2_Hotelling_k1()], [LSC_T2_Hotelling_k1()],
#'     [T2_Hotelling_kn_MYT()], [T2_MEWMA_k1()], [T2_MEWMA_kn()],
#'     [generalized_variance()], [Principal_Component_construction()].
#'   \item Helpers: [Moy_X()], [Moy_T()], [covariance_X()].
#' }
#'
#' @importFrom stats cov var sd qf qbeta qchisq qnorm pnorm rnorm
#' @importFrom ggplot2 ggplot aes labs theme_minimal geom_point geom_line annotate
#' @importFrom utils globalVariables
#' @keywords internal
"_PACKAGE"

# Column names used inside aes() in plot_chart()
utils::globalVariables(c("t", "values"))
