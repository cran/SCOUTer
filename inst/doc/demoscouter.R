## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4,
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(SCOUTer)

## ----pcamb--------------------------------------------------------------------
X <- as.matrix(X)
pcamodel_ref <- pcamb_classic(X, 2, 0.05, "cent")

## ----pcame--------------------------------------------------------------------
pcax <- pcame(X, pcamodel_ref)

## ----dscplot_ref, fig.cap="Distance plot (left) and score plot (right) of the reference data and PCA model.", fig.dim = c(6, 3)----
dscplot(X, pcamodel_ref)

## ----dscplot_ref_vertical, fig.cap="Distance plot (left) and score plot (right) of the reference data and PCA model with vertical layout.", fig.dim = c(3, 5.5)----
dscplot(X, pcamodel_ref, nrow = 2, ncol = 1)

## ----distplot_ref, fig.cap="Distance plot of the reference data and PCA model.", fig.dim = c(3, 3)----
distplot(X, pcamodel_ref)

## ----obscontrib, fig.cap="Contribution panel for _SPE_ and _T^2^_ values.", fig.dim = c(9, 2), fig.fullwidth = TRUE----
obscontribpanel(pcax, pcamodel_ref, which.max(pcax$SPE))

## ----barplot1, fig.cap="Bar plot with the _SPE_ value of an observation (bar) and the UCL according to the PCA model (red line)", fig.dim = c(1, 1.5)----
# Display SPE of the first observation
barwithucl(pcax$SPE, 1, pcamodel_ref$limspe, plotname = "SPE")

## ----barplot2, fig.cap="Bar plot with the contributions of each variable (error vector, *e*) to the _SPE_ value", fig.dim = c(2, 2)----
# Display contributions to the SPE of the same observation
custombar(pcax$E, 1, plotname = "Contributions to SPE")

## ----scout_x_simple-----------------------------------------------------------
set.seed(1218) # ensure always the same result 
indsel <- sample(1:nrow(X), 1)
x <- t(as.matrix(X[indsel,]))
x.out <- scout(x, pcamodel_ref, T2.y = 40, SPE.y = 40, mode = "simple")

## ----scout_X_simple-----------------------------------------------------------
n <- nrow(X)
X.T2.40 <- scout(X, pcamodel_ref, T2.y = matrix(40, n, 1), mode = "simple")

## ----dcsplot_X_simple, fig.cap="Distance plot (left) and score plot (right) of the data simulated with simple mode.", fig.dim = c(6, 3)----
X.all <- rbind(X, X.T2.40$X)
tag.all <- dotag(X, X.T2.40$X)
dscplot(X.all, pcamodel_ref, obstag = tag.all)

## ----scout_x_steps, fig.cap="Distance plot (left) and score plot (right) of the data simulated with steps mode.", fig.dim = c(6, 3)----
x.out.steps <- scout(x, pcamodel_ref, T2.y = 40, SPE.y = 40, nsteps = 10, mode = "steps")
x.all <- rbind(x, x.out.steps$X)
tag.all <- dotag(x, x.out.steps$X)
dscplot(x.all, pcamodel_ref, obstag = tag.all)

## ----scout_x_grid, fig.cap="Distance plot (left) and score plot (right) of the data simulated with grid mode.", fig.dim = c(6, 3)----
x.out.grid <- scout(x, pcamodel_ref, T2.y = 40, SPE.y = 40, nsteps.spe = 2, nsteps.t2 = 3, gspe = 3, gt2 =0.3, mode = "grid")
x.all <- rbind(x, x.out.grid$X)
tag.all <- dotag(x, x.out.grid$X)
dscplot(x.all, pcamodel_ref, obstag = tag.all)

