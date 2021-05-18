# TODO: Define this somewhere else.

setClass("RandomSampling",
         representation(
             method="character",
             x="matrix",
             y="numeric",
             features_sampled="integer",
             observations_sampled="integer",
             feature_weight="ANY"
         )
)

setClass("Standardization",
         representation(
             method="character",
             x="matrix",
             y="numeric",
             y_sd="numeric"
         )
)

setClass("Regression",
         representation(
             coef="numeric"
         )
)

setOldClass("glmnet")
setOldClass("cv.glmnet")
setOldClass("elnet")

setClass("Lasso",
         representation(
             cv_glmnet="cv.glmnet",
             glmnet="elnet"
         ),
         contains="Regression"
)

setClass("Bootstrap",
         representation(
             method="character",
             sample_coef="numeric",
             random_sampling="RandomSampling",
             standardization="Standardization",
             regression="ANY"
         )
)

setClass("RandomLassoPart",
         representation(
             coef="numeric",
             bootstrap_matrix="matrix",
             bootstraps="list"
         ),
         prototype = list(
             coef = NA_real_,
             bootstrap_matrix=matrix()
         )
)

setClass("RandomLasso",
         representation(
             coef="numeric",
             part1="RandomLassoPart",
             part2="RandomLassoPart"
         )
)
