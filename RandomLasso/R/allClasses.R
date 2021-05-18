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
             bootstrap_matrix="numeric",
             random_sampling="RandomSampling",
             standardization="Standardization",
             regression="ANY"
         )
)

setClass("RandomLasso",
         representation(
             method="character",
             bootstraps="list"
         )
)
