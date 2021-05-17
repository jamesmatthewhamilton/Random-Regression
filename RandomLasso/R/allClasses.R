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
