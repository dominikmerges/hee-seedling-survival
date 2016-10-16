seedling-survival
=================

Comparison of white and black oak seedling growth and survival under multiple harvest treatments (clearcut interior, clearcut edge, shelterwood, and unharvested control). Hierarchical models fit in a Bayesian framework were used for both growth and survival analyses.

The results are published in the following article:

[*Kellner, Kenneth F., and Robert K. Swihart. 2016. Timber harvest and drought interact to impact oak seedling growth and survival in the Central Hardwood Forest. Ecosphere 7: e01473.*](http://dx.doi.org/10.1002/ecs2.1473)

Metadata
========

The *analysis_seedling_growth.R* and *analysis_seedling_survival.R* files contain the Bayesian analyses of seedling growth and survival, respectively.

The *analysis_light.R* file contains an analysis of differences in canopy cover/light availability along a transect across a clearcut edge.

The *function_format_data.R* file contains a function for parsing the raw growth/survival and covariate data into more useful formats.

The *models* folder contains the model files (in the BUGS language) for the analyses.

The *figures* folder contains code generating the figures in the final manuscript.

The *data* subfolder contains example CSV files representative of the actual data used in this study. I do not currently have permission to publish the complete raw data.