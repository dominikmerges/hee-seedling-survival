
source('format_data.R')

#Analyses Ideas

#1. Seedling survival
#     Random Covariates:
#     - Site (n=15)
#     - Plot (n=54)
#     Fixed covariates:
#     - Distance to edge (?), how to deal with shelterwoods?
#     - Species
#     - Source (0-0 or 1-0)
#     - Initial height (z-score separated by source)
#     - Canopy cover
#     - Competition (stem density?)
#     - Browse at time t-1
#     - Time since establishment
#
#2. Seedling growth (height)
#     Random Covariates:
#     - Site, plot
#     Fixed Covariates:
#     - Canopy cover
#     - Browse at time t-1
#     - Height at time t-1
#     - Competition
#     - Species
#     - Source
#
#3. Browse intensity (how to calculate?)
#     Random Covariates:
#     - Site, plot
#     Fixed Covariates:
#     - Distance to edge
#     - Treatment?
#     - Plant density
#     - Use (pellet counts)
#     - Age of opening





seedling <- format.seedling('seedlingmaster.csv')

surv <- seedling$surv.sprout
