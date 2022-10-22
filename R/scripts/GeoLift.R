## renv::install("ebenmichael/augsynth")
## renv::install("facebookincubator/GeoLift")

library(GeoLift)

# GeoLift Walkthrough
# https://facebookincubator.github.io/GeoLift/docs/GettingStarted/Walkthrough

# 1. Data -----------------------------------------------------------------

data(GeoLift_PreTest)

GeoTestData_PreTest <- GeoDataRead(data = GeoLift_PreTest,
                                   date_id = "date",
                                   location_id = "location",
                                   Y_id = "Y",
                                   X = c(), #empty list as we have no covariates
                                   format = "yyyy-mm-dd",
                                   summary = TRUE)

head(GeoTestData_PreTest)

GeoPlot(GeoTestData_PreTest,
        Y_id = "Y",
        time_id = "time",
        location_id = "location")


# 2. Power Analysis -------------------------------------------------------

MarketSelections <- GeoLiftMarketSelection(data = GeoTestData_PreTest,
                                           treatment_periods = c(10,15),
                                           N = c(2,3,4,5),
                                           Y_id = "Y",
                                           location_id = "location",
                                           time_id = "time",
                                           effect_size = seq(0, 0.5, 0.05),
                                           lookback_window = 1,
                                           include_markets = c("chicago"),
                                           exclude_markets = c("honolulu"),
                                           holdout = c(0.5, 1),
                                           cpic = 7.50,
                                           budget = 100000,
                                           alpha = 0.1,
                                           Correlations = TRUE,
                                           fixed_effects = TRUE,
                                           side_of_test = "two_sided")

# Plot for chicago, cincinnati, houston, portland for a 15 day test
plot(MarketSelections, market_ID = 1, print_summary = FALSE)

# Plot for chicago, portland for a 15 day test
plot(MarketSelections, market_ID = 2, print_summary = FALSE)


# Power output - deep dive into power curves ------------------------------

market_id = 2
market_row <- MarketSelections$BestMarkets %>% dplyr::filter(ID == market_id)
treatment_locations <- stringr::str_split(market_row$location, ", ")[[1]]
treatment_duration <- market_row$duration
lookback_window <- 7

power_data <- GeoLiftPower(
  data = GeoTestData_PreTest,
  locations = treatment_locations,
  effect_size = seq(-0.25, 0.25, 0.01),
  lookback_window = lookback_window,
  treatment_periods = treatment_duration,
  cpic = 7.5,
  side_of_test = "two_sided"
)


plot(power_data, show_mde = TRUE, smoothed_values = FALSE, breaks_x_axis = 5) 
# + labs(caption = unique(power_data$location))

# Plot for chicago, portland for a 15 day test
plot(MarketSelections, market_ID = 2, print_summary = TRUE)

weights <- GetWeights(Y_id = "Y",
                      location_id = "location",
                      time_id = "time",
                      data = GeoTestData_PreTest,
                      locations = c("chicago", "portland"),
                      pretreatment_end_time = 90,
                      fixed_effects = TRUE)
#> One outcome and one treatment time found. Running single_augsynth.

# Top weights
head(dplyr::arrange(weights, desc(weight)))


# 3. Analyzing the Test Results -------------------------------------------



# Test Data ---------------------------------------------------------------

data(GeoLift_Test)

GeoTestData_Test <- GeoDataRead(data = GeoLift_Test,
                                date_id = "date",
                                location_id = "location",
                                Y_id = "Y",
                                X = c(), #empty list as we have no covariates
                                format = "yyyy-mm-dd",
                                summary = TRUE)

GeoPlot(GeoTestData_Test,
        Y_id = "Y",
        time_id = "time",
        location_id = "location",
        treatment_start = 91)


# GeoLift Inference -------------------------------------------------------

GeoTest <- GeoLift(Y_id = "Y",
                   data = GeoTestData_Test,
                   locations = c("chicago", "portland"),
                   treatment_start_time = 91,
                   treatment_end_time = 105)

GeoTest

summary(GeoTest)

plot(GeoTest, type = "Lift")
#> You can include dates in your chart if you supply the end date of the treatment. Just specify the treatment_end_date parameter.

plot(GeoTest, type = "ATT")
#> You can include dates in your chart if you supply the end date of the treatment. Just specify the treatment_end_date parameter.


# Improving The Model -----------------------------------------------------

GeoTestBest <- GeoLift(Y_id = "Y",
                       data = GeoTestData_Test,
                       locations = c("chicago", "portland"),
                       treatment_start_time = 91,
                       treatment_end_time = 105,
                       model = "best")

summary(GeoTestBest)

plot(GeoTestBest, type = "Lift")
#> You can include dates in your chart if you supply the end date of the treatment. Just specify the treatment_end_date parameter.

plot(GeoTestBest, type = "ATT")
#> You can include dates in your chart if you supply the end date of the treatment. Just specify the treatment_end_date parameter.
