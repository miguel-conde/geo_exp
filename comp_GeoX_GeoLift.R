library(dplyr)
library(GeoLift)
library(GeoexperimentsResearch)

# 1. GeoX data with GeoLift -----------------------------------------------


# 1.1 - Data --------------------------------------------------------------


data(salesandcost)

head(salesandcost)

pretest_data <- salesandcost %>% 
  filter(date >= as.Date("2015-01-05"),
         date <= as.Date("2015-02-15")) # 42 days
test_data <- salesandcost %>% 
  filter(# date > as.Date("2015-02-15"),
         date <= as.Date("2015-03-15 ")) # 42 + 28 days
         
         

GeoTestData_PreTest <- GeoDataRead(data = pretest_data,
                                   date_id = "date",
                                   location_id = "geo",
                                   Y_id = "sales",
                                   X = c(), #empty list as we have no covariates
                                   format = "yyyy-mm-dd",
                                   summary = TRUE)

head(GeoTestData_PreTest)

GeoPlot(GeoTestData_PreTest,
        Y_id = "Y",
        time_id = "time",
        location_id = "location")

# 1.2 Power Analysis ------------------------------------------------------

MarketSelections <- GeoLiftMarketSelection(data = GeoTestData_PreTest,
                                           treatment_periods = c(10,15, 20, 25, 30),
                                           N = c(5,15,25,35),
                                           Y_id = "Y",
                                           location_id = "location",
                                           time_id = "time",
                                           effect_size = seq(0, 0.5, 0.05),
                                           lookback_window = 1,
                                           # include_markets = c("chicago"),
                                           # exclude_markets = c("honolulu"),
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
plot(MarketSelections, market_ID = market_id, print_summary = TRUE)

weights <- GetWeights(Y_id = "Y",
                      location_id = "location",
                      time_id = "time",
                      data = GeoTestData_PreTest,
                      locations = c("13", "16", "4", "5", "53"),
                      pretreatment_end_time = 42,
                      fixed_effects = TRUE)
#> One outcome and one treatment time found. Running single_augsynth.

# Top weights
head(dplyr::arrange(weights, desc(weight)))


# 1.3 Analyzing the Test Results -------------------------------------------

GeoTestData_Test <- GeoDataRead(data = test_data,
                                date_id = "date",
                                location_id = "geo",
                                Y_id = "sales",
                                X = c(), #empty list as we have no covariates
                                format = "yyyy-mm-dd",
                                summary = TRUE)

GeoPlot(GeoTestData_Test,
        Y_id = "Y",
        time_id = "time",
        location_id = "location",
        treatment_start = 43)


# GeoLift Inference -------------------------------------------------------

GeoTest <- GeoLift(Y_id = "Y",
                   data = GeoTestData_Test,
                   locations = c("13", "16", "4", "5", "53"),
                   treatment_start_time = 43,
                   treatment_end_time = 52)

GeoTest

summary(GeoTest)

plot(GeoTest, type = "Lift")
#> You can include dates in your chart if you supply the end date of the treatment. Just specify the treatment_end_date parameter.

plot(GeoTest, type = "ATT")
#> You can include dates in your chart if you supply the end date of the treatment. Just specify the treatment_end_date parameter.


# Improving The Model -----------------------------------------------------

GeoTestBest <- GeoLift(Y_id = "Y",
                       data = GeoTestData_Test,
                       locations = c("13", "16", "4", "5", "53"),
                       treatment_start_time = 43,
                       treatment_end_time = 52,
                       model = "best")

summary(GeoTestBest)

plot(GeoTestBest, type = "Lift")
#> You can include dates in your chart if you supply the end date of the treatment. Just specify the treatment_end_date parameter.

plot(GeoTestBest, type = "ATT")
#> You can include dates in your chart if you supply the end date of the treatment. Just specify the treatment_end_date parameter.


# 2. GeoLift data with GeoX -----------------------------------------------


# 2.1 - Data --------------------------------------------------------------

data(GeoLift_PreTest)
data(GeoLift_Test) # Este incluye al anterior

GeoLift_PreTest

GeoLift_PreTest$date %>% range()
GeoLift_Test$date %>% range()

head(GeoLift_Test)

obj.gts <- GeoTimeseries(GeoLift_Test %>% 
                           rename(geo = location) %>% 
                           mutate(cost = ifelse(date < as.Date("2021-04-01"), 0, 100)), 
                         metrics=c("Y"))

# EDA ---------------------------------------------------------------------

aggregate(obj.gts, by='.weekindex')

plot(obj.gts)



# Experiment Periods ------------------------------------------------------

obj.per <- ExperimentPeriods(c("2021-01-01", "2021-04-01", "2021-04-15"))
obj.per

# Geo Assignment ----------------------------------------------------------

# data(geoassignment)
# head(geoassignment)
# 
# obj.ga <- GeoAssignment(geoassignment)
# head(obj.ga)

obj.geo.strata <- ExtractGeoStrata(obj.gts, volume="Y", n.groups=2)
obj.geo.assignment <- Randomize(obj.geo.strata)
head(obj.geo.assignment)

# Combining all information about the experiment into one object ----------

obj <- GeoExperimentData(obj.gts,
                         periods=obj.per,
                         geo.assignment=obj.geo.assignment)
head(obj)


# Exploratory data analysis -----------------------------------------------

aggregate(obj, by=c('period', 'geo.group'))

# Geo-Based Regression (GBR) Analysis -------------------------------------

result <- DoGBRROASAnalysis(obj, response='Y', cost='cost',
                            pretest.period=0,
                            intervention.period=1,
                            cooldown.period=NULL,
                            control.group=1,
                            treatment.group=2)
result

summary(result, level=0.95, interval.type="two-sided")

summary(result, threshold=3.0)

# Time-Based Regression (TBR) ROAS Analysis -------------------------------

obj.tbr.roas <- DoTBRROASAnalysis(obj, response='Y', cost='cost',
                                  model='tbr1',
                                  pretest.period=0,
                                  intervention.period=1,
                                  cooldown.period=NULL,
                                  control.group=1,
                                  treatment.group=2)
obj.tbr.roas

summary(obj.tbr.roas, level=0.95, interval.type="two-sided")

summary(obj.tbr.roas, threshold=3.0)

plot(obj.tbr.roas)


# Time-Based Regression (TBR) Causal Effect Analysis ----------------------

obj.tbr <- DoTBRAnalysis(obj, response='Y',
                         model='tbr1',
                         pretest.period=0,
                         intervention.period=1,
                         cooldown.period=NULL,
                         control.group=1,
                         treatment.group=2)

summary(obj.tbr)

plot(obj.tbr)

# Preanalysis -------------------------------------------------------------


# A randomized geo assignment ---------------------------------------------

# obj.geo.strata <- ExtractGeoStrata(obj.gts, volume="sales", n.groups=2)
# head(obj.geo.strata)
# 
# obj.geo.assignment <- Randomize(obj.geo.strata)
# head(obj.geo.assignment)


# Predicting the precision ------------------------------------------------

obj.pre <- DoROASPreanalysis(obj.gts, response="Y",
                             geos=obj.geo.assignment,
                             prop.to="Y",
                             period.lengths=c(42, 21, 7))

results <- summary(obj.pre,
                   level=0.90,
                   type="one-sided",
                   precision=1.0)
print(results)

print(obj.pre)

results2 <- summary(obj.pre,
                    level=0.90,
                    type="one-sided",
                    cost=10000)
print(results2)
