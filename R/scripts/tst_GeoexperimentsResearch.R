## renv::install("google/GeoexperimentsResearch")

library(GeoexperimentsResearch)

# Data --------------------------------------------------------------------


data(salesandcost)

head(salesandcost)

obj.gts <- GeoTimeseries(salesandcost, metrics=c("sales", "cost"))

head(obj.gts)


# EDA ---------------------------------------------------------------------

aggregate(obj.gts, by='.weekindex')

plot(obj.gts)



# Experiment Periods ------------------------------------------------------

obj.per <- ExperimentPeriods(c("2015-01-05", "2015-02-16", "2015-03-15"))
obj.per

# Geo Assignment ----------------------------------------------------------

data(geoassignment)
head(geoassignment)

obj.ga <- GeoAssignment(geoassignment)
head(obj.ga)

# Combining all information about the experiment into one object ----------

obj <- GeoExperimentData(obj.gts,
                         periods=obj.per,
                         geo.assignment=obj.ga)
head(obj)


# Exploratory data analysis -----------------------------------------------

aggregate(obj, by=c('period', 'geo.group'))

# Geo-Based Regression (GBR) Analysis -------------------------------------

result <- DoGBRROASAnalysis(obj, response='sales', cost='cost',
                            pretest.period=0,
                            intervention.period=1,
                            cooldown.period=NULL,
                            control.group=1,
                            treatment.group=2)
result

summary(result, level=0.95, interval.type="two-sided")

summary(result, threshold=3.0)

# Time-Based Regression (TBR) ROAS Analysis -------------------------------

obj.tbr.roas <- DoTBRROASAnalysis(obj, response='sales', cost='cost',
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

obj.tbr <- DoTBRAnalysis(obj, response='sales',
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

obj.geo.strata <- ExtractGeoStrata(obj.gts, volume="sales", n.groups=2)
head(obj.geo.strata)

obj.geo.assignment <- Randomize(obj.geo.strata)
head(obj.geo.assignment)


# Predicting the precision ------------------------------------------------

obj.pre <- DoROASPreanalysis(obj.gts, response="sales",
                             geos=obj.geo.assignment,
                             prop.to="sales",
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
