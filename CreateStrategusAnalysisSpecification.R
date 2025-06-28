################################################################################
# INSTRUCTIONS: 



# ##############################################################################
library(dplyr)
library(Strategus)

# Time-at-risks (TARs) for the outcomes of interest in your study
timeAtRisks <- tibble(
  label = c("1-year fixed", 
            "1-year fixed with blanking", 
            "On treatment", 
            "On treatment with blanking", 
            "5-year fixed", 
            "5-year fixed with blanking"),
  riskWindowStart = c(1, 29, 1, 29, 1, 29),
  startAnchor = "cohort start", # 모든 행의 값이 동일
  riskWindowEnd = c(365, 365, 0, 0, 1825, 1825),
  endAnchor = c("cohort start", 
                "cohort start", 
                "cohort end", 
                "cohort end", 
                "cohort start", 
                "cohort start")
)



# Consider these settings for estimation  ----------------------------------------

useCleanWindowForPriorOutcomeLookback <- FALSE # If FALSE, lookback window is all time prior, i.e., including only first events
psMatchMaxRatio <- 1 # If bigger than 1, the outcome model will be conditioned on the matched set

# Shared Resources -------------------------------------------------------------
# Get the list of cohorts 

baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"

# Cohort Definitions
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1788314, # Ticagrelor
    1788315, # Clopidogrel
    1788319 # NACE
  ),
  generateStats = TRUE
)

# Re-number cohorts
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1788314,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1788315,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1788319,]$cohortId <- 3

# Negative control outcomes
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1882685,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(
    baseUrl = baseUrl
  ) %>%
  ROhdsiWebApi::getConcepts(
    baseUrl = baseUrl
  ) %>%
  rename(outcomeConceptId = "conceptId",
         cohortName = "conceptName") %>%
  mutate(cohortId = row_number() + 100) %>% # target/comparator cohort ids start with 1, 2, 3... negativeControl -> 101, 102, 103...
  select(cohortId, cohortName, outcomeConceptId)


# # OPTIONAL: Create a subset to define the new user cohorts
# # More information: https://ohdsi.github.io/CohortGenerator/articles/CreatingCohortSubsetDefinitions.html
# subset1 <- CohortGenerator::createCohortSubsetDefinition(
#   name = "New Users",
#   definitionId = 1,
#   subsetOperators = list(
#     CohortGenerator::createLimitSubset(
#       priorTime = 365,
#       limitTo = "firstEver"
#     )
#   )
# )
# 
# cohortDefinitionSet <- cohortDefinitionSet |>
#   CohortGenerator::addCohortSubsetDefinition(subset1, targetCohortIds = c(1,2))


if (any(duplicated(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# Create some data frames to hold the cohorts we'll use in each analysis ---------------
# Outcomes: The outcome for this study is cohort_id == 3 
oList <- cohortDefinitionSet %>%
  filter(.data$cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) # %>%
  # mutate(cleanWindow = 365)

# # For the CohortMethod analysis we'll use the subsetted cohorts
# cmTcList <- data.frame(
#   targetCohortId = 1001,
#   targetCohortName = "celecoxib new users",
#   comparatorCohortId = 2001,
#   comparatorCohortName = "diclofenac new users"
# )

# target&comparator
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "ticagrelor",
  comparatorCohortId = 2,
  comparatorCohortName = "clopidogrel"
)

# For the CohortMethod LSPS we'll need to exclude the drugs of interest in this
# study
excludedCovariateConcepts <- data.frame(
  conceptId = c(40241186, 1322184),
  conceptName = c("ticagrelor", "clopidogrel")
)
# 
# # For the SCCS analysis we'll use the all exposure cohorts
# sccsTList <- data.frame(
#   targetCohortId = c(1,2),
#   targetCohortName = c("celecoxib", "diclofenac")
# )

# CohortGeneratorModule --------------------------------------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "ALL",
  detectOnDescendants = TRUE
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# CohortDiagnoticsModule Settings ---------------------------------------------
cdModuleSettingsCreator <- CohortDiagnosticsModule$new()
cohortDiagnosticsModuleSpecifications <- cdModuleSettingsCreator$createModuleSpecifications(
  cohortIds = cohortDefinitionSet$cohortId,
  runInclusionStatistics = TRUE,
  runIncludedSourceConcepts = TRUE,
  runOrphanConcepts = TRUE,
  runTimeSeries = FALSE,
  runVisitContext = TRUE,
  runBreakdownIndexEvents = TRUE,
  runIncidenceRate = TRUE,
  runCohortRelationship = TRUE,
  runTemporalCohortCharacterization = TRUE,
  minCharacterizationMean = 0.01
)

# CharacterizationModule Settings ---------------------------------------------
cModuleSettingsCreator <- CharacterizationModule$new()
characterizationModuleSpecifications <- cModuleSettingsCreator$createModuleSpecifications(
  targetIds = cohortDefinitionSet$cohortId, # NOTE: This is all T/C/I/O
  outcomeIds = oList$outcomeCohortId,
  minPriorObservation = 365,
  dechallengeStopInterval = 30,
  dechallengeEvaluationWindow = 30,
  riskWindowStart = timeAtRisks$riskWindowStart, 
  startAnchor = timeAtRisks$startAnchor, 
  riskWindowEnd = timeAtRisks$riskWindowEnd, 
  endAnchor = timeAtRisks$endAnchor,
  minCharacterizationMean = .01
)


# # CohortIncidenceModule --------------------------------------------------------
# ciModuleSettingsCreator <- CohortIncidenceModule$new()
# tcIds <- cohortDefinitionSet %>%
#   filter(!cohortId %in% oList$outcomeCohortId & isSubset) %>%
#   pull(cohortId)
# targetList <- lapply(
#   tcIds,
#   function(cohortId) {
#     CohortIncidence::createCohortRef(
#       id = cohortId, 
#       name = cohortDefinitionSet$cohortName[cohortDefinitionSet$cohortId == cohortId]
#     )
#   }
# )
# outcomeList <- lapply(
#   seq_len(nrow(oList)),
#   function(i) {
#     CohortIncidence::createOutcomeDef(
#       id = i, 
#       name = cohortDefinitionSet$cohortName[cohortDefinitionSet$cohortId == oList$outcomeCohortId[i]], 
#       cohortId = oList$outcomeCohortId[i], 
#       cleanWindow = oList$cleanWindow[i]
#     )
#   }
# )
# 
# tars <- list()
# for (i in seq_len(nrow(timeAtRisks))) {
#   tars[[i]] <- CohortIncidence::createTimeAtRiskDef(
#     id = i, 
#     startWith = gsub("cohort ", "", timeAtRisks$startAnchor[i]), 
#     endWith = gsub("cohort ", "", timeAtRisks$endAnchor[i]), 
#     startOffset = timeAtRisks$riskWindowStart[i],
#     endOffset = timeAtRisks$riskWindowEnd[i]
#   )
# }
# analysis1 <- CohortIncidence::createIncidenceAnalysis(
#   targets = tcIds,
#   outcomes = seq_len(nrow(oList)),
#   tars = seq_along(tars)
# )
# # irStudyWindow <- CohortIncidence::createDateRange(
# #   startDate = studyStartDateWithHyphens,
# #   endDate = studyEndDateWithHyphens
# # )
# irDesign <- CohortIncidence::createIncidenceDesign(
#   targetDefs = targetList,
#   outcomeDefs = outcomeList,
#   tars = tars,
#   analysisList = list(analysis1),
#   #studyWindow = irStudyWindow,
#   strataSettings = CohortIncidence::createStrataSettings(
#     byYear = TRUE,
#     byGender = TRUE,
#     byAge = TRUE,
#     ageBreaks = seq(0, 110, by = 10)
#   )
# )
# cohortIncidenceModuleSpecifications <- ciModuleSettingsCreator$createModuleSpecifications(
#   irDesign = irDesign$toList()
# )


# CohortMethodModule -----------------------------------------------------------
cmModuleSettingsCreator <- CohortMethodModule$new()
covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
  addDescendantsToExclude = TRUE # Keep TRUE because you're excluding concepts
)
outcomeList <- append(
  lapply(seq_len(nrow(oList)), function(i) {
    if (useCleanWindowForPriorOutcomeLookback)
      priorOutcomeLookback <- oList$cleanWindow[i]
    else
      priorOutcomeLookback <- 99999
    CohortMethod::createOutcome(
      outcomeId = oList$outcomeCohortId[i],
      outcomeOfInterest = TRUE,
      trueEffectSize = NA,
      priorOutcomeLookback = priorOutcomeLookback
    )
  }),
  lapply(negativeControlOutcomeCohortSet$cohortId, function(i) {
    CohortMethod::createOutcome(
      outcomeId = i,
      outcomeOfInterest = FALSE,
      trueEffectSize = 1
    )
  })
)
targetComparatorOutcomesList <- list()
for (i in seq_len(nrow(cmTcList))) {
  targetComparatorOutcomesList[[i]] <- CohortMethod::createTargetComparatorOutcomes(
    targetId = cmTcList$targetCohortId[i],
    comparatorId = cmTcList$comparatorCohortId[i],
    outcomes = outcomeList,
    excludedCovariateConceptIds = c(
      cmTcList$targetConceptId[i], 
      cmTcList$comparatorConceptId[i],
      excludedCovariateConcepts$conceptId
    )
  )
}
getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
  restrictToCommonPeriod = TRUE,
  studyStartDate = "",
  studyEndDate = "",
  maxCohortSize = 0,
  covariateSettings = covariateSettings
)
createPsArgs = CohortMethod::createCreatePsArgs(
  maxCohortSizeForFitting = 250000,
  errorOnHighCorrelation = TRUE,
  stopOnError = FALSE, # Setting to FALSE to allow Strategus complete all CM operations; when we cannot fit a model, the equipoise diagnostic should fail
  estimator = "att",
  prior = Cyclops::createPrior(
    priorType = "laplace", 
    exclude = c(0), 
    useCrossValidation = TRUE
  ),
  control = Cyclops::createControl(
    noiseLevel = "silent", 
    cvType = "auto", 
    seed = 1, 
    resetCoefficients = TRUE, 
    tolerance = 2e-07, 
    cvRepetitions = 1, 
    startingVariance = 0.01
  )
)
matchOnPsArgs = CohortMethod::createMatchOnPsArgs(
  maxRatio = psMatchMaxRatio,
  caliper = 0.2,
  caliperScale = "standardized logit",
  allowReverseMatch = FALSE,
  stratificationColumns = c()
)
# stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
#   numberOfStrata = 5,
#   stratificationColumns = c(),
#   baseSelection = "all"
# )
computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
  maxCohortSize = 250000,
  covariateFilter = NULL
)
computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
  maxCohortSize = 250000,
  covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
)
fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
  modelType = "cox",
  stratified = psMatchMaxRatio != 1,
  useCovariates = FALSE,
  inversePtWeighting = FALSE,
  prior = Cyclops::createPrior(
    priorType = "laplace", 
    useCrossValidation = TRUE
  ),
  control = Cyclops::createControl(
    cvType = "auto", 
    seed = 1, 
    resetCoefficients = TRUE,
    startingVariance = 0.01, 
    tolerance = 2e-07, 
    cvRepetitions = 1, 
    noiseLevel = "quiet"
  )
)
cmAnalysisList <- list()
for (i in seq_len(nrow(timeAtRisks))) {
  createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
    firstExposureOnly = FALSE,
    washoutPeriod = 0,
    removeDuplicateSubjects = "keep all",
    censorAtNewRiskWindow = TRUE,
    removeSubjectsWithPriorOutcome = TRUE,
    priorOutcomeLookback = 99999,
    riskWindowStart = timeAtRisks$riskWindowStart[[i]],
    startAnchor = timeAtRisks$startAnchor[[i]],
    riskWindowEnd = timeAtRisks$riskWindowEnd[[i]],
    endAnchor = timeAtRisks$endAnchor[[i]],
    minDaysAtRisk = 1,
    maxDaysAtRisk = 99999
  )
  cmAnalysisList[[i]] <- CohortMethod::createCmAnalysis(
    analysisId = i,
    description = sprintf(
      "Cohort method, %s",
      timeAtRisks$label[i]
    ),
    getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
    createStudyPopArgs = createStudyPopArgs,
    createPsArgs = createPsArgs,
    matchOnPsArgs = matchOnPsArgs,
    # stratifyByPsArgs = stratifyByPsArgs,
    computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
    computeCovariateBalanceArgs = computeCovariateBalanceArgs,
    fitOutcomeModelArgs = fitOutcomeModelArgs
  )
}
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)




# Create the analysis specifications ------------------------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortDiagnosticsModuleSpecifications) |>
  Strategus::addModuleSpecifications(characterizationModuleSpecifications) |>
  # Strategus::addModuleSpecifications(cohortIncidenceModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)


ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "sampleStudy", "sampleStudyAnalysisSpecification.json")
)
