################################################################################
library(dplyr)
library(Strategus)

# ---------------------- Time-at-Risk 설정 -------------------------------------
timeAtRisks <- tibble(
  label = c(
    "365d from index",
    "5y from index",
    "On treatment",
    "29-365d from index"
  ),
  riskWindowStart  = c(1, 1, 1, 29),
  startAnchor = c("cohort start", "cohort start", "cohort start", "cohort start"),
  riskWindowEnd  = c(365, 1825, 0, 365),
  endAnchor = c("cohort start", "cohort start", "cohort end", "cohort start")
)

# --------------------- Study period 두 개의 세트 ------------------------------
studyPeriods <- tibble(
  studyStartDate = c('20111101', '20130301'),
  studyEndDate   = c('20190331', '20161231')
)

# --------------------- Cohort 정의 --------------------------------------------
baseUrl <- "https://atlas-demo.ohdsi.org/WebAPI"
cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = c(
    1788314, # Target: Ticagrelor
    1788315, # Comparator: Clopidogrel
    1788319  # Outcome: NACE
  ),
  generateStats = TRUE
)

# 내부 cohortId를 1, 2, 3으로 통일
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1788314,]$cohortId <- 1
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1788315,]$cohortId <- 2
cohortDefinitionSet[cohortDefinitionSet$cohortId == 1788319,]$cohortId <- 3

# -------------------- Negative Control Outcomes -------------------------------
negativeControlOutcomeCohortSet <- ROhdsiWebApi::getConceptSetDefinition(
  conceptSetId = 1882685,
  baseUrl = baseUrl
) %>%
  ROhdsiWebApi::resolveConceptSet(baseUrl = baseUrl) %>%
  ROhdsiWebApi::getConcepts(baseUrl = baseUrl) %>%
  rename(outcomeConceptId = "conceptId", cohortName = "conceptName") %>%
  mutate(cohortId = row_number() + 100) %>%
  select(cohortId, cohortName, outcomeConceptId)

if (any(duplicated(c(cohortDefinitionSet$cohortId, negativeControlOutcomeCohortSet$cohortId)))) {
  stop("*** Error: duplicate cohort IDs found ***")
}

# -------------------- Outcome 리스트 ------------------------------------------
oList <- cohortDefinitionSet %>%
  filter(cohortId == 3) %>%
  mutate(outcomeCohortId = cohortId, outcomeCohortName = cohortName) %>%
  select(outcomeCohortId, outcomeCohortName) %>%
  mutate(cleanWindow = 365)

# -------------------- Target-Comparator 매칭 ---------------------------------
cmTcList <- data.frame(
  targetCohortId = 1,
  targetCohortName = "Ticagrelor",
  comparatorCohortId = 2,
  comparatorCohortName = "Clopidogrel"
)

# -------------------- 제외할 covariate 개념 -----------------------------------
excludedCovariateConcepts <- data.frame(
  conceptId = c(40241186, 1322184),
  conceptName = c("Ticagrelor", "Clopidogrel")
)

# --------------------- CohortGeneratorModule 설정 -----------------------------
cgModuleSettingsCreator <- CohortGeneratorModule$new()
cohortDefinitionShared <- cgModuleSettingsCreator$createCohortSharedResourceSpecifications(cohortDefinitionSet)
negativeControlsShared <- cgModuleSettingsCreator$createNegativeControlOutcomeCohortSharedResourceSpecifications(
  negativeControlOutcomeCohortSet = negativeControlOutcomeCohortSet,
  occurrenceType = "first",
  detectOnDescendants = TRUE
)
cohortGeneratorModuleSpecifications <- cgModuleSettingsCreator$createModuleSpecifications(
  generateStats = TRUE
)

# ------------------ CohortMethodModule (분석 조합 모두 생성) -------------------

# PS 세팅 조합
psArgsList <- list(
  list(psAdjustment = "match", ratio = 1),
  list(psAdjustment = "match", ratio = 10),
  list(psAdjustment = "stratify", ratio = 10)
)

# Outcome Model Args (고정: cox)
outcomeModelType <- "cox"

# 분석을 위한 리스트를 만듭니다
cmAnalysisList <- list()
analysisId <- 1

# 모든 조합을 순회
for (s in seq_len(nrow(studyPeriods))) {
  studyStartDate <- studyPeriods$studyStartDate[s]
  studyEndDate <- studyPeriods$studyEndDate[s]
  for (t in seq_len(nrow(timeAtRisks))) {
    for (p in seq_along(psArgsList)) {
      psSpec <- psArgsList[[p]]
      
      # PS matching or stratification 설정
      if (psSpec$psAdjustment == "match") {
        psMatchMaxRatio <- psSpec$ratio
        matchOnPsArgs <- CohortMethod::createMatchOnPsArgs(
          maxRatio = psMatchMaxRatio,
          caliper = 0.2,
          caliperScale = "standardized logit",
          allowReverseMatch = FALSE,
          stratificationColumns = c()
        )
        stratifyByPsArgs <- NULL
      } else if (psSpec$psAdjustment == "stratify") {
        matchOnPsArgs <- NULL
        stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(
          numberOfStrata = psSpec$ratio,
          stratificationColumns = c(),
          baseSelection = "all"
        )
      }
      
      covariateSettings <- FeatureExtraction::createDefaultCovariateSettings(
        addDescendantsToExclude = TRUE
      )
      
      # outcomeList 생성
      outcomeList <- append(
        lapply(seq_len(nrow(oList)), function(i) {
          CohortMethod::createOutcome(
            outcomeId = oList$outcomeCohortId[i],
            outcomeOfInterest = TRUE,
            trueEffectSize = NA,
            priorOutcomeLookback = 99999
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
      # Target-Comparator-Outcome 세트
      targetComparatorOutcomesList <- list(
        CohortMethod::createTargetComparatorOutcomes(
          targetId = cmTcList$targetCohortId,
          comparatorId = cmTcList$comparatorCohortId,
          outcomes = outcomeList,
          excludedCovariateConceptIds = excludedCovariateConcepts$conceptId
        )
      )
      
      # 데이터 argument
      getDbCohortMethodDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(
        restrictToCommonPeriod = TRUE,
        studyStartDate = studyStartDate,
        studyEndDate = studyEndDate,
        maxCohortSize = 0,
        covariateSettings = covariateSettings
      )
      
      createPsArgs = CohortMethod::createCreatePsArgs(
        maxCohortSizeForFitting = 250000,
        errorOnHighCorrelation = TRUE,
        stopOnError = FALSE,
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
      
      computeSharedCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = NULL
      )
      computeCovariateBalanceArgs = CohortMethod::createComputeCovariateBalanceArgs(
        maxCohortSize = 250000,
        covariateFilter = FeatureExtraction::getDefaultTable1Specifications()
      )
      
      fitOutcomeModelArgs = CohortMethod::createFitOutcomeModelArgs(
        modelType = outcomeModelType,
        stratified = !is.null(stratifyByPsArgs),
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
      
      createStudyPopArgs <- CohortMethod::createCreateStudyPopulationArgs(
        firstExposureOnly = FALSE,
        washoutPeriod = 0,
        removeDuplicateSubjects = "keep first",
        censorAtNewRiskWindow = TRUE,
        removeSubjectsWithPriorOutcome = TRUE,
        priorOutcomeLookback = 99999,
        riskWindowStart = timeAtRisks$riskWindowStart[t],
        startAnchor = timeAtRisks$startAnchor[t],
        riskWindowEnd = timeAtRisks$riskWindowEnd[t],
        endAnchor = timeAtRisks$endAnchor[t],
        minDaysAtRisk = 1,
        maxDaysAtRisk = 99999
      )
      
      # 실제 분석 조합 등록
      cmAnalysisList[[analysisId]] <- CohortMethod::createCmAnalysis(
        analysisId = analysisId,
        description = sprintf(
          "TAR: %s; PS: %s-%d; Study: %s-%s",
          timeAtRisks$label[t],
          psSpec$psAdjustment,
          psSpec$ratio,
          studyStartDate,
          studyEndDate
        ),
        getDbCohortMethodDataArgs = getDbCohortMethodDataArgs,
        createStudyPopArgs = createStudyPopArgs,
        createPsArgs = createPsArgs,
        matchOnPsArgs = matchOnPsArgs,
        stratifyByPsArgs = stratifyByPsArgs,
        computeSharedCovariateBalanceArgs = computeSharedCovariateBalanceArgs,
        computeCovariateBalanceArgs = computeCovariateBalanceArgs,
        fitOutcomeModelArgs = fitOutcomeModelArgs
      )
      analysisId <- analysisId + 1
    }
  }
}

# CohortMethodModule에 세팅
cmModuleSettingsCreator <- CohortMethodModule$new()
cohortMethodModuleSpecifications <- cmModuleSettingsCreator$createModuleSpecifications(
  cmAnalysisList = cmAnalysisList,
  targetComparatorOutcomesList = targetComparatorOutcomesList,
  analysesToExclude = NULL,
  refitPsForEveryOutcome = FALSE,
  refitPsForEveryStudyPopulation = FALSE,  
  cmDiagnosticThresholds = CohortMethod::createCmDiagnosticThresholds()
)

# -------------------- Analysis Specification 전체 조립 ------------------------
analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() |>
  Strategus::addSharedResources(cohortDefinitionShared) |> 
  Strategus::addSharedResources(negativeControlsShared) |>
  Strategus::addModuleSpecifications(cohortGeneratorModuleSpecifications) |>
  Strategus::addModuleSpecifications(cohortMethodModuleSpecifications)

ParallelLogger::saveSettingsToJson(
  analysisSpecifications, 
  file.path("inst", "TicagrelorVsClopidogrel", "TicagrelorVsClopidogrelAnalysisSpecification.json")
)
