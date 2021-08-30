# custom functions

# load NZAVS self-report and gameplay data
ch5_loadData <- function() {
  read_sav("data/NZAVS Longitudinal SPSS Base Dataset.sav",
           col_select = c("Questionnaire.Num",
                          
                          # time variables
                          "egame.TSCORE.T11", "egame.TSCORE.T10", "TSCORE.T11", "TSCORE.T10",
                          
                          # demographic variables
                          "Gender.T10", "Age.T10", "NZDep.2013.T10", "EthnicCats.T10", "Religious.T10",
                          "NZREG.T10", "NZSEI13.T10",
                          
                          # economic games variables
                          "egame.Chk1.T10", "egame.Chk2.T10", "egame.TG1.T10", 
                          "egame.TG2.T10", "egame.PGG.T10", "egame.DG.T10", "egame.cmpTG.T10",
                          "egame.cmpPGG.T10", "egame.cmpDG.T10", "egame.Chk1.T11", "egame.Chk2.T11",
                          "egame.TG1.T11", "egame.TG2.T11", "egame.PGG.T11", "egame.DG.T11", 
                          "egame.cmpTG.T11", "egame.cmpPGG.T11", "egame.cmpDG.T11",
                          starts_with("egame.Secs"), starts_with("egame.Paid."),
                          
                          # political variables
                          "T10.SDO", "T11.SDO", "T10.RWA", "IncomeAttribution.T10",
                          "IncomeAttribution.T11", "Issue.IncomeRedistribution.T10", 
                          "Issue.IncomeRedistribution.T11", "Pol.SupNational.T10",
                          "Pol.SupNational.T11"
           )
  ) %>%
    # remove labels
    zap_label() %>%
    zap_labels() %>%
    # remove timeouts and no pay
    filter(
      egame.Chk1.T10 == 1 &
        egame.Chk2.T10 == 1 &
        egame.Chk1.T11 == 1 &
        egame.Chk2.T11 == 1
    ) %>%
    # "time on games"
    select(-(egame.Secs49.T10:egame.Secs57.T10), -egame.SecsW.T10, -egame.SecsG.T10) %>%
    select(-(egame.Secs49.T11:egame.Secs57.T11), -egame.SecsW.T11, -egame.SecsG.T11) %>%
    mutate(egame.SecsAll.T10 = rowSums(select(., starts_with("egame.Secs") & ends_with(".T10")), na.rm = TRUE),
           egame.SecsAll.T11 = rowSums(select(., starts_with("egame.Secs") & ends_with(".T11")), na.rm = TRUE)) %>%
    # standardise variables and set types
    mutate(egame.TG2.T10                  = egame.TG2.T10 / 150,
           egame.PGG.T10                  = egame.PGG.T10 / 100,
           egame.DG.T10                   = egame.DG.T10  / 100,
           egame.TG2.T11                  = egame.TG2.T11 / 150,
           egame.PGG.T11                  = egame.PGG.T11 / 100,
           egame.DG.T11                   = egame.DG.T11  / 100,
           Age.T10.c                      = as.numeric(scale(Age.T10)),
           NZDep.2013.T10                 = as.numeric(scale(NZDep.2013.T10)),
           NZREG.T10                      = as.numeric(scale(NZREG.T10)),
           NZSEI13.T10                    = as.numeric(scale(NZSEI13.T10)),
           EthnicCats.T10                 = factor(ifelse(EthnicCats.T10 == 1, "Pakeha",
                                                          ifelse(EthnicCats.T10 == 2, "Maori",
                                                                 ifelse(EthnicCats.T10 == 3, "Pacific",
                                                                        ifelse(EthnicCats.T10 == 4, "Asian", EthnicCats.T10)))))
    )
}

# item table
ch5_makeItemTable <- function() {
  tibble(
    Item = 
      c("SDO1", "SDO2", "SDO3", "SDO4 (reversed)", "SDO5 (reversed)", "SDO6 (reversed)", 
        "RWA1", "RWA2", "RWA3", "RWA4 (reversed)", "RWA5 (reversed)", "RWA6 (reversed)",
        "Income redistribution", "Income attribution", "Support for National Party", "Age", 
        "Gender", "Ethnicity", "Education level", "Socio-economic status", "Local deprivation",
        "Religiosity"
      ),
    `Description / Text` = 
      c("It is OK if some groups have more of a chance in life than others",
        "Inferior groups should stay in their place",
        "To get ahead in life, it is sometimes okay to step on other groups",
        "We should have increased social equality",
        "It would be good if groups could be equal",
        "We should do what we can to equalise conditions for different groups",
        "It is always better to trust the judgment of the proper authorities in government and religion than to listen to the noisy rabble-rousers in our society who are trying to create doubt in people's minds",
        "It would be best for everyone if the proper authorities censored magazines so that people could not get their hands on trashy and disgusting material",
        "Our country will be destroyed some day if we do not smash the perversions eating away at our moral fibre and traditional beliefs",
        "People should pay less attention to The Bible and other old traditional forms of religious guidance, and instead develop their own personal standards of what is moral and immoral",
        "Atheists and others who have rebelled against established religions are no doubt every bit as good and virtuous as those who attend church regularly",
        "Some of the best people in our country are those who are challenging our government, criticizing religion, and ignoring the `normal way' things are supposed to be done",
        "Redistributing money and wealth more evenly among a larger percentage of the people in New Zealand through heavy taxes on the rich",
        "If incomes were more equal, people would be less motivated to work hard",
        "Level of support for The National Party",
        "What is your date of birth?",
        "What is your gender? (open-ended)",
        "Which ethnic group do you belong to? (NZ census question)",
        "NZ Reg (0-10 education ordinal rank)",
        "NZSEI13 (NZ Socio-economic index)",
        "Deprivation score 2013 (for Meshblock)",
        "Do you identify with a religion and/or spiritual group?"
      ),
    Wave = c(rep("10 - 11", 6), rep("10", 6), rep("10 - 11", 3), rep("10", 7))
  )
}

# timeline figure
ch5_plotTimeline <- function(d) {
  # plot timeline
  out <-
    d %>%
    select("Questionnaire.Num", contains("TSCORE")) %>%
    rename(EconomicGames.T11 = egame.TSCORE.T11,
           EconomicGames.T10 = egame.TSCORE.T10,
           NZAVS.T11 = TSCORE.T11,
           NZAVS.T10 = TSCORE.T10) %>%
    pivot_longer(-Questionnaire.Num, names_to = "Data", values_to = "Year") %>%
    mutate(Year = as.Date("2009-06-30") + Year) %>%
    drop_na(Year) %>%
    ggplot(aes(y = Data, x = Year)) +
    geom_jitter(size = 0.85, alpha = 0.1) +
    labs(y = NULL)
  return(out)
}

# data imputation with mice
ch5_imputeData <- function(d) {
  # remove some egame variables
  d <- select(d, -starts_with("egame.Secs"), -starts_with("egame.Paid."), -Age.T10)
  # remove QNum from predictor matrix
  init  <- mice(d, maxit = 0)
  predM <- init$predictorMatrix
  predM[, c("Questionnaire.Num")] <- 0
  # impute the data
  out <- mice(d, m = 20, predictorMatrix = predM, seed = 12345)
  # dummy ethnicity columns
  out <- 
    complete(out, "long", include = TRUE) %>%
    dummy_columns("EthnicCats.T10") %>%
    as.mids()
  return(out)
}

# plot imputation model fit
ch5_plotImpModel <- function(dM) {
  vars <- colnames(dM$data %>% select(-Questionnaire.Num, -starts_with("egame."), 
                                      -contains("TSCORE"), -starts_with("EthnicCats")))
  plots <- list()
  for (i in 1:length(vars)) plots[[i]] <- densityplot(dM, as.formula(paste("~", vars[i])))
  out <- cowplot::plot_grid(plotlist = plots, ncol = 5, nrow = 3)
  return(out)
}

# fit cfa 1
ch5_fitCFA1 <- function(d) {
  out <- cfa("coop =~ egame.TG1.T11 + egame.TG2.T11 + egame.PGG.T11 + egame.DG.T11",
             data = d, ordered = "egame.TG1.T11")
  return(out)
}

# fit sem 1
ch5_fitSEM1 <- function(dM) {
  # model specification
  model <- '# measurement model
            coop =~ egame.TG1.T11 + egame.TG2.T11 + egame.PGG.T11 + egame.DG.T11
            # regression
            coop ~ T11.SDO'
  # fit model with multiply imputed data
  out <- sem.mi(model, data = dM, 
                ordered = c("egame.TG1.T11"))
  return(out)
}

# plot coop sdo
ch5_plotCoopSDO <- function(dM) {
  # get "complete" data for plotting
  d <- complete(dM)
  # fit sem model to get factor score
  model <- '# measurement model
            coop =~ egame.TG1.T11 + egame.TG2.T11 + egame.PGG.T11 + egame.DG.T11
            # regression
            coop ~ T11.SDO'
  sem <- sem(model, data = d, ordered = c("egame.TG1.T11"))
  # add coop to dataframe
  pred <- lavPredict(sem)
  d$coop <- pred[,1]
  # plot
  out <-
    ggplot(d, aes(x = T11.SDO, y = coop)) +
    geom_jitter(width = 0.1, height = 0.1, colour = "#0072B2", alpha = 0.5) +
    stat_smooth(method = "lm", se = TRUE, colour = "black") +
    labs(x = "Social Dominance Orientation", y = "Cooperation")
  return(out)
}

# fit measurement invariance model
ch5_fitMI <- function(d, type = "config") {
  # model specification
  if (type == "config") {
    model <- 'coopT11 =~ egame.TG1.T11 + egame.TG2.T11 + egame.PGG.T11 + egame.DG.T11
              coopT10 =~ egame.TG1.T10 + egame.TG2.T10 + egame.PGG.T10 + egame.DG.T10
              # variances and covariances
              coopT11 ~~ coopT10
              coopT10 ~~ coopT10
              coopT11 ~~ coopT11
              egame.TG2.T11 ~~ egame.TG2.T11
              egame.PGG.T11 ~~ egame.PGG.T11
              egame.DG.T11  ~~ egame.DG.T11
              egame.TG2.T10 ~~ egame.TG2.T10
              egame.PGG.T10 ~~ egame.PGG.T10
              egame.DG.T10  ~~ egame.DG.T10
              egame.TG1.T10 ~~ egame.TG1.T11
              egame.TG2.T10 ~~ egame.TG2.T11
              egame.PGG.T10 ~~ egame.PGG.T11
              egame.DG.T10  ~~ egame.DG.T11
              # intercepts and thresholds
              egame.TG1.T11 | t1
              egame.TG2.T11 ~ 1
              egame.PGG.T11 ~ 1
              egame.DG.T11  ~ 1
              egame.TG1.T10 | t1
              egame.TG2.T10 ~ 1
              egame.PGG.T10 ~ 1
              egame.DG.T10  ~ 1' 
  } else if (type == "metric") {
    model <- 'coopT11 =~ egame.TG1.T11 + l2*egame.TG2.T11 + l3*egame.PGG.T11 + l4*egame.DG.T11
              coopT10 =~ egame.TG1.T10 + l2*egame.TG2.T10 + l3*egame.PGG.T10 + l4*egame.DG.T10
              # variances and covariances
              coopT11 ~~ coopT10
              coopT10 ~~ coopT10
              coopT11 ~~ coopT11
              egame.TG2.T11 ~~ egame.TG2.T11
              egame.PGG.T11 ~~ egame.PGG.T11
              egame.DG.T11  ~~ egame.DG.T11
              egame.TG2.T10 ~~ egame.TG2.T10
              egame.PGG.T10 ~~ egame.PGG.T10
              egame.DG.T10  ~~ egame.DG.T10
              egame.TG1.T10 ~~ egame.TG1.T11
              egame.TG2.T10 ~~ egame.TG2.T11
              egame.PGG.T10 ~~ egame.PGG.T11
              egame.DG.T10  ~~ egame.DG.T11
              # intercepts and thresholds
              egame.TG1.T11 | t1
              egame.TG2.T11 ~ 1
              egame.PGG.T11 ~ 1
              egame.DG.T11  ~ 1
              egame.TG1.T10 | t1
              egame.TG2.T10 ~ 1
              egame.PGG.T10 ~ 1
              egame.DG.T10  ~ 1'
  } else if (type == "scalar") {
    model <- 'coopT11 =~ egame.TG1.T11 + l2*egame.TG2.T11 + l3*egame.PGG.T11 + l4*egame.DG.T11
              coopT10 =~ egame.TG1.T10 + l2*egame.TG2.T10 + l3*egame.PGG.T10 + l4*egame.DG.T10
              # variances and covariances
              coopT11 ~~ coopT10
              coopT10 ~~ coopT10
              coopT11 ~~ coopT11
              egame.TG2.T11 ~~ egame.TG2.T11
              egame.PGG.T11 ~~ egame.PGG.T11
              egame.DG.T11  ~~ egame.DG.T11
              egame.TG2.T10 ~~ egame.TG2.T10
              egame.PGG.T10 ~~ egame.PGG.T10
              egame.DG.T10  ~~ egame.DG.T10
              egame.TG1.T10 ~~ egame.TG1.T11
              egame.TG2.T10 ~~ egame.TG2.T11
              egame.PGG.T10 ~~ egame.PGG.T11
              egame.DG.T10  ~~ egame.DG.T11
              # intercepts and thresholds
              egame.TG1.T11 | i1*t1
              egame.TG2.T11 ~ i2*1
              egame.PGG.T11 ~ i3*1
              egame.DG.T11  ~ i4*1
              egame.TG1.T10 | i1*t1
              egame.TG2.T10 ~ i2*1
              egame.PGG.T10 ~ i3*1
              egame.DG.T10  ~ i4*1'
  } else if (type == "strict") {
    model <- 'coopT11 =~ egame.TG1.T11 + l2*egame.TG2.T11 + l3*egame.PGG.T11 + l4*egame.DG.T11
              coopT10 =~ egame.TG1.T10 + l2*egame.TG2.T10 + l3*egame.PGG.T10 + l4*egame.DG.T10
              # variances and covariances
              coopT11 ~~ coopT10
              coopT10 ~~ coopT10
              coopT11 ~~ coopT11
              egame.TG2.T11 ~~ v1*egame.TG2.T11
              egame.PGG.T11 ~~ v2*egame.PGG.T11
              egame.DG.T11  ~~ v3*egame.DG.T11
              egame.TG2.T10 ~~ v1*egame.TG2.T10
              egame.PGG.T10 ~~ v2*egame.PGG.T10
              egame.DG.T10  ~~ v3*egame.DG.T10
              egame.TG1.T10 ~~ egame.TG1.T11
              egame.TG2.T10 ~~ egame.TG2.T11
              egame.PGG.T10 ~~ egame.PGG.T11
              egame.DG.T10  ~~ egame.DG.T11
              # intercepts and thresholds
              egame.TG1.T11 | i1*t1
              egame.TG2.T11 ~ i2*1
              egame.PGG.T11 ~ i3*1
              egame.DG.T11  ~ i4*1
              egame.TG1.T10 | i1*t1
              egame.TG2.T10 ~ i2*1
              egame.PGG.T10 ~ i3*1
              egame.DG.T10  ~ i4*1'
  }
  # fit model
  out <- cfa(model, data = d, ordered = c("egame.TG1.T11", "egame.TG1.T10"))
  return(out)
}

ch5_getComparisonTable <- function(configMI, metricMI, scalarMI, strictMI) {
  getMeasure <- function(measure) {
    c(fitMeasures(configMI)[measure],
      fitMeasures(metricMI)[measure],
      fitMeasures(scalarMI)[measure],
      fitMeasures(strictMI)[measure])
  }
  roundTable <- function(x, digits) {
    ifelse(is.na(x), "-", as.character(format(round(x, digits), trim = T, nsmall = digits)))
  }
  out <- 
    tibble(
      Model            = c("Configural", "Metric", "Scalar", "Strict"),
      `$\\chi^2$`      = getMeasure("chisq"),
      df               = getMeasure("df"),
      CFI              = getMeasure("cfi"),
      RMSEA            = getMeasure("rmsea"),
      SRMR             = getMeasure("srmr")
    ) %>%
    mutate(
      `$\\Delta$CFI`   = c(NA, .$CFI[-1]   - .$CFI[1:3]),
      `$\\Delta$RMSEA` = c(NA, .$RMSEA[-1] - .$RMSEA[1:3]),
      `$\\Delta$SRMR`  = c(NA, .$SRMR[-1]  - .$SRMR[1:3])
    ) %>%
    mutate_at(vars(`$\\chi^2$`),         roundTable, digits = 2) %>%
    mutate_at(vars(df),                  roundTable, digits = 0) %>%
    mutate_at(vars(CFI:`$\\Delta$SRMR`), roundTable, digits = 3)
  return(out)
}

# fit standard cross-lagged panel model for multiply imputed data
ch5_fitCLPM <- function(dM, var1, var2, ordered, controls = "", group = NULL) {
  # model specification
  model <- 
    paste0(
      '# economic game measurement model
       coopT11 =~ egame.TG1.T11 + l2*egame.TG2.T11 + l3*egame.PGG.T11 + l4*egame.DG.T11
       coopT10 =~ egame.TG1.T10 + l2*egame.TG2.T10 + l3*egame.PGG.T10 + l4*egame.DG.T10
       # variances
       coopT10 ~~ coopT10
       coopT11 ~~ coopT11
       egame.TG2.T11 ~~ v1*egame.TG2.T11
       egame.PGG.T11 ~~ v2*egame.PGG.T11
       egame.DG.T11  ~~ v3*egame.DG.T11
       egame.TG2.T10 ~~ v1*egame.TG2.T10
       egame.PGG.T10 ~~ v2*egame.PGG.T10
       egame.DG.T10  ~~ v3*egame.DG.T10
       egame.TG1.T10 ~~ egame.TG1.T11
       egame.TG2.T10 ~~ egame.TG2.T11
       egame.PGG.T10 ~~ egame.PGG.T11
       egame.DG.T10  ~~ egame.DG.T11
       # intercepts
       egame.TG1.T11 | i1*t1
       egame.TG2.T11 ~ i2*1
       egame.PGG.T11 ~ i3*1
       egame.DG.T11  ~ i4*1
       egame.TG1.T10 | i1*t1
       egame.TG2.T10 ~ i2*1
       egame.PGG.T10 ~ i3*1
       egame.DG.T10  ~ i4*1
       # covariances
       coopT11 ~~ ', var2, '\n       ',
      'coopT10 ~~ ', var1, '\n       ',
      '# regressions
       coopT11 ~ ar1*coopT10 + cl1*', var1, '\n       ',
      var2, ' ~ cl2*coopT10 + ar2*', var1, '\n       ',
      '# difference between cross-lagged paths
       clDiff := cl1 - cl2'
    )
  if (controls != "") {
    model <- 
      paste0(
        model, '\n       ',
        '# control regressions
       coopT11 ~ ', controls, '\n       ',
        var2, ' ~ ', controls, '\n       ',
        '# control covariances
       coopT10 ~~ ', controls, '\n       ',
        var1, ' ~~ ', controls
      )
  }
  # fit model with multiply imputed data
  out <- sem.mi(model, data = dM, ordered = ordered, group = group)
  return(out)
}

# plot clpm model and parameters
ch5_drawCLPMFigure <- function(modelList, var1, var2, semPlotVar, xmin, xmax) {
  # fig A
  # get labelling function
  getLabel <- function(model, LHS, OP, RHS) {
    s <- filter(tibble(summary(model, standardized = TRUE)), lhs == LHS & op == OP & rhs == RHS)
    paste0(format(round(s$std.all, 2), nsmall = 2), ifelse(s$pvalue < 0.05, "*", ""))
  }
  # plot
  pA <-
    data.frame(from = c(1, 1, 2, 2, 3, 4, 1, 2),
               to = c(3, 4, 3, 4, 4, 3, 2, 1), 
               coef = c(
                 getLabel(modelList[[1]], "coopT11", "~",  "coopT10"),
                 getLabel(modelList[[1]], var2,      "~",  "coopT10"),
                 getLabel(modelList[[1]], "coopT11", "~",  var1   ),
                 getLabel(modelList[[1]], var2,      "~",  var1   ),
                 getLabel(modelList[[1]], "coopT11", "~~", var2   ), "",
                 getLabel(modelList[[1]], "coopT10", "~~", var1   ), ""),
               label_pos = c(0.5, 0.6, 0.6, 0.5, 0.5, 0.5, 0.5, 0.5),
               end_cap = c(14, 13, 9, 14, 5, 5, 5, 5)) %>%
    graph_from_data_frame() %>%
    create_layout(layout = "auto") %>%
    mutate(x = c(0, 0, 2, 2),
           y = c(0.75, 0, 0.75, 0),
           name = c("Coop1", paste0(semPlotVar, 1),
                    "Coop2", paste0(semPlotVar, 2))) %>%
    ggraph(layout = "stress") +
    geom_edge_link(aes(label = coef, label_pos = label_pos, end_cap = circle(end_cap, "mm")),
                   angle_calc = 'along',
                   label_dodge = unit(2.5, 'mm'),
                   arrow = arrow(length = unit(4, 'mm'), type = "closed", ends = "last")) +
    geom_rect(aes(xmin = 1.74, xmax = 2.26, ymin = -0.07, ymax = 0.07), 
              fill = "white", colour = "black") +
    geom_rect(aes(xmin = 0.26, xmax = -0.26, ymin = -0.07, ymax = 0.07), 
              fill = "white", colour = "black") +
    geom_ellipse(aes(x0 = 0, y0 = 0.75, a = 0.27, b = 0.07, angle = 0), 
                 fill = "white", colour = "black") +
    geom_ellipse(aes(x0 = 2, y0 = 0.75, a = 0.27, b = 0.07, angle = 0), 
                 fill = "white", colour = "black") +
    geom_node_text(aes(label = name), size = 5)
  # plotting function for fig b and c
  getEstimatePlot <- function(modelList, iv, dv, title) {
    # extracting function
    extractFromModel <- function(model, LHS, OP, RHS, extract) {
      filter((summary(model)), lhs == LHS & op == OP & rhs == RHS)[,extract]
    }
    # extract estimates and CIs
    est <- rep(NA, 10); for (i in 1:10) est[i] <- extractFromModel(modelList[[i]], dv, "~", iv, "est")
    se  <- rep(NA, 10); for (i in 1:10) se[i]  <- extractFromModel(modelList[[i]], dv, "~", iv, "se")
    models <- c("No controls", "Age", "Gender", "Ethnicity", 
                "Education", "SES", "Deprivation",
                "Religious", "RWA", "Full model")
    # plot
    tibble(Model = factor(models, levels = models), Estimate = est, SE = se) %>%
      mutate(Upper = Estimate + 1.96*SE,
             Lower = Estimate - 1.96*SE) %>%
      ggplot(aes(x = Estimate, y = fct_rev(Model), xmin = Lower, xmax = Upper)) +
      geom_vline(linetype = "dashed", xintercept = 0) +
      geom_pointrange() +
      labs(y = NULL, x = "Unstandardised Estimate") +
      scale_x_continuous(limits = c(xmin, xmax)) +
      ggtitle(title)
  }
  # fig B
  pB <- getEstimatePlot(modelList, var1, "coopT11", expr(!!paste0(semPlotVar, "1 ") %->% " Coop2"))
  # fig C
  pC <- getEstimatePlot(modelList, "coopT10", var2, expr("Coop1 " %->% !!paste0(" ", semPlotVar, "2")))
  # put everything together
  top    <- plot_grid(pA, labels = letters[1])
  bottom <- plot_grid(pB, pC, nrow = 1, labels = letters[2:3])
  out <- plot_grid(top, bottom, nrow = 2)
  return(out)
}

ch5_dMfactor <- function(dM, var) {
  # dynamic var
  var <- rlang::sym(var)
  # make factors for multi-group analysis
  dM <- complete(dM, "long", include = TRUE)
  if (var == "EthnicCats.T10") {
    dM <- mutate(dM, EthnicCats.T10 = factor(ifelse(EthnicCats.T10 == "Pakeha", "Pakeha", "Non-Pakeha")))
  } else if (var == "Gender.T10") {
    dM <- mutate(dM, Gender.T10 = factor(ifelse(Gender.T10 == 0, "Female", "Male")))
  }
  # output
  as.mids(dM)
}