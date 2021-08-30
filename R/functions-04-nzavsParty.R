# custom functions

ch4_loadData <- function() {
  # load data
  read_sav("data/NZAVS Longitudinal SPSS Base Dataset.sav",
           col_select = c(Questionnaire.Num, starts_with("egame.") & ends_with(".T11"),
                          Age.T11, Gender.T11, EthnicCats.T11, NZREG.T11,
                          NZSEI13.T11, NZDepRAW.2013.T11, Religious.T11,
                          Pol.SupNational.T11, Pol.SupLabour.T11,
                          Pol.SupGreens.T11, Pol.SupNZFirst.T11,
                          Pol.SupMaori.T11, Pol.SupACT.T11)) %>%
    # remove all labels
    remove_all_labels() %>%
    # mutate vars
    # "time on games"
    select(-(egame.Secs49.T11:egame.Secs57.T11), -egame.SecsW.T11, -egame.SecsG.T11) %>%
    mutate(egame.SecsAll.T11 = rowSums(select(., starts_with("egame.Secs") & ends_with(".T11")), na.rm = TRUE)) %>%
    # ethnicity
    mutate(EthnicCats.T11 = factor(
      ifelse(EthnicCats.T11 == 1, "Pakeha",
             ifelse(EthnicCats.T11 == 2, "Maori", 
                    ifelse(EthnicCats.T11 == 3, "Pacific",
                           ifelse(EthnicCats.T11 == 4, "Asian", NA)))))) %>%
    # gender
    mutate(Gender.T11 = factor(ifelse(Gender.T11 == 0, "Female", ifelse(Gender.T11 == 1, "Male", NA)))) %>%
    # religious
    mutate(Religious.T11 = factor(ifelse(Religious.T11 == 0, "No", ifelse(Religious.T11 == 1, "Yes", NA)))) %>%
    # centre numeric vars
    mutate(Age.T11.c           = as.numeric(scale(Age.T11)),
           NZREG.T11.c         = NZREG.T11 - 5,
           NZSEI13.T11.c       = as.numeric(scale(NZSEI13.T11)),
           NZDepRAW.2013.T11.c = as.numeric(scale(NZDepRAW.2013.T11)),
           # game data scaled between 0 and 1
           egame.TG2.T11  = egame.TG2.T11  / 150,
           egame.PGG.T11  = egame.PGG.T11  / 100,
           egame.UG1.T11  = egame.UG1.T11  / 100,
           egame.UG2.T11  = egame.UG2.T11  / 100,
           egame.DG.T11   = egame.DG.T11   / 100,
           egame.TPP2.T11 = egame.TPP2.T11 / 100,
           egame.SPP2.T11 = egame.SPP2.T11 / 50,
           egame.SPP3.T11 = egame.SPP3.T11 / 50,
           # comprehension questions: 1 == correct, 0 = incorrect
           egame.cmpTG.T11    = ifelse(egame.cmpTG.T11    == 1, 1, 0),
           egame.cmpPGG.T11   = ifelse(egame.cmpPGG.T11   == 3, 1, 0),
           egame.cmpDG.T11    = ifelse(egame.cmpDG.T11    == 1, 1, 0),
           egame.cmpUG.T11    = ifelse(egame.cmpUG.T11    == 3, 1, 0),
           egame.cmpTPP.T11   = ifelse(egame.cmpTPP.T11   == 1, 1, 0),
           egame.cmpSPP.T11   = ifelse(egame.cmpSPP.T11   == 2, 1, 0),
           egame.cmpBEAST.T11 = ifelse(egame.cmpBEAST.T11 == 1, 1, 0),
           egame.cmpRFT.T11   = ifelse(egame.cmpRFT.T11   == 1, 1, 0)) %>%
    rowwise() %>%
    mutate(beast = mean(c_across(egame.BEASTR1score.T11:egame.BEASTR5score.T11), na.rm = TRUE)) %>%
    ungroup() %>%
    # exclude participants according to criteria
    # keep only participants who were paid and did not time out in wave 2
    filter(egame.Chk1.T11 == 1 & egame.Chk2.T11 == 1) %>%
    # n = 631
    # less than 5 mins or longer than 50 mins to complete games in wave 2
    filter(egame.SecsAll.T11 > 300 & egame.SecsAll.T11 < 3000)
  # n = 631
}

ch4_fitCFA <- function(d) {
  model <- '# measurement model
            coop =~ egame.PGG.T11 + egame.DG.T11 + egame.TG1.T11 + egame.TG2.T11
            # comprehension
            egame.PGG.T11 ~ egame.cmpPGG.T11
            egame.DG.T11 ~ egame.cmpDG.T11
            egame.TG1.T11 + egame.TG2.T11 ~ egame.cmpTG.T11'
  out <- cfa(model, data = d, ordered = c("egame.TG1.T11"))
  return(out)
}

ch4_imputeData <- function(d, cfa) {
  d <-
    d %>%
    select(Questionnaire.Num, Gender.T11, Age.T11.c, EthnicCats.T11,
           Religious.T11, NZREG.T11.c, NZSEI13.T11.c, NZDepRAW.2013.T11.c,
           Pol.SupNational.T11, Pol.SupLabour.T11, Pol.SupACT.T11,
           Pol.SupGreens.T11, Pol.SupMaori.T11, Pol.SupNZFirst.T11, beast) %>%
    mutate(coop = as.numeric(lavPredict(cfa)[,1]))
  # initialise mice
  pred <- mice(d, maxit = 0)$predictorMatrix
  pred[,"Questionnaire.Num"] <- 0 # do not use qnum to predict
  # 10 imputations (average prop missing data across columns = 0.08)
  dM <- mice(d, m = 10, predictorMatrix = pred, seed = 2113)
  return(dM)
}

ch4_fitParty <- function(dM, cfa, predictors, formula) {
  # pivot imputed data longer
  dLong <-  
    complete(dM, "long", include = TRUE) %>%
    pivot_longer(cols = starts_with("Pol.Sup"), names_to = "party", values_to = "support") %>%
    mutate(.id = rep(1:(nrow(dM$data)*6), times = dM$m + 1),
           party = str_remove(str_remove(party, "Pol.Sup"), ".T11"),
           party = ifelse(party == "NZFirst", "NZ First", party)) %>%
    as.mids()
  # priors give each ordinal outcome (1-7) equal prior plausibility
  priors <- c(prior(normal(0, 1.75), class = Intercept), prior(exponential(4), class = sd))
  if (predictors) priors <- c(priors, prior(normal(0, 0.5), class = b))
  # fit model
  out <- brm_multiple(formula, dLong, family = cumulative, 
                      prior = priors, cores = 4, seed = 2113,
                      control = list(adapt_delta = 0.99))
  # get information criteria
  out <- add_criterion(out, c("loo", "waic"))
  return(out)
}

ch4_getPartySlope <- function(post, party, var) {
  post[,paste0(ifelse(var == "moNZREG.T11.c", "bsp_", "b_"), var)] + 
    post[,paste0("r_party[", party, ",", var, "]")] 
}

ch4_fitBEASTmodel <- function(dM, formula) {
  # fit model
  out <- brm_multiple(formula, dM, family = cumulative, 
                      prior = c(prior(normal(0, 1.75), class = Intercept),
                                prior(normal(0, 0.5), class = b)), 
                      cores = 4, seed = 2113,
                      control = list(adapt_delta = 0.99))
  # get information criteria
  out <- add_criterion(out, c("loo", "waic"))
  return(out)
}

ch4_makePartyHist <- function(m0) {
  out <-
    ggplot(m0$data, aes(x = support)) +
    geom_histogram(bins = 7, binwidth = 0.5) +
    facet_wrap(. ~ party, labeller = as_labeller(function(x) ifelse(x == "Maori", sprintf("M\u101ori"), x))) +
    labs(x = "Support for political party (1-7)", y = "Frequency")
  return(out)
}

ch4_makePartyGrid <- function(model, filename, xlab) {
  conditions <- data.frame(party = unique(model$data$party))
  rownames(conditions) <- unique(model$data$party)
  cond <- conditional_effects(
    model, conditions = conditions, 
    re_formula = NULL, nsamples = 2000,
    prob = 0.95
  )
  jitter_width <- ifelse(xlab %in% c("Gender", "Religious", "Ethnicity", "Education"), 0.15, 0)
  out <- 
    plot(cond, ncol = 3, plot = FALSE, points = TRUE,
         point_args = list(height = 0.2, width = jitter_width, alpha = 0.05, size = 1),
         line_args = list(colour = "black"),
         facet_args = list(labeller = as_labeller(function(x) ifelse(x == "Maori", sprintf("M\u101ori"), x))))[[1]] +
    scale_y_continuous(name = "Support for political party (1-7)", limits = c(1, 7), breaks = 1:7) +
    labs(x = xlab)
  if (xlab == "Ethnicity") out <- out + scale_x_discrete(labels = function(x) ifelse(x == "Maori", sprintf("M\u101ori"), x))
  return(out)
}

ch4_makeSlopePlot <- function(slopeLabour1, slopeNational1, slopeGreens1,
                          slopeACT1, slopeNZFirst1, slopeMaori1, order, xlab) {
  out <-
    tibble(
      iter       = 1:length(slopeLabour1),
      Labour     = slopeLabour1,
      National   = slopeNational1,
      Greens     = slopeGreens1,
      ACT        = slopeACT1,
      `NZ First` = slopeNZFirst1,
      Maori      = slopeMaori1
    ) %>%
    pivot_longer(
      cols = Labour:Maori,
      names_to = "party",
      values_to = "slope"
    ) %>%
    mutate(party = factor(party, levels = order)) %>%
    ggplot(aes(x = slope, y = party)) +
    geom_density_ridges(scale = 0.9, rel_min_height = 0.01) +
    stat_pointinterval() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_y_discrete(name = NULL, labels = function(x) ifelse(x == "Maori", "M\u101ori", x)) + 
    scale_x_continuous(name = xlab)
  return(out)
}

ch4_makeLooPlot <- function(loo0, loo1, loo2, loo3, loo4, loo5, 
                            loo6, loo7, loo8, loo9, loo10, loo11) {
  # prepare for plotting
  models <- c("Cooperation", "BEAST", "Gender",
              "Age", "Ethnicity", "Religious", "Education",
              "SES", "Local deprivation")
  getLooDiff <- function(model, baseline, col) {
    diff <- loo_compare(baseline, model)
    est  <- diff[2, "elpd_diff"]
    est  <- ifelse(rownames(diff)[1] == attr(model, "model_name"), abs(est), est)
    se   <- diff[2, "se_diff"]
    return(ifelse(col == "elpd_diff", est, se))
  }
  # plot models
  p <-
    tibble(x = 1:100, y = 1:100) %>%
    ggplot(aes(x, y)) +
    theme(axis.title = element_blank(),
          axis.text  = element_blank(),
          axis.ticks = element_blank(),
          axis.line  = element_blank(),
          title = element_text(size = 10)
    ) +
    geom_rect(xmin = 10, xmax = 40, ymin = 66, ymax = 74, color = 'black', fill = 'white', size = 0.3) +
    geom_rect(xmin = 10, xmax = 40, ymin = 56, ymax = 64, color = 'black', fill = 'white', size = 0.3) +
    geom_rect(xmin = 10, xmax = 40, ymin = 46, ymax = 54, color = 'black', fill = 'white', size = 0.3) +
    geom_rect(xmin = 10, xmax = 40, ymin = 36, ymax = 44, color = 'black', fill = 'white', size = 0.3) +
    geom_rect(xmin = 10, xmax = 40, ymin = 26, ymax = 34, color = 'black', fill = 'white', size = 0.3) +
    geom_rect(xmin = 10, xmax = 40, ymin = 16, ymax = 24, color = 'black', fill = 'white', size = 0.3) +
    geom_rect(xmin = 10, xmax = 40, ymin = 06, ymax = 14, color = 'black', fill = 'white', size = 0.3) +
    geom_rect(xmin = 60, xmax = 90, ymin = 46, ymax = 54, color = 'black', fill = 'white', size = 0.3) +
    annotate('text', x = 25, y = 70, size = 2.5, label = 'Gender'     ) +
    annotate('text', x = 25, y = 60, size = 2.5, label = 'Age'        ) +
    annotate('text', x = 25, y = 50, size = 2.5, label = 'Ethnicity'  ) +
    annotate('text', x = 25, y = 40, size = 2.5, label = 'Education'  ) +
    annotate('text', x = 25, y = 30, size = 2.5, label = 'Religious'  ) +
    annotate('text', x = 25, y = 20, size = 2.5, label = 'SES'        ) +
    annotate('text', x = 25, y = 10, size = 2.5, label = 'Deprivation') +
    annotate('text', x = 75, y = 50, size = 2.5, label = 'Party') +
    geom_segment(x = 40, xend = 60, y = 10, yend = 46, arrow = arrow(length = unit(1, "mm")), size = 0.03) +
    geom_segment(x = 40, xend = 60, y = 20, yend = 47, arrow = arrow(length = unit(1, "mm")), size = 0.03) +
    geom_segment(x = 40, xend = 60, y = 30, yend = 48, arrow = arrow(length = unit(1, "mm")), size = 0.03) +
    geom_segment(x = 40, xend = 60, y = 40, yend = 49, arrow = arrow(length = unit(1, "mm")), size = 0.03) +
    geom_segment(x = 40, xend = 60, y = 50, yend = 50, arrow = arrow(length = unit(1, "mm")), size = 0.03) +
    geom_segment(x = 40, xend = 60, y = 60, yend = 51, arrow = arrow(length = unit(1, "mm")), size = 0.03) +
    geom_segment(x = 40, xend = 60, y = 70, yend = 52, arrow = arrow(length = unit(1, "mm")), size = 0.03)
  # general-orientations model
  pA <-
    p +
    geom_rect(xmin = 10, xmax = 40, ymin = 86, ymax = 94, color = 'black', fill = 'white', size = 0.3) +
    geom_rect(xmin = 10, xmax = 40, ymin = 76, ymax = 84, color = 'black', fill = 'white', size = 0.3) +
    annotate('text', x = 25, y = 90, size = 2.5, label = 'Cooperation') +
    annotate('text', x = 25, y = 80, size = 2.5, label = 'BEAST'      ) +
    geom_segment(x = 40, xend = 60, y = 80, yend = 53, arrow = arrow(length = unit(1, "mm")), size = 0.03) +
    geom_segment(x = 40, xend = 60, y = 90, yend = 54, arrow = arrow(length = unit(1, "mm")), size = 0.03) +
    annotate('text', x = 0, y = 40, size = 2.5, angle = 90, label = 'Socio-demographics') +
    annotate('text', x = 0, y = 85, size = 2.5, angle = 90, label = 'Behaviour') +
    ggtitle("   General-orientations")
  # domain-specific model
  pB <- p + ggtitle("   Domain-specific")
  # compare domain-specific and general-orientations models
  pC <-
    tibble(model = list(loo10)) %>%
    mutate(est   = map(model, getLooDiff, loo11, "elpd_diff"),
           se    = map(model, getLooDiff, loo11, "se_diff")) %>%
    unnest(c(est, se)) %>%
    mutate(model = "General\norientations\nmodel") %>%
    ggplot(aes(x = est, xmin = est - se*2, xmax = est + se*2, y = model)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_pointrange() +
    labs(x = "Improvement in predictive\naccuracy from domain-specific\nmodel (ELPD difference)", y = NULL)
  # compare to null
  pD <-
    tibble(model = list(loo1, loo2, loo3, loo4, loo5, loo6, loo7, loo8, loo9)) %>%
    mutate(est   = map(model, getLooDiff, loo0, "elpd_diff"),
           se    = map(model, getLooDiff, loo0, "se_diff")) %>%
    unnest(c(est, se)) %>%
    mutate(model = factor(models, levels = models)) %>%
    ggplot(aes(x = est, xmin = est - se*2, xmax = est + se*2, 
               y = fct_rev(model))) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_pointrange() +
    labs(x = "Improvement in predictive\naccuracy from null\nmodel (ELPD difference)", y = NULL) +
    theme(legend.position = "none")
  # compare to cooperation
  pE <-
    tibble(model = list(loo2, loo3, loo4, loo5, loo6, loo7, loo8, loo9)) %>%
    mutate(est    = map(model, getLooDiff, loo1, "elpd_diff"),
           se     = map(model, getLooDiff, loo1, "se_diff")) %>%
    unnest(c(est, se)) %>%
    mutate(model = factor(models[2:9], levels = models[2:9])) %>%
    ggplot(aes(x = est, xmin = est - se*2, xmax = est + se*2, 
               y = fct_rev(model))) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_pointrange() +
    labs(x = "Improvement in predictive\naccuracy from cooperation\nmodel (ELPD difference)", y = NULL) +
    theme(legend.position = "none")
  # put together
  top <- plot_grid(pA, NULL, pB, NULL, pC, nrow = 1, labels = c("a", "", "b", "", "c"), 
                   rel_widths = c(0.8, -0.05, 0.8, -0.05, 1))
  bot <- plot_grid(pD, pE, nrow = 1, labels = c("d", "e"))
  out <- plot_grid(top, bot, rel_heights = c(1, 0.7), ncol = 1)
  return(out)
}

ch4_getBeastImages <- function() {
  getBeastImage <- function(file) ggdraw() + draw_image(image_read(file), scale = 0.9)
  pA <- getBeastImage("images/ch4/beastImages/ants.jpg")
  pB <- getBeastImage("images/ch4/beastImages/bees.jpg")
  pC <- getBeastImage("images/ch4/beastImages/flamingos.jpg")
  pD <- getBeastImage("images/ch4/beastImages/cranes.jpg")
  pE <- getBeastImage("images/ch4/beastImages/crickets.jpg")
  out <- 
    plot_grid(
      plot_grid(pA, pB, nrow = 1, labels = c("a","b")),
      plot_grid(pC, pD, nrow = 1, labels = c("c","d")),
      plot_grid(NULL, pE, NULL, rel_widths = c(0.25, 0.5, 0.25), nrow = 1, labels = c("","e","")),
      nrow = 3
    )
  return(out)
}

ch4_plotImpModel <- function(dM) {
  vars <- c("Age.T11.c", "NZREG.T11.c", "NZSEI13.T11.c",
            "NZDepRAW.2013.T11.c", "Pol.SupNational.T11",
            "Pol.SupLabour.T11", "Pol.SupACT.T11",
            "Pol.SupGreens.T11", "Pol.SupMaori.T11",
            "Pol.SupNZFirst.T11")
  plots <- list()
  for (i in 1:length(vars)) plots[[i]] <- densityplot(dM, as.formula(paste("~", vars[i])))
  out <- cowplot::plot_grid(plotlist = plots, ncol = 3, nrow = 4)
  return(out)
}

ch4_plotBEASTNZFirst <- function(beastNZFirst) {
  cond <- conditional_effects(beastNZFirst, nsamples = 2000, prob = 0.95)
  out <- 
    plot(cond, plot = FALSE, points = TRUE,
         point_args = list(height = 0.2, width = 0, alpha = 0.05, size = 1),
         line_args = list(colour = "black"))[[1]] +
    scale_y_continuous(name = "Support for NZ First (1-7)", limits = c(1, 7), breaks = 1:7) +
    labs(x = "BEAST Score\n(0 = no conformity, 1 = full conformity)")
  return(out)
}

ch4_makeItemTable <- function() {
  tibble(
    Item = 
      c("Support for National Party", "Support for Labour Party",
        "Support for Green Party", "Support for New Zealand First Party",
        "Support for ACT Party", "Support for M\\={a}ori Party",
        "Age", "Gender", "Ethnicity", "Education level", 
        "Socio-economic status", "Local deprivation",
        "Religiosity"
      ),
    `Description / Text` = 
      c("Please rate how strongly you oppose or support each of the following political parties... the National Party",
        "Please rate how strongly you oppose or support each of the following political parties... the Labour Party",
        "Please rate how strongly you oppose or support each of the following political parties... the Green Party",
        "Please rate how strongly you oppose or support each of the following political parties... the NZ First Party",
        "Please rate how strongly you oppose or support each of the following political parties... the ACT Party",
        "Please rate how strongly you oppose or support each of the following political parties... the M\\={a}ori Party",
        "What is your date of birth?",
        "What is your gender? (open-ended)",
        "Which ethnic group do you belong to? (NZ census question)",
        "NZ Reg (0-10 education ordinal rank)",
        "NZSEI13 (NZ Socio-economic index)",
        "Deprivation score 2013 (for Meshblock)",
        "Do you identify with a religion and/or spiritual group?"
      )
  )
}
