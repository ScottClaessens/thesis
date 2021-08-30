# drake plan
plan <- drake_plan(
  
  ###################
  #### Chapter 2 ####
  ###################
  
  ch2_figUniMulti   = ch2_createFigUniMulti(),
  ch2_tabScales     = ch2_createTabScales(),
  ch2_tabScaleItems = ch2_createTabScaleItems(),
  
  ###################
  #### Chapter 3 ####
  ###################
  
  ### Load data ###

  ch3_d = ch3_loadData(),

  ### Study 1 ###

  # filter data
  ch3_d1 = ch3_filterData1(ch3_d),
  # table of items
  ch3_itemTable = ch3_makeItemTable(),
  # sample characteristics
  ch3_samplePlot = ch3_makeSamplePlot(ch3_d1),
  # correlations
  ch3_cor = ch3_makeCor(ch3_d1),
  # pca
  ch3_pca1.1 = ch3_runPCA(ch3_d1, extraGames = F, nfactors = 7, rotate = "none"),
  ch3_pca1.2 = ch3_runPCA(ch3_d1, extraGames = F, nfactors = 2, rotate = "varimax"),
  ch3_pca2.1 = ch3_runPCA(ch3_d1, extraGames = T, nfactors = 9, rotate = "none"),
  ch3_pca2.2 = ch3_runPCA(ch3_d1, extraGames = T, nfactors = 2, rotate = "varimax"),
  ch3_corPcaPlot = ch3_makeCorPcaPlot(ch3_cor, ch3_pca1.2, ch3_pca2.2),
  ch3_screePlot = ch3_makeScreePlot(ch3_pca1.1, ch3_pca2.1),
  # cfa
  ch3_cfa1 = ch3_runCFA(ch3_d1, extraGames = F),
  ch3_cfa2 = ch3_runCFA(ch3_d1, extraGames = T),
  # Cronbach's alpha
  ch3_alphaSDO = alpha(ch3_d1 %>% select(SDO01.T10:SDO06r.T10))$total$std.alpha,
  ch3_alphaRWA = alpha(ch3_d1 %>% select(RWA01.T10:RWA06r.T10))$total$std.alpha,
  # sem
  ch3_sem1 = ch3_runSEM(ch3_d1),
  ch3_sem2 = ch3_runSEM(ch3_d1, controls = " + Age.T10.c + Gender.T10"),
  ch3_sem3 = ch3_runSEM(ch3_d1, controls = " + EthnicCats.T10_Pakeha"),
  ch3_sem4 = ch3_runSEM(ch3_d1, controls = " + NZREG.T10.c"),
  ch3_sem5 = ch3_runSEM(ch3_d1, controls = " + NZSEI13.T10.c + NZDepRAW.2013.T10.c"),
  ch3_sem6 = ch3_runSEM(ch3_d1, controls = " + Religious.T10"),
  ch3_sem7 = ch3_runSEM(ch3_d1, controls = " + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10"),
  ch3_coopPunPlot = ch3_makeCoopPunPlot(ch3_d1, ch3_sem1),
  ch3_semTable = ch3_makeSEMtable(ch3_sem7),
  # individual games with controls
  ch3_indGamesSDO = ch3_fitIndGames(ch3_d1, "SDO.T10 ~ RWA.T10.c"),
  ch3_indGamesRWA = ch3_fitIndGames(ch3_d1, "RWA.T10 ~ SDO.T10.c"),
  ch3_indGamesPlotSDO = ch3_makeIndPlotGrid(ch3_d1, ch3_indGamesSDO, "SDO.T10"),
  ch3_indGamesPlotRWA = ch3_makeIndPlotGrid(ch3_d1, ch3_indGamesRWA, "RWA.T10"),
  # policies
  ch3_policy01 = ch3_fitPolicy(ch3_d1, "Issue.IncomeRedistribution.T10", "coop + ", "pun + "),
  ch3_policy02 = ch3_fitPolicy(ch3_d1, "IncomeAttribution.T10",          "coop + ", "pun + "),
  ch3_policy03 = ch3_fitPolicy(ch3_d1, "Env.SacWilling.T09",             "coop + ", "pun + "),
  ch3_policy04 = ch3_fitPolicy(ch3_d1, "Issue.SameSexMarriage.T09",      "coop + ", "pun + "),
  ch3_policy05 = ch3_fitPolicy(ch3_d1, "Issue.Euthanasia.T09",           "coop + ", "pun + "),
  ch3_policy06 = ch3_fitPolicy(ch3_d1, "Issue.Abortion.AnyReason.T10",   "coop + ", "pun + "),
  ch3_policy07 = ch3_fitPolicy(ch3_d1, "Issue.Payments.Jobseeker.T06",   "coop + ", "pun + "),
  ch3_policy08 = ch3_fitPolicy(ch3_d1, "Issue.Payments.SoleParent.T06",  "coop + ", "pun + "),
  ch3_policy09 = ch3_fitPolicy(ch3_d1, "Issue.TaxPolicy.T05",            "coop + ", "pun + "),
  ch3_policyPlotCoop = ch3_makePolicyGrid(ch3_d1, ch3_sem1),
  ch3_policyR2Plot = ch3_makePolicyR2Grid(ch3_d1),
  ch3_policy01table = ch3_makePolicyTable(ch3_policy01),
  ch3_policy02table = ch3_makePolicyTable(ch3_policy02),
  ch3_policy03table = ch3_makePolicyTable(ch3_policy03),
  ch3_policy04table = ch3_makePolicyTable(ch3_policy04),
  ch3_policy05table = ch3_makePolicyTable(ch3_policy05),
  ch3_policy06table = ch3_makePolicyTable(ch3_policy06),
  ch3_policy07table = ch3_makePolicyTable(ch3_policy07),
  ch3_policy08table = ch3_makePolicyTable(ch3_policy08),
  ch3_policy09table = ch3_makePolicyTable(ch3_policy09),
  # effect sizes
  ch3_r_SDO_coop       = ch3_semiPartialR1(ch3_d1, ch3_sem1, "", "coop", "coop \\~ SDO\\.T10\\.c \\+ RWA\\.T10\\.c", "coop ~ RWA.T10.c"),
  ch3_r_RWA_coop       = ch3_semiPartialR1(ch3_d1, ch3_sem1, "", "coop", "coop \\~ SDO\\.T10\\.c \\+ RWA\\.T10\\.c", "coop ~ SDO.T10.c"),
  ch3_r_SDO_pun        = ch3_semiPartialR1(ch3_d1, ch3_sem1, "", "pun",  "pun  \\~ SDO\\.T10\\.c \\+ RWA\\.T10\\.c", "pun ~ RWA.T10.c" ),
  ch3_r_RWA_pun        = ch3_semiPartialR1(ch3_d1, ch3_sem1, "", "pun",  "pun  \\~ SDO\\.T10\\.c \\+ RWA\\.T10\\.c", "pun ~ SDO.T10.c" ),
  ch3_r_indGamesSDO    = ch3_semiPartialR2(ch3_indGamesSDO),
  ch3_r_indGamesRWA    = ch3_semiPartialR2(ch3_indGamesRWA),
  ch3_r_policy01_coop  = ch3_semiPartialR3(ch3_d1, ch3_policy01, "", "pun + "),
  ch3_r_policy02_coop  = ch3_semiPartialR3(ch3_d1, ch3_policy02, "", "pun + "),
  ch3_r_policy03_coop  = ch3_semiPartialR3(ch3_d1, ch3_policy03, "", "pun + "),
  ch3_r_policy04_coop  = ch3_semiPartialR3(ch3_d1, ch3_policy04, "", "pun + "),
  ch3_r_policy05_coop  = ch3_semiPartialR3(ch3_d1, ch3_policy05, "", "pun + "),
  ch3_r_policy06_coop  = ch3_semiPartialR3(ch3_d1, ch3_policy06, "", "pun + "),
  ch3_r_policy07_coop  = ch3_semiPartialR3(ch3_d1, ch3_policy07, "", "pun + "),
  ch3_r_policy08_coop  = ch3_semiPartialR3(ch3_d1, ch3_policy08, "", "pun + "),
  ch3_r_policy09_coop  = ch3_semiPartialR3(ch3_d1, ch3_policy09, "", "pun + "),
  ch3_r_policy01_pun   = ch3_semiPartialR3(ch3_d1, ch3_policy01, "coop + ", ""),
  ch3_r_policy02_pun   = ch3_semiPartialR3(ch3_d1, ch3_policy02, "coop + ", ""),
  ch3_r_policy03_pun   = ch3_semiPartialR3(ch3_d1, ch3_policy03, "coop + ", ""),
  ch3_r_policy04_pun   = ch3_semiPartialR3(ch3_d1, ch3_policy04, "coop + ", ""),
  ch3_r_policy05_pun   = ch3_semiPartialR3(ch3_d1, ch3_policy05, "coop + ", ""),
  ch3_r_policy06_pun   = ch3_semiPartialR3(ch3_d1, ch3_policy06, "coop + ", ""),
  ch3_r_policy07_pun   = ch3_semiPartialR3(ch3_d1, ch3_policy07, "coop + ", ""),
  ch3_r_policy08_pun   = ch3_semiPartialR3(ch3_d1, ch3_policy08, "coop + ", ""),
  ch3_r_policy09_pun   = ch3_semiPartialR3(ch3_d1, ch3_policy09, "coop + ", ""),
  # extra punishment analysis
  ch3_compWorld01 = ch3_fitPolicy(ch3_d1, "Comp.World01.T06", "coop + ", "pun + "),
  ch3_compWorld02 = ch3_fitPolicy(ch3_d1, "Comp.World02r.T06", "coop + ", "pun + "),
  ch3_compWorld03 = ch3_fitPolicy(ch3_d1, "Comp.World01.T06", "SDO.T10.c + ", ""),

  ### Study 2 ###

  # filter data
  ch3_d2 = ch3_filterData2(ch3_d),
  # RULE FOLLOWING TASK
  # prepare data for modelling
  ch3_d2RF = ch3_rfData(ch3_d2),
  # spearman's correlation
  ch3_corRF = ch3_rfCor(ch3_d2),
  # distribution
  ch3_histRF = ch3_rfHist(ch3_d2),
  # fit rf models
  ch3_rf0 = ch3_fitRFModel(ch3_d2RF, bf("ruleFollow ~ 0 + Intercept + round + (1 + round | Questionnaire.Num)")),
  ch3_rf1 = ch3_fitRFModel(ch3_d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + (1 + round | Questionnaire.Num)")),
  ch3_rf2 = ch3_fitRFModel(ch3_d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + SDO.T10.c + (1 + round | Questionnaire.Num)")),
  ch3_rf3 = ch3_fitRFModel(ch3_d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + Gender.T10 + (1 + round | Questionnaire.Num)")),
  ch3_rf4 = ch3_fitRFModel(ch3_d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + Age.T10.c + (1 + round | Questionnaire.Num)")),
  ch3_rf5 = ch3_fitRFModel(ch3_d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + NZDepRAW.2013.T10.c + (1 + round | Questionnaire.Num)")),
  ch3_rf6 = ch3_fitRFModel(ch3_d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + EthnicCats.T10 + (1 + round | Questionnaire.Num)")),
  ch3_rf7 = ch3_fitRFModel(ch3_d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + NZREG.T10.c + (1 + round | Questionnaire.Num)")),
  ch3_rf8 = ch3_fitRFModel(ch3_d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + NZSEI13.T10.c + (1 + round | Questionnaire.Num)")),
  ch3_rf9 = ch3_fitRFModel(ch3_d2RF, bf("ruleFollow ~ 0 + Intercept + round + RWA.T10.c + SDO.T10.c + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c + (1 + round | Questionnaire.Num)")),
  # kfold rf
  ch3_kfoldRF0 = kfold(ch3_rf0),
  ch3_kfoldRF1 = kfold(ch3_rf1),
  ch3_kfoldRFcompare = loo_compare(ch3_kfoldRF0, ch3_kfoldRF1),
  # fit policy rf models
  ch3_policyRF01 = ch3_fitPolicyRF(ch3_d2, bf("Issue.IncomeRedistribution.T10    ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyRF02 = ch3_fitPolicyRF(ch3_d2, bf("IncomeAttribution.T10             ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyRF03 = ch3_fitPolicyRF(ch3_d2, bf("Env.SacWilling.T09                ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyRF04 = ch3_fitPolicyRF(ch3_d2, bf("Issue.SameSexMarriage.T09         ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyRF05 = ch3_fitPolicyRF(ch3_d2, bf("Issue.Euthanasia.T09              ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyRF06 = ch3_fitPolicyRF(ch3_d2, bf("Issue.Abortion.AnyReason.T10      ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyRF07 = ch3_fitPolicyRF(ch3_d2, bf("Issue.Payments.Jobseeker.T06      ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyRF08 = ch3_fitPolicyRF(ch3_d2, bf("Issue.Payments.SoleParent.T06     ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyRF09 = ch3_fitPolicyRF(ch3_d2, bf("Issue.TaxPolicy.T05               ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyRF10 = ch3_fitPolicyRF(ch3_d2, bf("Issue.Abortion.SpecificReason.T10 ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyRF11 = ch3_fitPolicyRF(ch3_d2, bf("Issue.ReligiousEd.T10             ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyRF12 = ch3_fitPolicyRF(ch3_d2, bf("RaceEssent.T10                    ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyRF13 = ch3_fitPolicyRF(ch3_d2, bf("SexualPrejudice01r.T10            ~ 1 + rf + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  # BEAST
  # get beast images
  ch3_beastImages = ch3_getBeastImages(),
  # prepare data for modelling
  ch3_d2BEAST = ch3_beastData(ch3_d2),
  # distribution
  ch3_histBEAST = ch3_beastHist(ch3_d2),
  # fit beast models
  ch3_beast0  = ch3_fitBEASTModel(ch3_d2BEAST, bf("bScore ~ 0 + Intercept + (1 | round) + (1 | Questionnaire.Num)")),
  ch3_beast1  = ch3_fitBEASTModel(ch3_d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + (1 | round) + (1 | Questionnaire.Num)")),
  ch3_beast2  = ch3_fitBEASTModel(ch3_d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + SDO.T10.c + (1 | round) + (1 | Questionnaire.Num)")),
  ch3_beast3  = ch3_fitBEASTModel(ch3_d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + Gender.T10 + (1 | round) + (1 | Questionnaire.Num)")),
  ch3_beast4  = ch3_fitBEASTModel(ch3_d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + Age.T10.c + (1 | round) + (1 | Questionnaire.Num)")),
  ch3_beast5  = ch3_fitBEASTModel(ch3_d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + NZDepRAW.2013.T10.c + (1 | round) + (1 | Questionnaire.Num)")),
  ch3_beast6  = ch3_fitBEASTModel(ch3_d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + EthnicCats.T10 + (1 | round) + (1 | Questionnaire.Num)")),
  ch3_beast7  = ch3_fitBEASTModel(ch3_d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + NZREG.T10.c + (1 | round) + (1 | Questionnaire.Num)")),
  ch3_beast8  = ch3_fitBEASTModel(ch3_d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + NZSEI13.T10.c + (1 | round) + (1 | Questionnaire.Num)")),
  ch3_beast9  = ch3_fitBEASTModel(ch3_d2BEAST, bf("bScore ~ 0 + Intercept + RWA.T10.c + SDO.T10.c + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c + (1 | round) + (1 | Questionnaire.Num)")),
  # kfold beast
  ch3_kfoldBEAST0 = kfold(ch3_beast0),
  ch3_kfoldBEAST1 = kfold(ch3_beast1),
  ch3_kfoldBEASTcompare = loo_compare(ch3_kfoldBEAST0, ch3_kfoldBEAST1),
  # plot beast and rf data and predictions
  ch3_rfBeastPlot = ch3_plotRFBEAST(ch3_d2, ch3_beast1, ch3_rf1),
  # fit policy beast models
  ch3_policyBEAST01 = ch3_fitPolicyBEAST(ch3_d2, bf("Issue.IncomeRedistribution.T10    ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyBEAST02 = ch3_fitPolicyBEAST(ch3_d2, bf("IncomeAttribution.T10             ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyBEAST03 = ch3_fitPolicyBEAST(ch3_d2, bf("Env.SacWilling.T09                ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyBEAST04 = ch3_fitPolicyBEAST(ch3_d2, bf("Issue.SameSexMarriage.T09         ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyBEAST05 = ch3_fitPolicyBEAST(ch3_d2, bf("Issue.Euthanasia.T09              ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyBEAST06 = ch3_fitPolicyBEAST(ch3_d2, bf("Issue.Abortion.AnyReason.T10      ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyBEAST07 = ch3_fitPolicyBEAST(ch3_d2, bf("Issue.Payments.Jobseeker.T06      ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyBEAST08 = ch3_fitPolicyBEAST(ch3_d2, bf("Issue.Payments.SoleParent.T06     ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyBEAST09 = ch3_fitPolicyBEAST(ch3_d2, bf("Issue.TaxPolicy.T05               ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyBEAST10 = ch3_fitPolicyBEAST(ch3_d2, bf("Issue.Abortion.SpecificReason.T10 ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyBEAST11 = ch3_fitPolicyBEAST(ch3_d2, bf("Issue.ReligiousEd.T10             ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyBEAST12 = ch3_fitPolicyBEAST(ch3_d2, bf("RaceEssent.T10                    ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  ch3_policyBEAST13 = ch3_fitPolicyBEAST(ch3_d2, bf("SexualPrejudice01r.T10            ~ 1 + bScore + Gender.T10 + Age.T10.c + NZDepRAW.2013.T10.c + EthnicCats.T10 + NZREG.T10.c + NZSEI13.T10.c")),
  # plot policy beast models
  ch3_policyPlotBEAST = ch3_makePolicyGridBEAST(ch3_d2, ch3_policyBEAST06, ch3_policyBEAST10, ch3_policyBEAST11, ch3_policyBEAST12, ch3_policyBEAST13),
  # tables
  ch3_rfTable = ch3_makeRFtable(ch3_rf9),
  ch3_beastTable = ch3_makeBEASTtable(ch3_beast9),
  ch3_beastPolicyTable04 = ch3_makeBEASTpolicyTable(ch3_policyBEAST04),
  ch3_beastPolicyTable05 = ch3_makeBEASTpolicyTable(ch3_policyBEAST05),
  ch3_beastPolicyTable06 = ch3_makeBEASTpolicyTable(ch3_policyBEAST06),
  ch3_beastPolicyTable10 = ch3_makeBEASTpolicyTable(ch3_policyBEAST10),
  ch3_beastPolicyTable11 = ch3_makeBEASTpolicyTable(ch3_policyBEAST11),
  ch3_beastPolicyTable12 = ch3_makeBEASTpolicyTable(ch3_policyBEAST12),
  ch3_beastPolicyTable13 = ch3_makeBEASTpolicyTable(ch3_policyBEAST13),
  
  ###################
  #### Chapter 4 ####
  ###################
  
  # load data
  ch4_d = ch4_loadData(),
  # cfa
  ch4_cfa = ch4_fitCFA(ch4_d),
  # imputation
  ch4_dM = ch4_imputeData(ch4_d, ch4_cfa),
  # party models
  ch4_m0  = ch4_fitParty(ch4_dM, ch4_cfa, F, bf("support ~ 1 + (1 | Questionnaire.Num) + (1 | party)")),
  ch4_m1  = ch4_fitParty(ch4_dM, ch4_cfa, T, bf("support ~ 1 + (1 | Questionnaire.Num) + coop + (coop | party)")),
  ch4_m2  = ch4_fitParty(ch4_dM, ch4_cfa, T, bf("support ~ 1 + (1 | Questionnaire.Num) + beast + (beast | party)")),
  ch4_m3  = ch4_fitParty(ch4_dM, ch4_cfa, T, bf("support ~ 1 + (1 | Questionnaire.Num) + Gender.T11 + (Gender.T11 | party)")),
  ch4_m4  = ch4_fitParty(ch4_dM, ch4_cfa, T, bf("support ~ 1 + (1 | Questionnaire.Num) + Age.T11.c + (Age.T11.c | party)")),
  ch4_m5  = ch4_fitParty(ch4_dM, ch4_cfa, T, bf("support ~ 1 + (1 | Questionnaire.Num) + EthnicCats.T11 + (EthnicCats.T11 | party)")),
  ch4_m6  = ch4_fitParty(ch4_dM, ch4_cfa, T, bf("support ~ 1 + (1 | Questionnaire.Num) + Religious.T11 + (Religious.T11 | party)")),
  ch4_m7  = ch4_fitParty(ch4_dM, ch4_cfa, T, bf("support ~ 1 + (1 | Questionnaire.Num) + mo(NZREG.T11.c) + (mo(NZREG.T11.c) | party)")),
  ch4_m8  = ch4_fitParty(ch4_dM, ch4_cfa, T, bf("support ~ 1 + (1 | Questionnaire.Num) + NZSEI13.T11.c + (NZSEI13.T11.c | party)")),
  ch4_m9  = ch4_fitParty(ch4_dM, ch4_cfa, T, bf("support ~ 1 + (1 | Questionnaire.Num) + NZDepRAW.2013.T11.c + (NZDepRAW.2013.T11.c | party)")),
  ch4_m10 = ch4_fitParty(ch4_dM, ch4_cfa, T, bf("support ~ 1 + (1 | Questionnaire.Num) + coop + beast + Gender.T11 + Age.T11.c + EthnicCats.T11 + 
                                                 Religious.T11 + mo(NZREG.T11.c) + NZSEI13.T11.c + NZDepRAW.2013.T11.c + (coop + beast + Gender.T11 + 
                                                 Age.T11.c + EthnicCats.T11 + Religious.T11 + mo(NZREG.T11.c) + NZSEI13.T11.c + NZDepRAW.2013.T11.c | party)")),
  ch4_m11 = ch4_fitParty(ch4_dM, ch4_cfa, T, bf("support ~ 1 + (1 | Questionnaire.Num) + Gender.T11 + Age.T11.c + EthnicCats.T11 + 
                                                 Religious.T11 + mo(NZREG.T11.c) + NZSEI13.T11.c + NZDepRAW.2013.T11.c + (Gender.T11 + 
                                                 Age.T11.c + EthnicCats.T11 + Religious.T11 + mo(NZREG.T11.c) + NZSEI13.T11.c + NZDepRAW.2013.T11.c | party)")),
  # loo
  ch4_loo0  = loo(ch4_m0),
  ch4_loo1  = loo(ch4_m1),
  ch4_loo2  = loo(ch4_m2),
  ch4_loo3  = loo(ch4_m3),
  ch4_loo4  = loo(ch4_m4),
  ch4_loo5  = loo(ch4_m5),
  ch4_loo6  = loo(ch4_m6),
  ch4_loo7  = loo(ch4_m7),
  ch4_loo8  = loo(ch4_m8),
  ch4_loo9  = loo(ch4_m9),
  ch4_loo10 = loo(ch4_m10),
  ch4_loo11 = loo(ch4_m11),
  # compare full models
  ch4_looDiffFull = loo_compare(ch4_loo10, ch4_loo11),
  # posterior samples
  ch4_post0  = posterior_samples(ch4_m0),
  ch4_post1  = posterior_samples(ch4_m1),
  ch4_post2  = posterior_samples(ch4_m2),
  ch4_post3  = posterior_samples(ch4_m3),
  ch4_post4  = posterior_samples(ch4_m4),
  ch4_post5  = posterior_samples(ch4_m5),
  ch4_post6  = posterior_samples(ch4_m6),
  ch4_post7  = posterior_samples(ch4_m7),
  ch4_post8  = posterior_samples(ch4_m8),
  ch4_post9  = posterior_samples(ch4_m9),
  ch4_post10 = posterior_samples(ch4_m10),
  ch4_post11 = posterior_samples(ch4_m11),
  # slopes for individual parties
  ch4_slopeLabour1    = ch4_getPartySlope(ch4_post1, "Labour"  , "coop"                 ),
  ch4_slopeLabour2    = ch4_getPartySlope(ch4_post2, "Labour"  , "beast"                ),
  ch4_slopeLabour3    = ch4_getPartySlope(ch4_post3, "Labour"  , "Gender.T11Male"       ),
  ch4_slopeLabour4    = ch4_getPartySlope(ch4_post4, "Labour"  , "Age.T11.c"            ),
  ch4_slopeLabour5a   = ch4_getPartySlope(ch4_post5, "Labour"  , "EthnicCats.T11Maori"  ),
  ch4_slopeLabour5b   = ch4_getPartySlope(ch4_post5, "Labour"  , "EthnicCats.T11Pacific"),
  ch4_slopeLabour5c   = ch4_getPartySlope(ch4_post5, "Labour"  , "EthnicCats.T11Pakeha" ),
  ch4_slopeLabour6    = ch4_getPartySlope(ch4_post6, "Labour"  , "Religious.T11Yes"     ),
  ch4_slopeLabour7    = ch4_getPartySlope(ch4_post7, "Labour"  , "moNZREG.T11.c"        ),
  ch4_slopeLabour8    = ch4_getPartySlope(ch4_post8, "Labour"  , "NZSEI13.T11.c"        ),
  ch4_slopeLabour9    = ch4_getPartySlope(ch4_post9, "Labour"  , "NZDepRAW.2013.T11.c"  ),
  ch4_slopeNational1  = ch4_getPartySlope(ch4_post1, "National", "coop"                 ),
  ch4_slopeNational2  = ch4_getPartySlope(ch4_post2, "National", "beast"                ),
  ch4_slopeNational3  = ch4_getPartySlope(ch4_post3, "National", "Gender.T11Male"       ),
  ch4_slopeNational4  = ch4_getPartySlope(ch4_post4, "National", "Age.T11.c"            ),
  ch4_slopeNational5a = ch4_getPartySlope(ch4_post5, "National", "EthnicCats.T11Maori"  ),
  ch4_slopeNational5b = ch4_getPartySlope(ch4_post5, "National", "EthnicCats.T11Pacific"),
  ch4_slopeNational5c = ch4_getPartySlope(ch4_post5, "National", "EthnicCats.T11Pakeha" ),
  ch4_slopeNational6  = ch4_getPartySlope(ch4_post6, "National", "Religious.T11Yes"     ),
  ch4_slopeNational7  = ch4_getPartySlope(ch4_post7, "National", "moNZREG.T11.c"        ),
  ch4_slopeNational8  = ch4_getPartySlope(ch4_post8, "National", "NZSEI13.T11.c"        ),
  ch4_slopeNational9  = ch4_getPartySlope(ch4_post9, "National", "NZDepRAW.2013.T11.c"  ),
  ch4_slopeGreens1    = ch4_getPartySlope(ch4_post1, "Greens"  , "coop"                 ),
  ch4_slopeGreens2    = ch4_getPartySlope(ch4_post2, "Greens"  , "beast"                ),
  ch4_slopeGreens3    = ch4_getPartySlope(ch4_post3, "Greens"  , "Gender.T11Male"       ),
  ch4_slopeGreens4    = ch4_getPartySlope(ch4_post4, "Greens"  , "Age.T11.c"            ),
  ch4_slopeGreens5a   = ch4_getPartySlope(ch4_post5, "Greens"  , "EthnicCats.T11Maori"  ),
  ch4_slopeGreens5b   = ch4_getPartySlope(ch4_post5, "Greens"  , "EthnicCats.T11Pacific"),
  ch4_slopeGreens5c   = ch4_getPartySlope(ch4_post5, "Greens"  , "EthnicCats.T11Pakeha" ),
  ch4_slopeGreens6    = ch4_getPartySlope(ch4_post6, "Greens"  , "Religious.T11Yes"     ),
  ch4_slopeGreens7    = ch4_getPartySlope(ch4_post7, "Greens"  , "moNZREG.T11.c"        ),
  ch4_slopeGreens8    = ch4_getPartySlope(ch4_post8, "Greens"  , "NZSEI13.T11.c"        ),
  ch4_slopeGreens9    = ch4_getPartySlope(ch4_post9, "Greens"  , "NZDepRAW.2013.T11.c"  ),
  ch4_slopeACT1       = ch4_getPartySlope(ch4_post1, "ACT"     , "coop"                 ),
  ch4_slopeACT2       = ch4_getPartySlope(ch4_post2, "ACT"     , "beast"                ),
  ch4_slopeACT3       = ch4_getPartySlope(ch4_post3, "ACT"     , "Gender.T11Male"       ),
  ch4_slopeACT4       = ch4_getPartySlope(ch4_post4, "ACT"     , "Age.T11.c"            ),
  ch4_slopeACT5a      = ch4_getPartySlope(ch4_post5, "ACT"     , "EthnicCats.T11Maori"  ),
  ch4_slopeACT5b      = ch4_getPartySlope(ch4_post5, "ACT"     , "EthnicCats.T11Pacific"),
  ch4_slopeACT5c      = ch4_getPartySlope(ch4_post5, "ACT"     , "EthnicCats.T11Pakeha" ),
  ch4_slopeACT6       = ch4_getPartySlope(ch4_post6, "ACT"     , "Religious.T11Yes"     ),
  ch4_slopeACT7       = ch4_getPartySlope(ch4_post7, "ACT"     , "moNZREG.T11.c"        ),
  ch4_slopeACT8       = ch4_getPartySlope(ch4_post8, "ACT"     , "NZSEI13.T11.c"        ),
  ch4_slopeACT9       = ch4_getPartySlope(ch4_post9, "ACT"     , "NZDepRAW.2013.T11.c"  ),
  ch4_slopeNZFirst1   = ch4_getPartySlope(ch4_post1, "NZ.First", "coop"                 ),
  ch4_slopeNZFirst2   = ch4_getPartySlope(ch4_post2, "NZ.First", "beast"                ),
  ch4_slopeNZFirst3   = ch4_getPartySlope(ch4_post3, "NZ.First", "Gender.T11Male"       ),
  ch4_slopeNZFirst4   = ch4_getPartySlope(ch4_post4, "NZ.First", "Age.T11.c"            ),
  ch4_slopeNZFirst5a  = ch4_getPartySlope(ch4_post5, "NZ.First", "EthnicCats.T11Maori"  ),
  ch4_slopeNZFirst5b  = ch4_getPartySlope(ch4_post5, "NZ.First", "EthnicCats.T11Pacific"),
  ch4_slopeNZFirst5c  = ch4_getPartySlope(ch4_post5, "NZ.First", "EthnicCats.T11Pakeha" ),
  ch4_slopeNZFirst6   = ch4_getPartySlope(ch4_post6, "NZ.First", "Religious.T11Yes"     ),
  ch4_slopeNZFirst7   = ch4_getPartySlope(ch4_post7, "NZ.First", "moNZREG.T11.c"        ),
  ch4_slopeNZFirst8   = ch4_getPartySlope(ch4_post8, "NZ.First", "NZSEI13.T11.c"        ),
  ch4_slopeNZFirst9   = ch4_getPartySlope(ch4_post9, "NZ.First", "NZDepRAW.2013.T11.c"  ),
  ch4_slopeMaori1     = ch4_getPartySlope(ch4_post1, "Maori"   , "coop"                 ),
  ch4_slopeMaori2     = ch4_getPartySlope(ch4_post2, "Maori"   , "beast"                ),
  ch4_slopeMaori3     = ch4_getPartySlope(ch4_post3, "Maori"   , "Gender.T11Male"       ),
  ch4_slopeMaori4     = ch4_getPartySlope(ch4_post4, "Maori"   , "Age.T11.c"            ),
  ch4_slopeMaori5a    = ch4_getPartySlope(ch4_post5, "Maori"   , "EthnicCats.T11Maori"  ),
  ch4_slopeMaori5b    = ch4_getPartySlope(ch4_post5, "Maori"   , "EthnicCats.T11Pacific"),
  ch4_slopeMaori5c    = ch4_getPartySlope(ch4_post5, "Maori"   , "EthnicCats.T11Pakeha" ),
  ch4_slopeMaori6     = ch4_getPartySlope(ch4_post6, "Maori"   , "Religious.T11Yes"     ),
  ch4_slopeMaori7     = ch4_getPartySlope(ch4_post7, "Maori"   , "moNZREG.T11.c"        ),
  ch4_slopeMaori8     = ch4_getPartySlope(ch4_post8, "Maori"   , "NZSEI13.T11.c"        ),
  ch4_slopeMaori9     = ch4_getPartySlope(ch4_post9, "Maori"   , "NZDepRAW.2013.T11.c"  ),
  # individual beast party models
  ch4_beastLabour   = ch4_fitBEASTmodel(ch4_dM, bf("Pol.SupLabour.T11   ~ beast")),
  ch4_beastNational = ch4_fitBEASTmodel(ch4_dM, bf("Pol.SupNational.T11 ~ beast")),
  ch4_beastGreens   = ch4_fitBEASTmodel(ch4_dM, bf("Pol.SupGreens.T11   ~ beast")),
  ch4_beastACT      = ch4_fitBEASTmodel(ch4_dM, bf("Pol.SupACT.T11      ~ beast")),
  ch4_beastNZFirst  = ch4_fitBEASTmodel(ch4_dM, bf("Pol.SupNZFirst.T11  ~ beast")),
  ch4_beastMaori    = ch4_fitBEASTmodel(ch4_dM, bf("Pol.SupMaori.T11    ~ beast")),
  # figures
  ch4_beastImages = ch4_getBeastImages(),
  ch4_plotImpute = ch4_plotImpModel(ch4_dM),
  ch4_plotHist = ch4_makePartyHist(ch4_m0),
  ch4_plotLOO = ch4_makeLooPlot(ch4_loo0, ch4_loo1, ch4_loo2, ch4_loo3, ch4_loo4, ch4_loo5, 
                                ch4_loo6, ch4_loo7, ch4_loo8, ch4_loo9, ch4_loo10, ch4_loo11),
  ch4_plotm1 = ch4_makePartyGrid(ch4_m1, "figures/plotCoop.pdf",        "Cooperative phenotype"),
  ch4_plotm2 = ch4_makePartyGrid(ch4_m2, "figures/plotBEAST.pdf",       "BEAST Score\n(0 = no conformity, 1 = full conformity)"),
  ch4_plotm3 = ch4_makePartyGrid(ch4_m3, "figures/plotGender.pdf",      "Gender"),
  ch4_plotm4 = ch4_makePartyGrid(ch4_m4, "figures/plotAge.pdf",         "Age (standardised)"),
  ch4_plotm5 = ch4_makePartyGrid(ch4_m5, "figures/plotEthnicity.pdf",   "Ethnicity"),
  ch4_plotm6 = ch4_makePartyGrid(ch4_m6, "figures/plotReligious.pdf",   "Religious"),
  ch4_plotm7 = ch4_makePartyGrid(ch4_m7, "figures/plotEducation.pdf",   "Education (standardised)"),
  ch4_plotm8 = ch4_makePartyGrid(ch4_m8, "figures/plotSES.pdf",         "SES (standardised)"),
  ch4_plotm9 = ch4_makePartyGrid(ch4_m9, "figures/plotDeprivation.pdf", "Local deprivation (standardised)"),
  ch4_plotBeast = ch4_plotBEASTNZFirst(ch4_beastNZFirst),
  ch4_plotCoopSlopes = ch4_makeSlopePlot(ch4_slopeLabour1, ch4_slopeNational1, ch4_slopeGreens1,
                                         ch4_slopeACT1, ch4_slopeNZFirst1, ch4_slopeMaori1,
                                         order = c("ACT", "National", "NZ First", "Labour", "Maori", "Greens"),
                                         xlab = "Posterior slope for cooperative phenotype\npredicting political party support"),
  ch4_plotBeastSlopes = ch4_makeSlopePlot(ch4_slopeLabour2, ch4_slopeNational2, ch4_slopeGreens2,
                                          ch4_slopeACT2, ch4_slopeNZFirst2, ch4_slopeMaori2,
                                          order = c("NZ First", "National", "Labour", "Maori", "ACT", "Greens"),
                                          xlab = "Posterior slope for average BEAST scores\npredicting political party support"),
  # tables
  ch4_itemTable = ch4_makeItemTable(),
  
  ###################
  #### Chapter 5 ####
  ###################
  
  # load data
  ch5_d = ch5_loadData(),
  # timeline
  ch5_timeline = ch5_plotTimeline(ch5_d),
  # item table
  ch5_itemTable = ch5_makeItemTable(),
  # impute data
  ch5_dM = ch5_imputeData(ch5_d),
  ch5_plotImputation = ch5_plotImpModel(ch5_dM),
  # fit cfa
  ch5_cfa1 = ch5_fitCFA1(ch5_d),
  # fit sem
  ch5_sem1 = ch5_fitSEM1(ch5_dM),
  ch5_plotSem1 = ch5_plotCoopSDO(ch5_dM),
  # measurement invariance across waves
  ch5_configMI  = ch5_fitMI(ch5_d, type = "config"),
  ch5_metricMI  = ch5_fitMI(ch5_d, type = "metric"),
  ch5_scalarMI  = ch5_fitMI(ch5_d, type = "scalar"),
  ch5_strictMI  = ch5_fitMI(ch5_d, type = "strict"),
  ch5_tableCompareMI = ch5_getComparisonTable(ch5_configMI, ch5_metricMI, ch5_scalarMI, ch5_strictMI),
  # cross-lagged panel models
  # with mean sdo
  ch5_clpm1.01 = ch5_fitCLPM(ch5_dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = c(paste0("egame.TG1.T1", 0:1))),
  ch5_clpm1.02 = ch5_fitCLPM(ch5_dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = c(paste0("egame.TG1.T1", 0:1)), controls = "Age.T10.c"),
  ch5_clpm1.03 = ch5_fitCLPM(ch5_dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = c(paste0("egame.TG1.T1", 0:1)), controls = "Gender.T10"),
  ch5_clpm1.04 = ch5_fitCLPM(ch5_dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = c(paste0("egame.TG1.T1", 0:1)), controls = "EthnicCats.T10_Asian + EthnicCats.T10_Maori + EthnicCats.T10_Pacific"),
  ch5_clpm1.05 = ch5_fitCLPM(ch5_dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = c(paste0("egame.TG1.T1", 0:1)), controls = "NZREG.T10"),
  ch5_clpm1.06 = ch5_fitCLPM(ch5_dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = c(paste0("egame.TG1.T1", 0:1)), controls = "NZSEI13.T10"),
  ch5_clpm1.07 = ch5_fitCLPM(ch5_dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = c(paste0("egame.TG1.T1", 0:1)), controls = "NZDep.2013.T10"),
  ch5_clpm1.08 = ch5_fitCLPM(ch5_dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = c(paste0("egame.TG1.T1", 0:1)), controls = "Religious.T10"),
  ch5_clpm1.09 = ch5_fitCLPM(ch5_dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = c(paste0("egame.TG1.T1", 0:1)), controls = "T10.RWA"),
  ch5_clpm1.10 = ch5_fitCLPM(ch5_dM, var1 = "T10.SDO", var2 = "T11.SDO", ordered = c(paste0("egame.TG1.T1", 0:1)), controls = "Age.T10.c + Gender.T10 + EthnicCats.T10_Asian + EthnicCats.T10_Maori + EthnicCats.T10_Pacific + NZREG.T10 + NZSEI13.T10 + NZDep.2013.T10 + Religious.T10 + T10.RWA"),
  # with income redistribution
  ch5_clpm2.01 = ch5_fitCLPM(ch5_dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Issue.IncomeRedistribution.T1", 0:1))),
  ch5_clpm2.02 = ch5_fitCLPM(ch5_dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Issue.IncomeRedistribution.T1", 0:1)), controls = "Age.T10.c"),
  ch5_clpm2.03 = ch5_fitCLPM(ch5_dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Issue.IncomeRedistribution.T1", 0:1)), controls = "Gender.T10"),
  ch5_clpm2.04 = ch5_fitCLPM(ch5_dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Issue.IncomeRedistribution.T1", 0:1)), controls = "EthnicCats.T10_Asian + EthnicCats.T10_Maori + EthnicCats.T10_Pacific"),
  ch5_clpm2.05 = ch5_fitCLPM(ch5_dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Issue.IncomeRedistribution.T1", 0:1)), controls = "NZREG.T10"),
  ch5_clpm2.06 = ch5_fitCLPM(ch5_dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Issue.IncomeRedistribution.T1", 0:1)), controls = "NZSEI13.T10"),
  ch5_clpm2.07 = ch5_fitCLPM(ch5_dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Issue.IncomeRedistribution.T1", 0:1)), controls = "NZDep.2013.T10"),
  ch5_clpm2.08 = ch5_fitCLPM(ch5_dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Issue.IncomeRedistribution.T1", 0:1)), controls = "Religious.T10"),
  ch5_clpm2.09 = ch5_fitCLPM(ch5_dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Issue.IncomeRedistribution.T1", 0:1)), controls = "T10.RWA"),
  ch5_clpm2.10 = ch5_fitCLPM(ch5_dM, var1 = "Issue.IncomeRedistribution.T10", var2 = "Issue.IncomeRedistribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Issue.IncomeRedistribution.T1", 0:1)), controls = "Age.T10.c + Gender.T10 + EthnicCats.T10_Asian + EthnicCats.T10_Maori + EthnicCats.T10_Pacific + NZREG.T10 + NZSEI13.T10 + NZDep.2013.T10 + Religious.T10 + T10.RWA"),
  # with income attribution
  ch5_clpm3.01 = ch5_fitCLPM(ch5_dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("IncomeAttribution.T1", 0:1))),
  ch5_clpm3.02 = ch5_fitCLPM(ch5_dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("IncomeAttribution.T1", 0:1)), controls = "Age.T10.c"),
  ch5_clpm3.03 = ch5_fitCLPM(ch5_dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("IncomeAttribution.T1", 0:1)), controls = "Gender.T10"),
  ch5_clpm3.04 = ch5_fitCLPM(ch5_dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("IncomeAttribution.T1", 0:1)), controls = "EthnicCats.T10_Asian + EthnicCats.T10_Maori + EthnicCats.T10_Pacific"),
  ch5_clpm3.05 = ch5_fitCLPM(ch5_dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("IncomeAttribution.T1", 0:1)), controls = "NZREG.T10"),
  ch5_clpm3.06 = ch5_fitCLPM(ch5_dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("IncomeAttribution.T1", 0:1)), controls = "NZSEI13.T10"),
  ch5_clpm3.07 = ch5_fitCLPM(ch5_dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("IncomeAttribution.T1", 0:1)), controls = "NZDep.2013.T10"),
  ch5_clpm3.08 = ch5_fitCLPM(ch5_dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("IncomeAttribution.T1", 0:1)), controls = "Religious.T10"),
  ch5_clpm3.09 = ch5_fitCLPM(ch5_dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("IncomeAttribution.T1", 0:1)), controls = "T10.RWA"),
  ch5_clpm3.10 = ch5_fitCLPM(ch5_dM, var1 = "IncomeAttribution.T10", var2 = "IncomeAttribution.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("IncomeAttribution.T1", 0:1)), controls = "Age.T10.c + Gender.T10 + EthnicCats.T10_Asian + EthnicCats.T10_Maori + EthnicCats.T10_Pacific + NZREG.T10 + NZSEI13.T10 + NZDep.2013.T10 + Religious.T10 + T10.RWA"),
  # with National party support
  ch5_clpm4.01 = ch5_fitCLPM(ch5_dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Pol.SupNational.T1", 0:1))),
  ch5_clpm4.02 = ch5_fitCLPM(ch5_dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Pol.SupNational.T1", 0:1)), controls = "Age.T10.c"),
  ch5_clpm4.03 = ch5_fitCLPM(ch5_dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Pol.SupNational.T1", 0:1)), controls = "Gender.T10"),
  ch5_clpm4.04 = ch5_fitCLPM(ch5_dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Pol.SupNational.T1", 0:1)), controls = "EthnicCats.T10_Asian + EthnicCats.T10_Maori + EthnicCats.T10_Pacific"),
  ch5_clpm4.05 = ch5_fitCLPM(ch5_dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Pol.SupNational.T1", 0:1)), controls = "NZREG.T10"),
  ch5_clpm4.06 = ch5_fitCLPM(ch5_dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Pol.SupNational.T1", 0:1)), controls = "NZSEI13.T10"),
  ch5_clpm4.07 = ch5_fitCLPM(ch5_dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Pol.SupNational.T1", 0:1)), controls = "NZDep.2013.T10"),
  ch5_clpm4.08 = ch5_fitCLPM(ch5_dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Pol.SupNational.T1", 0:1)), controls = "Religious.T10"),
  ch5_clpm4.09 = ch5_fitCLPM(ch5_dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Pol.SupNational.T1", 0:1)), controls = "T10.RWA"),
  ch5_clpm4.10 = ch5_fitCLPM(ch5_dM, var1 = "Pol.SupNational.T10", var2 = "Pol.SupNational.T11", ordered = c(paste0("egame.TG1.T1", 0:1), paste0("Pol.SupNational.T1", 0:1)), controls = "Age.T10.c + Gender.T10 + EthnicCats.T10_Asian + EthnicCats.T10_Maori + EthnicCats.T10_Pacific + NZREG.T10 + NZSEI13.T10 + NZDep.2013.T10 + Religious.T10 + T10.RWA"),
  # multigroup analyses split by gender
  ch5_dM_gender    = ch5_dMfactor(ch5_dM, "Gender.T10"),
  ch5_clpm1_gender = ch5_fitCLPM(ch5_dM_gender, var1 = "T10.SDO", var2 = "T11.SDO", ordered = c(paste0("egame.TG1.T1", 0:1)), group = "Gender.T10"),
  ch5_dM_ethnic    = ch5_dMfactor(ch5_dM, "EthnicCats.T10"),
  ch5_clpm1_ethnic = ch5_fitCLPM(ch5_dM_ethnic, var1 = "T10.SDO", var2 = "T11.SDO", ordered = c(paste0("egame.TG1.T1", 0:1)), group = "EthnicCats.T10"),
  # figures
  ch5_clpmPlot_SDO      = ch5_drawCLPMFigure(list(ch5_clpm1.01, ch5_clpm1.02, ch5_clpm1.03, ch5_clpm1.04, ch5_clpm1.05,
                                                  ch5_clpm1.06, ch5_clpm1.07, ch5_clpm1.08, ch5_clpm1.09, ch5_clpm1.10),
                                             "T10.SDO", "T11.SDO", "SDO", -0.4, 0.2),
  ch5_clpmPlot_IncRed   = ch5_drawCLPMFigure(list(ch5_clpm2.01, ch5_clpm2.02, ch5_clpm2.03, ch5_clpm2.04, ch5_clpm2.05,
                                                  ch5_clpm2.06, ch5_clpm2.07, ch5_clpm2.08, ch5_clpm2.09, ch5_clpm2.10),
                                             "Issue.IncomeRedistribution.T10", "Issue.IncomeRedistribution.T11", "IncRed", -0.2, 0.4),
  ch5_clpmPlot_IncAtt   = ch5_drawCLPMFigure(list(ch5_clpm3.01, ch5_clpm3.02, ch5_clpm3.03, ch5_clpm3.04, ch5_clpm3.05,
                                                  ch5_clpm3.06, ch5_clpm3.07, ch5_clpm3.08, ch5_clpm3.09, ch5_clpm3.10),
                                             "IncomeAttribution.T10", "IncomeAttribution.T11", "IncAtt", -0.4, 0.3),
  ch5_clpmPlot_PolNat   = ch5_drawCLPMFigure(list(ch5_clpm4.01, ch5_clpm4.02, ch5_clpm4.03, ch5_clpm4.04, ch5_clpm4.05,
                                                  ch5_clpm4.06, ch5_clpm4.07, ch5_clpm4.08, ch5_clpm4.09, ch5_clpm4.10),
                                             "Pol.SupNational.T10", "Pol.SupNational.T11", "PolNat", -0.25, 0.5),
  
  
  ###################
  #### Chapter 6 ####
  ###################
  
  # power analysis with previous data
  #ch6_power = ch6_runPowerAnalysis(),
  # read in raw qualtrics data
  ch6_d0 = filter(read_survey("data/miniDGs/Mini+Dictator+Games_February+12,+2021_00.19.csv"), Finished),
  # read in filtered qualtrics data
  ch6_d = ch6_getData(),
  # pivot data for analyses
  ch6_dDG = ch6_pivotDataDG(ch6_d),
  ch6_dRF = ch6_pivotDataRF(ch6_d),
  ch6_dNorm = ch6_pivotDataNorm(ch6_d),
  # pre-registered analyses
  ch6_m0   = ch6_fitModel0(ch6_dDG),
  ch6_m1   = ch6_fitModel1(ch6_dDG),
  ch6_m2a  = ch6_fitModel2a(ch6_dDG),
  ch6_m2b  = ch6_fitModel2b(ch6_dDG),
  # exploratory analyses
  ch6_m2c  = ch6_fitModel2c(ch6_dDG),
  ch6_m2d  = ch6_fitModel2d(ch6_dDG),
  ch6_m3   = ch6_fitModel3(ch6_dNorm),
  ch6_m4   = ch6_fitModel4(ch6_dDG),
  ch6_m5   = ch6_fitModel5(ch6_dDG),
  ch6_m6   = ch6_fitModel6(ch6_dDG),
  ch6_m7   = ch6_fitModel7(ch6_dDG),
  ch6_m8   = ch6_fitModel8(ch6_dDG),
  ch6_m9   = ch6_fitModel9(ch6_dRF),
  ch6_m10  = ch6_fitModel10(ch6_dDG),
  ch6_m11  = ch6_fitModel11(ch6_dDG),
  # individual models
  ch6_indModels = ch6_fitIndModels(ch6_dDG),
  ch6_indDiffP = ch6_fitIndDiff(ch6_d, ch6_indModels, "diffP_est", "diffP_se", se = TRUE),
  ch6_indDiffN = ch6_fitIndDiff(ch6_d, ch6_indModels, "diffN_est", "diffN_se", se = TRUE),
  # loo differences
  ch6_looDiff1 = loo_compare(loo(ch6_m0), loo(ch6_m1)),
  ch6_looDiff2 = loo_compare(loo(ch6_m1), loo(ch6_m2a)),
  ch6_looDiff3 = loo_compare(loo(ch6_m1), loo(ch6_m7)),
  # summaries
  ch6_summary0   = summary(ch6_m0),
  ch6_summary1   = summary(ch6_m1),
  ch6_summary2a  = summary(ch6_m2a),
  ch6_summary2b  = summary(ch6_m2b),
  ch6_summary2c  = summary(ch6_m2c),
  ch6_summary2d  = summary(ch6_m2d),
  ch6_summary3   = summary(ch6_m3),
  ch6_summary4   = summary(ch6_m4),
  ch6_summary5   = summary(ch6_m5),
  ch6_summary6   = summary(ch6_m6),
  ch6_summary7   = summary(ch6_m7),
  ch6_summary8   = summary(ch6_m8),
  ch6_summary9   = summary(ch6_m9),
  ch6_summary10  = summary(ch6_m10),
  ch6_summary11  = summary(ch6_m11),
  # posteriors
  ch6_post0   = posterior_samples(ch6_m0),
  ch6_post1   = posterior_samples(ch6_m1),
  ch6_post2a  = posterior_samples(ch6_m2a),
  ch6_post2b  = posterior_samples(ch6_m2b),
  ch6_post2c  = posterior_samples(ch6_m2c),
  ch6_post2d  = posterior_samples(ch6_m2d),
  ch6_post3   = posterior_samples(ch6_m3),
  ch6_post4   = posterior_samples(ch6_m4),
  ch6_post5   = posterior_samples(ch6_m5),
  ch6_post6   = posterior_samples(ch6_m6),
  ch6_post7   = posterior_samples(ch6_m7),
  ch6_post8   = posterior_samples(ch6_m8),
  ch6_post9   = posterior_samples(ch6_m9),
  ch6_post10  = posterior_samples(ch6_m10),
  ch6_post11  = posterior_samples(ch6_m11),
  # figures
  ch6_plotCoopNorm   = ch6_createCoopNormPlot(ch6_dDG, ch6_dNorm),
  ch6_plotSDO        = ch6_createSDOPlot(ch6_m5),
  ch6_plotRWA        = ch6_createRWAPlot(ch6_m3),
  ch6_plotUtility    = ch6_createUtilityPlot(ch6_m1),
  ch6_plotUtilityVar = ch6_createUtilityVarPlot(ch6_m1),
  ch6_plotSDORWA     = ch6_createMainPlot(ch6_d, ch6_m2a, ch6_post2a, "sdo", "rwa", "Social Dominance Orientation", 
                                  "Right Wing Authoritarianism", "plotSDORWA.pdf"),
  ch6_plotInequalityAversion = ch6_createInequalityAversionPlot(ch6_m7),
  ch6_plotInequalityAversionVar = ch6_createInequalityAversionVarPlot(ch6_m7),
  ch6_plotInequalityAversionSDO = ch6_createInequalityAversionSDOPlot(ch6_d, ch6_m8, ch6_post8),
  # tables
  ch6_tableMiniDG = ch6_createMiniDGTable(),
  ch6_tableSelfReport = ch6_createSelfReportTable(),
  
  ##########################
  #### Final Manuscript ####
  ##########################
  
  manuscript = bookdown::render_book("index.Rmd")
  
)
