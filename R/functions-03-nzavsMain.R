# custom functions

ch3_loadData <- function() {
  d <- 
    # load data
    read_sav("data/NZAVS Longitudinal SPSS Base Dataset.sav",
             col_select = c(Questionnaire.Num, starts_with("egame."), Age.T10, Gender.T10, EthnicCats.T10,
                            NZREG.T10, NZSEI13.T10, NZDepRAW.2013.T10, Religious.T10, MBU_2018.T10,
                            SDO01.T10, SDO02.T10, SDO03.T10, SDO04r.T10, SDO05r.T10, SDO06r.T10,
                            RWA01.T10, RWA02.T10, RWA03.T10, RWA04r.T10, RWA05r.T10, RWA06r.T10,
                            Issue.IncomeRedistribution.T10, IncomeAttribution.T10,
                            Issue.Abortion.AnyReason.T10, Issue.Abortion.SpecificReason.T10,
                            Issue.ReligiousEd.T10, RaceEssent.T10, SexualPrejudice01r.T10, 
                            Env.SacWilling.T09, Issue.SameSexMarriage.T09, Issue.Euthanasia.T09, 
                            Issue.Payments.Jobseeker.T06, Issue.Payments.SoleParent.T06, Comp.World01.T06, 
                            Comp.World02r.T06, Issue.TaxPolicy.T05)) %>%
    # remove labels
    remove_all_labels() %>%
    # mutate and create variables
    # "time on games"
    select(-(egame.Secs49.T10:egame.Secs57.T10), -egame.SecsW.T10, -egame.SecsG.T10) %>%
    select(-(egame.Secs49.T11:egame.Secs57.T11), -egame.SecsW.T11, -egame.SecsG.T11) %>%
    mutate(egame.SecsAll.T10 = rowSums(select(., starts_with("egame.Secs") & ends_with(".T10")), na.rm = TRUE),
           egame.SecsAll.T11 = rowSums(select(., starts_with("egame.Secs") & ends_with(".T11")), na.rm = TRUE)) %>%
    # mean SDO and RWA
    rowwise %>%
    mutate(SDO.T10 = mean(c(SDO01.T10, SDO02.T10, SDO03.T10, SDO04r.T10, SDO05r.T10, SDO06r.T10), na.rm = T),
           RWA.T10 = mean(c(RWA01.T10, RWA02.T10, RWA03.T10, RWA04r.T10, RWA05r.T10, RWA06r.T10), na.rm = T)) %>%
    ungroup() %>%
    mutate(
      # centred SDO and RWA
      SDO.T10.c = scale(SDO.T10, scale = FALSE)[,],
      RWA.T10.c = scale(RWA.T10, scale = FALSE)[,],
      # EthnicCats.T10 as a factor
      EthnicCats.T10 = factor(ifelse(EthnicCats.T10 == 1, "Pakeha",
                                     ifelse(EthnicCats.T10 == 2, "Maori",
                                            ifelse(EthnicCats.T10 == 3, "Pacific",
                                                   ifelse(EthnicCats.T10 == 4, "Asian", EthnicCats.T10))))),
      # centred age, nzreg, nzsei, and nzdep
      Age.T10.c = scale(Age.T10, scale = FALSE)[,],
      NZREG.T10.c = scale(NZREG.T10, scale = FALSE)[,],
      NZSEI13.T10.c = scale(NZSEI13.T10, scale = FALSE)[,] / 10,
      NZDepRAW.2013.T10.c = scale(NZDepRAW.2013.T10, scale = FALSE)[,] / 100,
      # game data scaled between 0 and 1
      egame.SHP2.T10 = egame.SHP2.T10 / 50,
      egame.SHP3.T10 = egame.SHP3.T10 / 50,
      egame.TG2.T10  = egame.TG2.T10  / 150,
      egame.PGG.T10  = egame.PGG.T10  / 100,
      egame.UG1.T10  = egame.UG1.T10  / 100,
      egame.UG2.T10  = egame.UG2.T10  / 100,
      egame.DG.T10   = egame.DG.T10   / 100,
      egame.TPP2.T10 = egame.TPP2.T10 / 100,
      egame.SPP2.T10 = egame.SPP2.T10 / 50,
      egame.SPP3.T10 = egame.SPP3.T10 / 50,
      egame.TG2.T11  = egame.TG2.T11  / 150,
      egame.PGG.T11  = egame.PGG.T11  / 100,
      egame.UG1.T11  = egame.UG1.T11  / 100,
      egame.UG2.T11  = egame.UG2.T11  / 100,
      egame.DG.T11   = egame.DG.T11   / 100,
      egame.TPP2.T11 = egame.TPP2.T11 / 100,
      egame.SPP2.T11 = egame.SPP2.T11 / 50,
      egame.SPP3.T11 = egame.SPP3.T11 / 50,
      # comprehension questions: 1 == correct, 0 = incorrect
      egame.cmpSH.T10    = ifelse(egame.cmpSH.T10    == 3, 1, 0),
      egame.cmpTG.T10    = ifelse(egame.cmpTG.T10    == 1, 1, 0),
      egame.cmpPGG.T10   = ifelse(egame.cmpPGG.T10   == 3, 1, 0),
      egame.cmpDG.T10    = ifelse(egame.cmpDG.T10    == 1, 1, 0),
      egame.cmpUG.T10    = ifelse(egame.cmpUG.T10    == 3, 1, 0),
      egame.cmpTPP.T10   = ifelse(egame.cmpTPP.T10   == 1, 1, 0),
      egame.cmpSPP.T10   = ifelse(egame.cmpSPP.T10   == 2, 1, 0),
      egame.cmoSHP.T10   = ifelse(egame.cmoSHP.T10   == 3, 1, 0),
      egame.cmpTG.T11    = ifelse(egame.cmpTG.T11    == 1, 1, 0),
      egame.cmpPGG.T11   = ifelse(egame.cmpPGG.T11   == 3, 1, 0),
      egame.cmpDG.T11    = ifelse(egame.cmpDG.T11    == 1, 1, 0),
      egame.cmpUG.T11    = ifelse(egame.cmpUG.T11    == 3, 1, 0),
      egame.cmpTPP.T11   = ifelse(egame.cmpTPP.T11   == 1, 1, 0),
      egame.cmpSPP.T11   = ifelse(egame.cmpSPP.T11   == 2, 1, 0),
      egame.cmpBEAST.T11 = ifelse(egame.cmpBEAST.T11 == 1, 1, 0),
      egame.cmpRFT.T11   = ifelse(egame.cmpRFT.T11   == 1, 1, 0),
      # composite comprehension variable
      egame.cmpOverall.T10 = (egame.cmpSH.T10  + egame.cmpTG.T10  +
                                egame.cmpPGG.T10 + egame.cmpDG.T10  +
                                egame.cmpUG.T10  + egame.cmpTPP.T10 +
                                egame.cmpSPP.T10 + egame.cmoSHP.T10) / 8,
      # variables with NA where comprehension failed
      egame.SH.T10.NA   = ifelse(egame.cmpSH.T10  == 1, egame.SH.T10,   NA),
      egame.TG1.T10.NA  = ifelse(egame.cmpTG.T10  == 1, egame.TG1.T10,  NA),
      egame.TG2.T10.NA  = ifelse(egame.cmpTG.T10  == 1, egame.TG2.T10,  NA),
      egame.PGG.T10.NA  = ifelse(egame.cmpPGG.T10 == 1, egame.PGG.T10,  NA),
      egame.DG.T10.NA   = ifelse(egame.cmpDG.T10  == 1, egame.DG.T10,   NA),
      egame.UG1.T10.NA  = ifelse(egame.cmpUG.T10  == 1, egame.UG1.T10,  NA),
      egame.UG2.T10.NA  = ifelse(egame.cmpUG.T10  == 1, egame.UG2.T10,  NA),
      egame.TPP1.T10.NA = ifelse(egame.cmpTPP.T10 == 1, egame.TPP1.T10, NA),
      egame.TPP2.T10.NA = ifelse(egame.cmpTPP.T10 == 1, egame.TPP2.T10, NA),
      egame.SPP1.T10.NA = ifelse(egame.cmpSPP.T10 == 1, egame.SPP1.T10, NA),
      egame.SPP2.T10.NA = ifelse(egame.cmpSPP.T10 == 1, egame.SPP2.T10, NA),
      egame.SPP3.T10.NA = ifelse(egame.cmpSPP.T10 == 1, egame.SPP3.T10, NA),
      egame.SHP1.T10.NA = ifelse(egame.cmoSHP.T10 == 1, egame.SHP1.T10, NA),
      egame.SHP2.T10.NA = ifelse(egame.cmoSHP.T10 == 1, egame.SHP2.T10, NA),
      egame.SHP3.T10.NA = ifelse(egame.cmoSHP.T10 == 1, egame.SHP3.T10, NA),
      egame.TG1.T11.NA  = ifelse(egame.cmpTG.T11  == 1, egame.TG1.T11,  NA),
      egame.TG2.T11.NA  = ifelse(egame.cmpTG.T11  == 1, egame.TG2.T11,  NA),
      egame.PGG.T11.NA  = ifelse(egame.cmpPGG.T11 == 1, egame.PGG.T11,  NA),
      egame.DG.T11.NA   = ifelse(egame.cmpDG.T11  == 1, egame.DG.T11,   NA),
      egame.UG1.T11.NA  = ifelse(egame.cmpUG.T11  == 1, egame.UG1.T11,  NA),
      egame.UG2.T11.NA  = ifelse(egame.cmpUG.T11  == 1, egame.UG2.T11,  NA),
      egame.TPP1.T11.NA = ifelse(egame.cmpTPP.T11 == 1, egame.TPP1.T11, NA),
      egame.TPP2.T11.NA = ifelse(egame.cmpTPP.T11 == 1, egame.TPP2.T11, NA),
      egame.SPP1.T11.NA = ifelse(egame.cmpSPP.T11 == 1, egame.SPP1.T11, NA),
      egame.SPP2.T11.NA = ifelse(egame.cmpSPP.T11 == 1, egame.SPP2.T11, NA),
      egame.SPP3.T11.NA = ifelse(egame.cmpSPP.T11 == 1, egame.SPP3.T11, NA)
    ) %>%
    # ethnicity dummies
    dummy_cols(select_columns = "EthnicCats.T10")
  
  return(d)
}

ch3_filterData1 <- function(d) {
  d %>%
    # n = 1069
    # exclude participants according to criteria
    # keep only participants who were paid and did not time out in wave 1
    filter(egame.Chk1.T10 == 1 & egame.Chk2.T10 == 1) %>%
    # n = 1045
    # less than 5 mins or longer than 50 mins to complete games in wave 1
    filter(egame.SecsAll.T10 > 300 & egame.SecsAll.T10 < 3000) %>%
    # n = 1043
    # retain only if we have SDO and RWA scores for time 10
    filter(!is.na(SDO.T10) & !is.na(RWA.T10))
  # n = 991
}

ch3_filterData2 <- function(d) {
  d %>%
    # n = 1069
    # exclude participants according to criteria
    # keep only participants who were paid and did not time out in wave 2
    filter(egame.Chk1.T11 == 1 & egame.Chk2.T11 == 1) %>%
    # n = 631
    # less than 5 mins or longer than 50 mins to complete games in wave 2
    filter(egame.SecsAll.T11 > 300 & egame.SecsAll.T11 < 3000) %>%
    # n = 631
    # retain only if we have RWA scores for time 10
    filter(!is.na(RWA.T10))
  # n = 609
}

ch3_makeItemTable <- function() {
  tibble(
    Item = 
      c("SDO1", "SDO2", "SDO3", "SDO4 (reversed)", "SDO5 (reversed)", "SDO6 (reversed)", 
        "RWA1", "RWA2", "RWA3", "RWA4 (reversed)", "RWA5 (reversed)", "RWA6 (reversed)",
        "Income redistribution", "Environmental sacrifice",
        "Support for Jobseeker Support", "Support for Sole Parent Support",
        "Support for flat tax", "Income attribution",
        "Support for same-sex marriage", "Support for euthanasia",
        "Support for abortion (any reason)", "Support for abortion (specific reason)",
        "Support for religious instruction", "Race essentialism", "Sexual prejudice (reversed)",
        "Competitive worldview", "Age", "Gender", "Ethnicity", 
        "Education level", "Socio-economic status", "Local deprivation",
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
        "Are you willing to make sacrifices to your standard of living (e.g., accept higher prices, drive less, conserve energy) in order to protect the environment?",
        "Increase payments for those receiving Jobseeker Support (formerly the Unemployment Benefit)",
        "Increase payments for those receiving Sole Parent Support (formerly the Domestic Purposes Benefit)",
        "A `flat' tax rate (everyone pays the same percentage of tax on their income)",
        "If incomes were more equal, people would be less motivated to work hard",
        "Same-sex marriage in NZ (The Marriage Amendment Act 2013)",
        "Suppose a person has a painful incurable disease. Do you think that doctors should be allowed by law to end the patient's life if the patient requests it?",
        "Legalized abortion for women, regardless of the reason",
        "Legalized abortion when the woman's life is endangered",
        "Including religious instruction in Christianity as part of the school curriculum",
        "To a large extent, a person's race biologically determines his or her abilities",
        "I think that homosexuality should be accepted by society",
        "It's a dog-eat-dog world where you have to be ruthless at times",
        "What is your date of birth?",
        "What is your gender? (open-ended)",
        "Which ethnic group do you belong to? (NZ census question)",
        "NZ Reg (0-10 education ordinal rank)",
        "NZSEI13 (NZ Socio-economic index)",
        "Deprivation score 2013 (for Meshblock)",
        "Do you identify with a religion and/or spiritual group?"
      ),
    Wave = c(rep("10", 13), "9", "6", "6", "5", "10", "9", "9", 
             rep("10", 5), "6", rep("10", 7))
  )
}

ch3_makeSamplePlot <- function(d1) {
  # age
  fig.a <-
    d1 %>% drop_na(Age.T10) %>%
    ggplot(aes(x = Age.T10)) +
    geom_histogram(binwidth = 2, fill = "azure3") +
    labs(x = "Age (years)", y = NULL)
  # gender
  fig.b <-
    d1 %>% drop_na(Gender.T10) %>%
    mutate(Gender.T10 = factor(ifelse(Gender.T10 == 0, "F", "M"))) %>%
    ggplot(aes(x = Gender.T10)) +
    geom_bar(fill = "azure3") +
    labs(x = "Gender", y = NULL)
  # ethnicity
  fig.c <-
    d1 %>% drop_na(EthnicCats.T10) %>%
    mutate(EthnicCats.T10 = factor(EthnicCats.T10, levels = names(sort(table(EthnicCats.T10), decreasing = TRUE)))) %>%
    ggplot(aes(x = EthnicCats.T10)) +
    geom_bar(fill = "azure3") +
    labs(x = "Ethnicity", y = NULL) +
    scale_x_discrete(labels = function(x) EthnicCats.T10 = ifelse(x == "Maori", sprintf("M\u101ori"), ifelse(x == "Pakeha", sprintf("P\u101keh\u101"), x)))
  # education
  fig.d <-
    d1 %>% drop_na(NZREG.T10) %>%
    ggplot(aes(x = NZREG.T10)) +
    geom_histogram(binwidth = 1, fill = "azure3", col = "white") +
    scale_x_continuous(breaks = seq(0, 10, by = 2)) +
    labs(x = "Education", y = NULL)
  # ses
  fig.e <-
    d1 %>% drop_na(NZSEI13.T10) %>%
    ggplot(aes(x = NZSEI13.T10)) +
    geom_histogram(fill = "azure3") +
    labs(x = "SES", y = NULL)
  # deprivation
  fig.f <-
    d1 %>% drop_na(NZDepRAW.2013.T10) %>%
    ggplot(aes(x = NZDepRAW.2013.T10)) +
    geom_histogram(fill = "azure3") +
    labs(x = "Local deprivation", y = NULL)
  # religion
  fig.g <-
    d1 %>% drop_na(Religious.T10) %>%
    mutate(Religious.T10 = factor(ifelse(Religious.T10 == 0, "No", "Yes"))) %>%
    ggplot(aes(x = Religious.T10)) +
    geom_bar(fill = "azure3") +
    labs(x = "Religion", y = NULL)
  # SDO
  fig.h <-
    d1 %>% drop_na(SDO.T10) %>%
    ggplot(aes(x = SDO.T10)) +
    geom_histogram(fill = "azure3") +
    labs(x = "SDO", y = NULL)
  # RWA
  fig.i <-
    d1 %>% drop_na(RWA.T10) %>%
    ggplot(aes(x = RWA.T10)) +
    geom_histogram(fill = "azure3") +
    labs(x = "RWA", y = NULL)
  # plot heatmap of locations
  mesh <- 
    read.csv("data/meshblock-2018-centroid-inside.csv") %>%
    select(MB2018_V1_00, LATITUDE, LONGITUDE) %>%
    rename(MBU_2018.T10 = MB2018_V1_00) %>%
    mutate(MBU_2018.T10 = as.numeric(MBU_2018.T10))
  fig.j <-
    d1 %>% left_join(mesh, by = "MBU_2018.T10") %>%
    drop_na(MBU_2018.T10) %>%
    group_by(MBU_2018.T10, LONGITUDE, LATITUDE) %>%
    summarise(n = n()) %>%
    ggplot() + 
    geom_polygon(data = maps::map("nz", plot = FALSE), 
                 aes(x = long, y = lat, group = group),
                 fill = "white", colour = "black") +
    geom_point(aes(x = LONGITUDE, y = LATITUDE, size = n), 
               colour = "red", alpha = 0.3) +
    coord_fixed(1) +
    theme(axis.title = element_blank(),
          axis.text  = element_blank(),
          axis.ticks = element_blank(),
          axis.line  = element_blank(),
          legend.position = "none",
          plot.margin = margin(-0.5, 0, -0.5, 0)) +
    scale_size(range = c(1,2))
  # put together
  layout <-
    rbind(c(1, 1, 2, 3, 4, 4, 4, 4),
          c(5, 5, 6, 6, 4, 4, 4, 4),
          c(7, 7, 8, 8, 9, 9, 10, 10))
  y.grob <- textGrob("        Frequency", 
                     gp = gpar(col = "black", fontsize = 12), rot = 90)
  out <- 
    arrangeGrob(fig.a, fig.b, fig.g, fig.j,
                fig.c, fig.d, fig.e, fig.f,
                fig.h, fig.i, layout_matrix = layout)
  out <- arrangeGrob(out, left = y.grob)
  out <- ggdraw(out)
  return(out)
}

ch3_makeCor <- function(d1) {
  d1 %>%
    select(egame.DG.T10.NA, egame.TG1.T10.NA, egame.TG2.T10.NA, 
           egame.PGG.T10.NA, egame.SH.T10.NA, egame.UG2.T10.NA, 
           egame.TPP2.T10.NA, egame.SPP3.T10.NA, egame.SHP3.T10.NA) %>%
    rename(
      DG  = egame.DG.T10.NA,
      TG1 = egame.TG1.T10.NA,
      TG2 = egame.TG2.T10.NA,
      PGG = egame.PGG.T10.NA,
      SH  = egame.SH.T10.NA,
      UG  = egame.UG2.T10.NA,
      SPP = egame.TPP2.T10.NA,
      TPP = egame.SPP3.T10.NA,
      SHP = egame.SHP3.T10.NA
    ) %>%
    corr.test(use = "pairwise", method = "spearman", adjust = "BH")
}

ch3_runPCA <- function(d1, extraGames, nfactors, rotate) {
  cols <- c(
    "egame.DG.T10.NA",
    "egame.TG1.T10.NA",
    "egame.TG2.T10.NA",
    "egame.PGG.T10.NA",
    "egame.SH.T10.NA",
    "egame.UG2.T10.NA",
    "egame.TPP2.T10.NA",
    "egame.SPP3.T10.NA",
    "egame.SHP3.T10.NA"
  )
  if (!extraGames) cols <- cols[-c(5,9)]
  d1 <-  
    d1 %>%
    select(all_of(cols)) %>%
    drop_na()
  
  # bartlett's test:
  # cortest.bartlett(d)$p.value
  
  # KMO:
  # kmo(d)$overall %>% round(2)
  
  out <- principal(d1, nfactors = nfactors, rotate = rotate)
  return(out)
}

ch3_makeCorPcaPlot <- function(cor, pca1.2, pca2.2) {
  # figA
  figA <-
    ggcorrplot(
      cor$r, type = "lower",
      outline.col = "white",
      p.mat = cor$p,
      sig.level = 0.05,
      pch.col = "grey",
      legend.title = expression(r[s])
    ) +
    theme(legend.title = element_text(size = 14))
  # figB
  l <- as.data.frame(matrix(pca1.2$loadings, nrow = 7))
  colnames(l) <- c("Factor1", "Factor2")
  l$coop_game <- factor(c(rep(0, 4), rep(1, 3)))
  l$game <- c("DG", "TG1", "TG2", "PGG", "UG", "TPP", "SPP")
  figB <-
    ggplot(l, aes(x = Factor1, y = Factor2, 
                  label = game, colour = coop_game)) +
    geom_point(size = 1.2) +
    geom_text_repel(segment.colour = NA,
                    nudge_x = ifelse(l$game == "DG", 0.02, 0),
                    nudge_y = ifelse(l$game == "DG", -0.01, 0)) +
    scale_colour_manual(values=c("#0072B2", "#D55E00")) +
    xlim(c(-0.25,1)) +
    ylim(c(-0.25,1)) +
    labs(x = "Factor 1", y = "Factor 2") +
    theme(legend.position = "none")
  # figC
  l <- as.data.frame(matrix(pca2.2$loadings, nrow = 9))
  colnames(l) <- c("Factor2", "Factor1")
  l$coop_game <- factor(c(rep(0, 5), rep(1, 4)))
  l$game <- c("DG", "TG1", "TG2", "PGG", "SH", "UG", "TPP", "SPP", "SHP")
  figC <-
    ggplot(l, aes(x = Factor1, y = Factor2, 
                  label = game, colour = coop_game)) +
    geom_point(size = 1.2) +
    geom_text_repel(segment.colour = NA) +
    scale_colour_manual(values=c("#0072B2", "#D55E00")) +
    xlim(c(-0.25,1)) +
    ylim(c(-0.25,1)) +
    labs(x = "Factor 1", y = "Factor 2") +
    theme(legend.position = "none")
  # put together
  out <- plot_grid(figB, figC, labels = c("b","c"), nrow = 2)
  out <- plot_grid(figA, NULL, out,  labels = c("a","",""),  nrow = 1,
                   rel_widths = c(1, 0.05, 0.55))
  return(out)
}

ch3_makeScreePlot <- function(pca1.1, pca2.1) {
  plotFun <- function(pcaModel) {
    plotData <-
      tibble(
        Factor = 1:length(pcaModel$values),
        Eigenvalue = pcaModel$values
      )
    ggplot(plotData, aes(x = Factor, y = Eigenvalue)) +
      geom_point() +
      geom_line() +
      geom_hline(yintercept = 1, linetype = "dashed") +
      scale_y_continuous(limits = c(0, 2.5)) +
      scale_x_continuous(breaks = 1:nrow(plotData))
  }
  # plots
  pA <- plotFun(pca1.1)
  pB <- plotFun(pca2.1)
  # put together
  out <- plot_grid(pA, pB, nrow = 1, labels = letters[1:2])
  return(out)
}

ch3_runCFA <- function(d1, extraGames) {
  if (!extraGames) {
    model <- '# measurement model
              coop =~ egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10
              pun  =~ egame.UG2.T10 + egame.TPP2.T10 + egame.SPP3.T10
              # control for comprehension
              egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.UG2.T10 + egame.TPP2.T10 + egame.SPP3.T10 ~ egame.cmpOverall.T10
              '
    order <- c('egame.TG1.T10')
  } else {
    model <- '# measurement model
              coop =~ egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10
              pun  =~ egame.UG2.T10 + egame.TPP2.T10 + egame.SPP3.T10 + egame.SHP3.T10
              # control for comprehension
              egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10 + egame.UG2.T10 + egame.TPP2.T10 + egame.SPP3.T10 + egame.SHP3.T10~ egame.cmpOverall.T10
              '
    order <- c('egame.TG1.T10', 'egame.SH.T10')
  }
  out <- cfa(model, data = d1, ordered = order)
  return(out)
}

ch3_runSEM <- function(d1, controls = "") {
  model <- '# measurement model
            coop =~ egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10
            pun  =~ egame.UG2.T10 + egame.TPP2.T10 + egame.SPP3.T10 + egame.SHP3.T10
            # control for comprehension
            egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10 + egame.UG2.T10 + egame.TPP2.T10 + egame.SPP3.T10 + egame.SHP3.T10 ~ egame.cmpOverall.T10
            # regressions
            coop ~ SDO.T10.c + RWA.T10.c'
  model <- paste0(model, 
                  controls, 
                  "\n            pun  ~ SDO.T10.c + RWA.T10.c", 
                  controls)
  out <- sem(model, data = d1, ordered = c('egame.TG1.T10','egame.SH.T10'))
  return(out)
}

ch3_makeCoopPunPlot <- function(d1, sem1) {
  # add coop and pun to dataframe
  pred <- lavPredict(sem1)
  d1$coop <- pred[,1]
  d1$pun <- pred[,2]
  # plotA
  pA <-
    ggplot(d1, aes(x = SDO.T10, y = coop)) +
    geom_jitter(width = 0.1, colour = "#0072B2", alpha = 0.5) +
    stat_smooth(method = "lm", se = TRUE, colour = "black") +
    labs(x = "Social Dominance Orientation", y = "Cooperation")
  # plotB
  pB <-
    ggplot(d1, aes(x = RWA.T10, y = pun)) +
    geom_jitter(width = 0.1, colour = "#D55E00", alpha = 0.5) +
    stat_smooth(method = "lm", se = TRUE, colour = "black") +
    labs(x = "Right Wing Authoritarianism", y = "Punishment") +
    scale_y_continuous(breaks = c(-0.05, 0, 0.05), limits = c(-0.051, 0.051))
  # put together
  out <- plot_grid(pA, pB, nrow = 1, labels = letters[1:2])
  return(out)
}

ch3_makeSEMtable <- function(sem7) {
  parameterEstimates(sem7) %>%
    filter(op == "~" & rhs != "egame.cmpOverall.T10") %>%
    select(est:pvalue) %>%
    mutate(
      DV = c("Cooperation", rep("", 8), "Punishment", rep("", 8)),
      IV = rep(c("SDO", "RWA", "Age", "Gender (Male = 1)", "Ethnicity (P\\={a}keh\\={a})", "Education", "SES", "Deprivation", "Religious (Yes = 1)"), 2)
    ) %>%
    select(DV, IV, everything()) %>%
    rename(Estimate = est, SE = se, p = pvalue)
}

ch3_semiPartialR1 <- function(d1, prevModel, controls, dv, replace, with) {
  model <- '# measurement model
            coop =~ egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10
            pun  =~ egame.UG2.T10 + egame.TPP2.T10 + egame.SPP3.T10 + egame.SHP3.T10
            # control for comprehension
            egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10 + egame.UG2.T10 + egame.TPP2.T10 + egame.SPP3.T10 + egame.SHP3.T10 ~ egame.cmpOverall.T10
            # regressions
            coop ~ SDO.T10.c + RWA.T10.c'
  model <- paste0(model, 
                  controls, 
                  "\n            pun  ~ SDO.T10.c + RWA.T10.c", 
                  controls)
  model <- str_replace(model, replace, with)
  m <- sem(model, data = d1, ordered = c('egame.TG1.T10','egame.SH.T10'))
  out <- sqrt(inspect(prevModel, "r2")[dv] - inspect(m, "r2")[dv]) %>% as.numeric()
  out <- ifelse(identical(out, NaN), 0, out)
  return(out)
}

ch3_fitIndGames <- function(d1, formula) {
  list(
    dg_null  = lm(as.formula(paste0(formula, " + egame.cmpDG.T10                   + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    dg       = lm(as.formula(paste0(formula, " + egame.DG.T10 + egame.cmpDG.T10    + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    tg_null  = lm(as.formula(paste0(formula, " + egame.cmpTG.T10                   + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    tg1      = lm(as.formula(paste0(formula, " + egame.TG1.T10 + egame.cmpTG.T10   + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    tg2      = lm(as.formula(paste0(formula, " + egame.TG2.T10 + egame.cmpTG.T10   + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    pgg_null = lm(as.formula(paste0(formula, " + egame.cmpPGG.T10                  + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    pgg      = lm(as.formula(paste0(formula, " + egame.PGG.T10 + egame.cmpPGG.T10  + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    sh_null  = lm(as.formula(paste0(formula, " + egame.cmpSH.T10                   + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    sh       = lm(as.formula(paste0(formula, " + egame.SH.T10 + egame.cmpSH.T10    + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    ug_null  = lm(as.formula(paste0(formula, " + egame.cmpUG.T10                   + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    ug1      = lm(as.formula(paste0(formula, " + egame.UG1.T10 + egame.cmpUG.T10   + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    ug2      = lm(as.formula(paste0(formula, " + egame.UG2.T10 + egame.cmpUG.T10   + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    tpp_null = lm(as.formula(paste0(formula, " + egame.cmpTPP.T10                  + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    tpp1     = lm(as.formula(paste0(formula, " + egame.TPP1.T10 + egame.cmpTPP.T10 + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    tpp2     = lm(as.formula(paste0(formula, " + egame.TPP2.T10 + egame.cmpTPP.T10 + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    spp_null = lm(as.formula(paste0(formula, " + egame.cmpSPP.T10                  + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    spp1     = lm(as.formula(paste0(formula, " + egame.SPP1.T10 + egame.cmpSPP.T10 + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    spp2     = lm(as.formula(paste0(formula, " + egame.SPP2.T10 + egame.cmpSPP.T10 + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    spp3     = lm(as.formula(paste0(formula, " + egame.SPP3.T10 + egame.cmpSPP.T10 + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    shp_null = lm(as.formula(paste0(formula, " + egame.cmoSHP.T10                  + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    shp1     = lm(as.formula(paste0(formula, " + egame.SHP1.T10 + egame.cmoSHP.T10 + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    shp2     = lm(as.formula(paste0(formula, " + egame.SHP2.T10 + egame.cmoSHP.T10 + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1),
    shp3     = lm(as.formula(paste0(formula, " + egame.SHP3.T10 + egame.cmoSHP.T10 + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")), d1)
  )
}

ch3_semiPartialR2 <- function(i) {
  list(
    dg   = sqrt(summary(i[["dg"  ]])$r.squared - summary(i[["dg_null" ]])$r.squared),
    tg1  = sqrt(summary(i[["tg1" ]])$r.squared - summary(i[["tg_null" ]])$r.squared),
    tg2  = sqrt(summary(i[["tg2" ]])$r.squared - summary(i[["tg_null" ]])$r.squared),
    pgg  = sqrt(summary(i[["pgg" ]])$r.squared - summary(i[["pgg_null"]])$r.squared),
    sh   = sqrt(summary(i[["sh"  ]])$r.squared - summary(i[["sh_null" ]])$r.squared),
    ug1  = sqrt(summary(i[["ug1" ]])$r.squared - summary(i[["ug_null" ]])$r.squared),
    ug2  = sqrt(summary(i[["ug2" ]])$r.squared - summary(i[["ug_null" ]])$r.squared),
    tpp1 = sqrt(summary(i[["tpp1"]])$r.squared - summary(i[["tpp_null"]])$r.squared),
    tpp2 = sqrt(summary(i[["tpp2"]])$r.squared - summary(i[["tpp_null"]])$r.squared),
    spp1 = sqrt(summary(i[["spp1"]])$r.squared - summary(i[["spp_null"]])$r.squared),
    spp2 = sqrt(summary(i[["spp2"]])$r.squared - summary(i[["spp_null"]])$r.squared),
    spp3 = sqrt(summary(i[["spp3"]])$r.squared - summary(i[["spp_null"]])$r.squared),
    shp1 = sqrt(summary(i[["shp1"]])$r.squared - summary(i[["shp_null"]])$r.squared),
    shp2 = sqrt(summary(i[["shp2"]])$r.squared - summary(i[["shp_null"]])$r.squared),
    shp3 = sqrt(summary(i[["shp3"]])$r.squared - summary(i[["shp_null"]])$r.squared)
  )
}

ch3_makeIndPlotGrid <- function(d1, i, pol) {
  indPlot <- function(d1, model, pol, game, type, title) {
    # get coefficients, significance, and predictions for plot
    coef <- summary(model)$coefficients[3,1]
    sig <- ifelse(summary(model)$coefficients[3,4]<(0.05/15), TRUE, FALSE)
    pred <- data.frame(x = seq(0, 1, length.out = 10001))
    pred$y <- model$coefficients[[1]] + model$coefficients[[3]]*pred$x
    # plot
    d1 %>%
      ggplot(aes_string(x = game, y = pol)) +
      geom_jitter(width = 0.05, height = 0.1, alpha = 0.05, size = 0.3,
                  colour = ifelse(type == "coop", "#0072B2", "#D55E00")) +
      geom_line(aes(x = x, y = y), data = pred) +
      annotate("text", label = paste0("b = ",
                                      format(round(coef, 2), nsmall = 2), 
                                      ifelse(sig, "*", "")), x = 0.8, y = 7, size = 2.5) +
      scale_x_continuous(name = NULL, limits = c(-0.05, 1.05), breaks = c(0, 0.5, 1)) +
      scale_y_continuous(name = NULL, limits = c(0.9, 7.1), breaks = 1:7) +
      ggtitle(NULL, subtitle = title) +
      theme(axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 7))
  }
  # get all plots
  pA <- indPlot(d1, i[["dg"  ]], pol, "egame.DG.T10",   "coop", "DG")
  pB <- indPlot(d1, i[["tg1" ]], pol, "egame.TG1.T10",  "coop", "TG1")
  pC <- indPlot(d1, i[["tg2" ]], pol, "egame.TG2.T10",  "coop", "TG2")
  pD <- indPlot(d1, i[["pgg" ]], pol, "egame.PGG.T10",  "coop", "PGG")
  pE <- indPlot(d1, i[["sh"  ]], pol, "egame.SH.T10",   "coop", "SH")
  pF <- indPlot(d1, i[["ug1" ]], pol, "egame.UG1.T10",  "coop", "UG1")
  pG <- indPlot(d1, i[["tpp1"]], pol, "egame.TPP1.T10", "coop", "TPP1")
  pH <- indPlot(d1, i[["spp1"]], pol, "egame.SPP1.T10", "coop", "SPP1")
  pI <- indPlot(d1, i[["shp1"]], pol, "egame.SHP1.T10", "coop", "SHP1")
  pJ <- indPlot(d1, i[["ug2" ]], pol, "egame.UG2.T10",  "pun" , "UG2")
  pK <- indPlot(d1, i[["tpp2"]], pol, "egame.TPP2.T10", "pun" , "TPP2")
  pL <- indPlot(d1, i[["spp2"]], pol, "egame.SPP2.T10", "pun" , "SPP2")
  pM <- indPlot(d1, i[["spp3"]], pol, "egame.SPP3.T10", "pun" , "SPP3")
  pN <- indPlot(d1, i[["shp2"]], pol, "egame.SHP2.T10", "pun" , "SHP2")
  pO <- indPlot(d1, i[["shp3"]], pol, "egame.SHP3.T10", "pun" , "SHP3")
  out <- plot_grid(pA, pB, pC, pJ, pK,
                   pD, pE, pF, pL, pM,
                   pG, pH, pI, pN, pO, nrow = 3)
  # add common x-axis
  x.grob <- textGrob(ifelse(grepl("SDO", pol),
                            "Social Dominance Orientation",
                            "Right Wing Authoritarianism"), 
                     gp = gpar(fontsize = 10), rot = 90)
  out <- grid.arrange(arrangeGrob(out, left = x.grob))
  out <- ggdraw(out)
  return(out)
}

ch3_fitPolicy <- function(d, variable, coop, pun) {
  model <- '# measurement model
            coop =~ egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10
            pun  =~ egame.UG2.T10 + egame.TPP2.T10 + egame.SPP3.T10 + egame.SHP3.T10
            # control for comprehension
            egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10 + egame.UG2.T10 + egame.TPP2.T10 + egame.SPP3.T10 + egame.SHP3.T10 ~ egame.cmpOverall.T10
            # regressions
            '
  model <- paste0(model, variable, " ~ ", coop, pun, "Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")
  out <- sem(model, data = d, ordered = c('egame.TG1.T10','egame.SH.T10',variable))
  return(out)
}

ch3_semiPartialR3 <- function(d1, policy, coop, pun) {
  m1 <- policy
  m2 <- ch3_fitPolicy(d1, parameterEstimates(policy)[19,1], coop, pun)
  out <- sqrt(
    inspect(m1, "R2")[[parameterEstimates(policy)[19,1]]] -
      inspect(m2, "R2")[[parameterEstimates(policy)[19,1]]])
  out <- ifelse(identical(out, NaN), 0, out)
  return(out)
}

ch3_makePolicyTable <- function(model) {
  parameterEstimates(model) %>%
    filter(op == "~" & rhs != "egame.cmpOverall.T10") %>%
    select(est:pvalue) %>%
    mutate(
      IV = c("Cooperation", "Punishment", "Age", "Gender (Male = 1)", "Ethnicity (P\\={a}keh\\={a})", "Education", "SES", "Deprivation", "Religious (Yes = 1)")
    ) %>%
    select(IV, everything()) %>%
    rename(Estimate = est, SE = se, p = pvalue)
}

ch3_makePolicyGrid <- function(d1, sem1) {
  # add coop and pun to dataframe
  pred <- lavPredict(sem1)
  d1$coop <- pred[,1]
  d1$pun <- pred[,2]
  # define plotting function
  policyPlot <- function(d1, policy, ylab) {
    d1 <- d1 %>% drop_na(all_of(policy))
    d1 %>%
      ggplot(aes_string(x = "coop", y = policy)) +
      geom_jitter(height = 0.2, alpha = 0.15, colour = "#0072B2", size = 0.4) +
      stat_smooth(method = "lm", se = TRUE, colour = "black") +
      scale_y_continuous(name = ylab, breaks = 1:7) +
      xlab(NULL) +
      theme(axis.title.x = element_text(size = 9.5, margin = margin(r = 5)))
  }
  # plots
  pA <- policyPlot(d1, "Issue.IncomeRedistribution.T10", "Preference\nfor income\nredistribution (1-7)")
  pB <- policyPlot(d1, "Env.SacWilling.T09", "Willingness to\nsacrifice for the\nenvironment (1-7)")
  pC <- policyPlot(d1, "Issue.Payments.Jobseeker.T06", "Increased\nJobseeker Support\npayments (1-7)")
  pD <- policyPlot(d1, "Issue.Payments.SoleParent.T06", "Increased Sole\nParent Support\npayments (1-7)")
  pE <- policyPlot(d1, "Issue.TaxPolicy.T05", "Support for\n'flat tax'\nsystem (1-7)")
  pF <- policyPlot(d1, "IncomeAttribution.T10", "'Income equality\nreduces motivation\nto work hard' (1-7)")
  # put together
  out <- plot_grid(pA, pB, pC, 
                   pD, pE, pF, nrow = 3, labels = letters[1:6])
  # add common x-axis
  x.grob <- textGrob("Cooperation", gp = gpar(fontsize = 12))
  out <- grid.arrange(arrangeGrob(out, bottom = x.grob))
  out <- ggdraw(out)
  return(out)
}

ch3_makePolicyR2Grid <- function(d1) {
  getR2 <- function(outcome, predictors) {
    model <- '# measurement model
              coop =~ egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10
              pun  =~ egame.UG2.T10 + egame.TPP2.T10 + egame.SPP3.T10 + egame.SHP3.T10
              # control for comprehension
              egame.PGG.T10 + egame.DG.T10 + egame.TG1.T10 + egame.TG2.T10 + egame.SH.T10 + egame.UG2.T10 + egame.TPP2.T10 + egame.SPP3.T10 + egame.SHP3.T10 ~ egame.cmpOverall.T10
              # regressions
              '
    model <- paste0(model, outcome, " ~ ", predictors)
    model <- sem(model, data = d1, ordered = c('egame.TG1.T10', 'egame.SH.T10', outcome))
    out <- inspect(model, "R2")[[outcome]]
    return(out)
  }
  outcomes <- c("Issue.IncomeRedistribution.T10", "IncomeAttribution.T10", "Env.SacWilling.T09",
                "Issue.SameSexMarriage.T09", "Issue.Euthanasia.T09", "Issue.Abortion.AnyReason.T10",
                "Issue.Payments.Jobseeker.T06", "Issue.Payments.SoleParent.T06", "Issue.TaxPolicy.T05")
  Outcomes <- c("Income Redistribution", "Income Attribution", "Sacrifice for Environment",
                "Same Sex Marriage", "Euthanasia", "Abortion",
                "Jobseeker Payments", "Sole Parent Payments", "Tax Policy")
  predictors <- c("coop", "pun", "Age.T10.c", "Gender.T10", "EthnicCats.T10_Pakeha",
                  "NZREG.T10.c", "NZSEI13.T10.c", "NZDepRAW.2013.T10.c", "Religious.T10",
                  "coop + pun + Age.T10.c + Gender.T10 + EthnicCats.T10_Pakeha + NZREG.T10.c + 
                  NZSEI13.T10.c + NZDepRAW.2013.T10.c + Religious.T10")
  Predictors <- c("Cooperation", "Punishment", "Age", "Gender", "Ethnicity", 
                  "Education", "SES", "Deprivation", "Religion", "Full model")
  plotData <-
    tibble(
      outcome   = rep(outcomes, each = length(predictors)),
      predictor = rep(predictors, times = length(outcomes)),
      dimension = rep(c(rep("econ", 3), rep("social", 3), rep("econ", 3)), each = length(predictors))
    ) %>%
    mutate(
      r.squared = map2(outcome, predictor, getR2),
      outcome   = factor(rep(Outcomes, each = length(Predictors)), levels = Outcomes),
      predictor = factor(rep(Predictors, times = length(Outcomes)), levels = Predictors)
    ) %>% 
    unnest(r.squared)
  # plotting function
  plotR2 <- function(plotData, dim) {
    plotData %>%
      filter(dimension == dim) %>%
      ggplot(aes(x = predictor, y = r.squared)) +
      geom_bar(stat = "identity", colour = "steelblue4", fill = "steelblue4", width = 0.7) +
      facet_wrap(.~outcome, nrow= 3) +
      scale_y_continuous(name = bquote(R^2), breaks = c(0, 0.1, 0.2), limits = c(0, 0.28)) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title.x = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines")
      )
  }
  # plot
  pA <- plotR2(plotData, "econ")   # econ plot
  pB <- plotR2(plotData, "social") # social plot
  # put together
  out <- plot_grid(pA, NULL, pB, nrow = 1, labels = c("a", "", "b"),
                   rel_widths = c((2/3) - 0.02, 0.04, (1/3) - 0.02))
  return(out)
}

ch3_rfHist <- function(d2) {
  d2 %>%
    mutate(egame.RFT_Sum.T11 = 30 - egame.RFT_Sum.T11) %>%
    ggplot(aes(x = egame.RFT_Sum.T11)) +
    geom_histogram(binwidth = 1) +
    labs(x = "Number of times rule followed", y = "Frequency")
}

ch3_rfCor <- function(d2) {
  d2$egame.RFT_Sum.T11 <- 30 - d2$egame.RFT_Sum.T11
  cor.test(d2$RWA.T10, d2$egame.RFT_Sum.T11, method = "spearman")
}

ch3_rfData <- function(d2) {
  d2 %>%
    # select only relevant variables
    select(Questionnaire.Num, starts_with("egame.RFT"), -egame.RFT_Sum.T11,
           Gender.T10, Age.T10.c, NZDepRAW.2013.T10.c, EthnicCats.T10, NZREG.T10.c,
           NZSEI13.T10.c, SDO.T10.c, RWA.T10.c) %>%
    # pivot to long data frame
    pivot_longer(cols = starts_with("egame.RFT"),
                 names_to = "round",
                 values_to = "ruleBreak") %>%
    # ruleFollow and round number
    mutate(ruleFollow = 1 - ruleBreak,
           round = (rep(1:30, times = nrow(.) / 30)) / 30)
}

ch3_fitRFModel <- function(d2RF, formula) {
  # fit model
  out <- brm(formula = formula, data = d2RF,
             family = bernoulli,
             prior = c(prior(normal(0, 0.5), class = b),
                       prior(exponential(3), class = sd),
                       prior(lkj_corr_cholesky(2), class = L)),
             iter = 3000, cores = 4)
  return(out)
}

ch3_fitPolicyRF <- function(d2, formula) {
  # get rf score between 0 and 1
  d2 <- mutate(d2, rf = (30 - egame.RFT_Sum.T11) / 30)
  # fit model
  out <- brm(formula, data = d2, family = cumulative,
             prior = c(prior(normal(0, 1.5), class = Intercept),
                       prior(normal(0, 0.5), class = b)), 
             cores = 4)
  return(out)
}

ch3_beastData <- function(d2) {
  d2 %>%
    # pivot to long data frame
    pivot_longer(cols = starts_with("egame.BEASTR"),
                 names_to = "round",
                 values_to = "bScore") %>%
    # extract round number
    mutate(round = rep(1:5, times = nrow(.) / 5)) %>%
    # drop missing scores
    drop_na(bScore) %>%
    # for modelling: 0 -> 0.000001
    mutate(bScore = ifelse(bScore == 0, 0.000001, bScore)) %>%
    # for modelling: 1 -> 0.999999
    mutate(bScore = ifelse(bScore == 1, 0.999999, bScore))
}

ch3_beastHist <- function(d2) {
  d2 %>%
    select(starts_with("egame.BEASTR") & ends_with("score.T11")) %>%
    mutate(bScore = rowMeans(., na.rm = T)) %>%
    drop_na(bScore) %>%
    ggplot(aes(x = bScore)) +
    geom_histogram(binwidth = 0.05) +
    labs(x = "Mean BEAST score (0 = no shift, 1 = complete shift)", y = "Frequency")
}

ch3_getBeastImages <- function() {
  getBeastImage <- function(file) ggdraw() + draw_image(image_read(file), scale = 0.9)
  pA <- getBeastImage("images/ch3/beastImages/ants.jpg")
  pB <- getBeastImage("images/ch3/beastImages/bees.jpg")
  pC <- getBeastImage("images/ch3/beastImages/flamingos.jpg")
  pD <- getBeastImage("images/ch3/beastImages/cranes.jpg")
  pE <- getBeastImage("images/ch3/beastImages/crickets.jpg")
  out <- 
    plot_grid(
      plot_grid(pA, pB, nrow = 1, labels = c("a","b")),
      plot_grid(pC, pD, nrow = 1, labels = c("c","d")),
      plot_grid(NULL, pE, NULL, rel_widths = c(0.25, 0.5, 0.25), nrow = 1, labels = c("","e","")),
      nrow = 3
    )
  return(out)
}

ch3_fitBEASTModel <- function(d2BEAST, formula) {
  # fit model
  out <- brm(formula = formula, data = d2BEAST,
             family = Beta,
             prior = c(prior(normal(0, 1), class = b),
                       prior(exponential(1), class = sd)),
             iter = 3000, cores = 4, 
             control = list(adapt_delta = 0.99),
             save_all_pars = TRUE)
  return(out)
}

ch3_plotRFBEAST <- function(d2, beast1, rf1) {
  # fitted function
  getFitted <- function(model) {
    newdata <- data.frame(RWA.T10.c = seq(from = min(d2$RWA.T10.c), 
                                          to = max(d2$RWA.T10.c), 
                                          length.out = 1000),
                          round = 1/30) # first round
    fitted(model, newdata = newdata, re_formula = NA) %>%
      as_tibble() %>%
      mutate(RWA.T10.c = newdata$RWA.T10.c,
             RWA.T10 = RWA.T10.c + 2.949277)
  }
  # add vars to d2
  d2 <- mutate(d2, rf = (30 - d2$egame.RFT_Sum.T11) / 30,
               bScore = rowMeans(select(d2, starts_with("egame.BEASTR")), na.rm = TRUE))
  # plot BEAST
  fittedBEAST <- getFitted(beast1)
  pA <-
    ggplot() +
    geom_jitter(data = d2, aes(x = RWA.T10, y = bScore),
                alpha = 0.5, height = 0.02, width = 0.05, colour = "#30BE00") +
    geom_ribbon(data = fittedBEAST, aes(x = RWA.T10, ymin = `Q2.5`, ymax = `Q97.5`),
                fill = "grey", alpha = 0.5) +
    geom_line(data = fittedBEAST, aes(x = RWA.T10, y = Estimate), colour = "black", size = 1) +
    scale_x_continuous(name = "Right Wing Authoritarianism", limits = c(1, 7), breaks = c(2, 4, 6)) +
    scale_y_continuous(name = "BEAST Score\n(0 = no shift, 1 = complete shift)")
  # plot RF
  fittedRF <- 
    getFitted(rf1) %>%
    mutate(Estimate = Estimate,
           Q2.5 = Q2.5,
           Q97.5 = Q97.5)
  pB <-
    ggplot() +
    geom_jitter(data = d2, aes(x = RWA.T10, y = rf), 
                alpha = 0.5, height = 0.02, width = 0.05, colour = "#C9B500") +
    geom_ribbon(data = fittedRF, aes(x = RWA.T10, ymin = `Q2.5`, ymax = `Q97.5`),
                fill = "grey", alpha = 0.5) +
    geom_line(data = fittedRF, aes(x = RWA.T10, y = Estimate), colour = "black", size = 1) +
    scale_x_continuous(name = "Right Wing Authoritarianism", limits = c(1, 7), breaks = c(2, 4, 6)) +
    scale_y_continuous(name = "Rule Following Proportion\n(0 = never follow, 1 = always follow)")
  # put together
  out <- plot_grid(pA, pB, nrow = 1, labels = letters[1:2])
  return(out)
}

ch3_fitPolicyBEAST <- function(d2, formula) {
  # get average beast score and drop NAs
  d2 <- 
    d2 %>%
    mutate(bScore = rowMeans(select(., all_of(paste0("egame.BEASTR", 1:5, "score.T11"))), na.rm = TRUE)) %>%
    drop_na(bScore)
  # fit model
  out <- brm(formula, data = d2, family = cumulative,
             prior = c(prior(normal(0, 1.5), class = Intercept),
                       prior(normal(0, 0.5), class = b)), 
             cores = 4)
  return(out)
}

ch3_makePolicyGridBEAST <- function(d2, mA, mB, mC, mD, mE) {
  # get average BEAST scores
  d2 <- mutate(d2, bScore = rowMeans(select(d2, all_of(paste0("egame.BEASTR", 1:5, "score.T11"))), na.rm = TRUE))
  # define plotting function
  policyPlot <- function(d2, model, policy, ylab) {
    cond <- as.data.frame(conditional_effects(model)$bScore)
    out <-
      d2 %>%
      drop_na(all_of(policy)) %>%
      ggplot(aes_string(x = "bScore", y = policy)) +
      geom_jitter(height = 0.2, width = 0.005, alpha = 0.15, colour = "#30BE00", size = 0.4) +
      geom_ribbon(data = cond, aes(x = bScore, ymin = lower__, ymax = upper__), fill = "grey", alpha = 0.6) +
      geom_line(data = cond, aes(x = bScore, y = estimate__), colour = "black") +
      scale_y_continuous(name = ylab, breaks = 1:7) +
      xlab(NULL) +
      theme(axis.title.x = element_text(size = 9.5, margin = margin(r = 5)))
    return(out)
  }
  # plots
  pA <- policyPlot(d2, mA, "Issue.Abortion.AnyReason.T10", "Support for legalised\nabortion for women\nfor any reason (1-7)")
  pB <- policyPlot(d2, mB, "Issue.Abortion.SpecificReason.T10", "Support for legalised\nabortion for women when\nlife endangered (1-7)")
  pC <- policyPlot(d2, mC, "Issue.ReligiousEd.T10", "Support for religious\ninstruction in\nschool curriculum (1-7)")
  pD <- policyPlot(d2, mD, "RaceEssent.T10", "'A person's race\nbiologically determines\ntheir abilities' (1-7)")
  pE <- policyPlot(d2, mE, "SexualPrejudice01r.T10", "Prejudice against\nhomosexuality\n(1-7)")
  # put together
  row1    <- plot_grid(pA, pB, nrow = 1, labels = letters[1:2])
  row2    <- plot_grid(pC, pD, nrow = 1, labels = letters[3:4])
  row3    <- plot_grid(NULL, pE, NULL, nrow = 1, labels = c("", letters[5], ""),
                       rel_widths = c(0.5, 1, 0.5))
  out <- plot_grid(row1, row2, row3, nrow = 3)
  # add common x-axis
  x.grob <- textGrob("BEAST score", gp = gpar(fontsize = 12))
  out <- grid.arrange(arrangeGrob(out, bottom = x.grob))
  out <- ggdraw(out)
  return(out)
}

ch3_makeRFtable <- function(rf9) {
  summary(rf9)$fixed %>%
    as_tibble() %>%
    select(1:4) %>%
    mutate(
      IV = c("Intercept", "Round number", "RWA", "SDO", "Gender (Male = 1)", "Age", 
             "Deprivation", "Ethnicity (M\\={a}ori)", "Ethnicity (Pacific)", "Ethnicity (P\\={a}keh\\={a})", 
             "Education", "SES")
    ) %>%
    select(IV, everything()) %>%
    rename(SE = Est.Error, `Lower 95% CI` = `l-95% CI`, `Upper 95% CI` = `u-95% CI`)
}

ch3_makeBEASTtable <- function(beast9) {
  summary(beast9)$fixed %>%
    as_tibble() %>%
    select(1:4) %>%
    mutate(
      IV = c("Intercept", "RWA", "SDO", "Gender (Male = 1)", "Age", 
             "Deprivation", "Ethnicity (M\\={a}ori)", "Ethnicity (Pacific)", 
             "Ethnicity (P\\={a}keh\\={a})","Education", "SES")
    ) %>%
    select(IV, everything()) %>%
    rename(SE = Est.Error, `Lower` = `l-95% CI`, `Upper` = `u-95% CI`)
}

ch3_makeBEASTpolicyTable <- function(model) {
  summary(model)$fixed %>%
    as_tibble() %>%
    select(1:4) %>%
    mutate(
      IV = c(paste0("Cutpoint ", 1:6), "BEAST score", "Gender (Male = 1)", "Age", 
             "Deprivation", "Ethnicity (M\\={a}ori)", "Ethnicity (Pacific)", 
             "Ethnicity (P\\={a}keh\\={a})","Education", "SES")
    ) %>%
    select(IV, everything()) %>%
    rename(SE = Est.Error, `Lower` = `l-95% CI`, `Upper` = `u-95% CI`)
}
