# custom functions

# set theme for thesis
theme_set(theme_classic())

ch2_createFigUniMulti <- function() {
  read.csv('data/twoDimensionsReview.csv') %>%
    as_tibble() %>%
    mutate(Source = fct_relevel(Source, 'Scopus', 'Wall Street Journal'),
           Type   = fct_relevel(Type, 'Unidimensionality')) %>%
    ggplot(aes(x = Source, y = Count, fill = Type)) +
    geom_bar(stat = 'identity', position = position_dodge()) +
    scale_fill_manual(values=c("#00497D", "#FFB682")) +
    labs(x = NULL) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 100.25)) +
    scale_x_discrete(name = NULL, 
                     breaks = c('Scopus', 'Wall Street Journal', 'New York Times'), 
                     labels = c('Scopus', 'Wall Street\nJournal', 'New York\nTimes')) +
    theme(legend.position = c(0.75, 0.9),
          legend.title = element_blank(),
          axis.ticks.x = element_blank())
}

ch2_createTabScales <- function() {
  data.frame(`Cooperation dimension` = 
               c(
                 "Economic conservatism",
                 "Social dominance\norientation",
                 "Tough vs. tender",
                 "Humanism",
                 "Equality",
                 "Power distance",
                 "Liberalism (i.e., humanism-egalitarianism)",
                 "Idealism (altruism-social concern)",
                 "Humanitarianism-egalitarianism",
                 "Economic conservatism vs. equality",
                 "Hierarchy vs. egalitarianism",
                 "International harmony",
                 "Self-enhancement vs. transcendence",
                 "Vertical vs. horizontal values",
                 "Unmitigated self-interest (`beta-isms')",
                 "Competition vs. compassion",
                 "Egalitarianism",
                 "Humanitarianism",
                 "Capitalist vs. socialist",
                 "Tolerance of inequality",
                 "Individualising (care-harm, fairness-reciprocity)"
               ), 
             `Group conformity dimension` = 
               c(
                 "Social conservatism",
                 "Right-wing authoritarianism",
                 "Conservatism vs. liberalism",
                 "Normativism (conservatism)",
                 "Freedom",
                 "Collectivism vs. individualism",
                 "Conservatism",
                 "Relativism (i.e., group orientation)",
                 "Protestant ethic",
                 "Cultural conservatism vs. openness",
                 "Group loyalty vs. individualism",
                 "National strength and order",
                 "Conservation vs. openness",
                 "Collectivism vs. individualism",
                 "Tradition-oriented religiousness (`alpha-isms')",
                 "Moral regulation vs. individual freedom",
                 "Conservatism",
                 "Religiosity",
                 "Religious vs. secular",
                 "Opposition to change",
                 "Binding (authority-respect, in-group-loyalty, purity-sanctity)"
               ),
             Reference = 
               c(
                 "(ref:ch2ref01)",
                 "(ref:ch2ref02)",
                 "(ref:ch2ref03)",
                 "(ref:ch2ref04)",
                 "(ref:ch2ref05)",
                 "(ref:ch2ref06)",
                 "(ref:ch2ref07)",
                 "(ref:ch2ref08)",
                 "(ref:ch2ref09)",
                 "(ref:ch2ref10)",
                 "(ref:ch2ref11)",
                 "(ref:ch2ref12)",
                 "(ref:ch2ref13)",
                 "(ref:ch2ref14)",
                 "(ref:ch2ref15)",
                 "(ref:ch2ref16)",
                 "(ref:ch2ref17)",
                 "(ref:ch2ref18)",
                 "(ref:ch2ref19)",
                 "(ref:ch2ref20)",
                 "(ref:ch2ref21)"
               )
  )
}

ch2_createTabScaleItems <- function() {
  data.frame(`Cooperation dimension` = 
               c(
                 "(ref:ch2ref22)",
                 "Do you think... there should be a government insurance plan that would cover all medical and hospital expenses for everyone?",
                 "... the government should provide fewer services, even in areas such as health and education, to reduce spending?",
                 "... the government in Washington should see to it that every person has a job and a good standard of living?",
                 "(ref:ch2ref24)",
                 "Some groups of people are simply inferior to other groups.",
                 "It's OK if some groups have more of a chance in life than others.",
                 "It's probably a good thing that certain groups are at the top and other groups are at the bottom.",
                 "(ref:ch2ref26)",
                 "Machiavellianism: craft and deceit are justified in pursuing and maintaining power in the political world.",
                 "Materialism: physical well-being and worldly possessions are the greatest good and highest value in life.",
                 "Solipsism: the self is the only reality.",
                 "(ref:ch2ref28)",
                 "Equality (equal opportunity for all)",
                 "Unity with nature (fitting into nature)",
                 "Helpful (working for the welfare of others)",
                 "(ref:ch2ref30)",
                 "Compassion for those who are suffering is the most crucial virtue.",
                 "When the government makes laws, the number one principle should be ensuring that everyone is treated fairly.",
                 "I think it's morally wrong that rich children inherit a lot of money while poor children inherit nothing."
               ), 
             `Group conformity dimension` = 
               c(
                 "(ref:ch2ref23)",
                 "Do you think... gay or lesbian couples (in other words, homosexual couples) should be legally permitted to adopt children?",
                 "... a woman's place is in the home?",
                 "... abortion should never be permitted?",
                 "(ref:ch2ref25)",
                 "What our country really needs is a strong, determined leader who will crush evil and take us back to our true path.",
                 "The `old-fashioned ways' and the `old-fashioned values' still show the best way to live.",
                 "God's laws about abortion, pornography and marriage must be strictly followed before it is too late, and those who break them must be strongly punished.",
                 "(ref:ch2ref27)",
                 "Legalism: I adhere strictly and literally to a code of religion and morality.",
                 "Ecclesiasticism: I am devoted to the principles and interests of the church.",
                 "Traditionalism: I adhere to tradition, especially in cultural and religious practice.",
                 "(ref:ch2ref29)",
                 "Obedient (dutiful, meeting obligations)",
                 "National security (protection of my nation from enemies)",
                 "Respect for tradition (preservation of time-honoured customs)",
                 "(ref:ch2ref31)",
                 "People should be loyal to their family members even when they have done something wrong.",
                 "Respect for authority is something all children need to learn.",
                 "What matters is whether or not someone's action showed love for his or her country."
               )
  )
}
