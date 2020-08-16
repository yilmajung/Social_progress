library(haven)
library(tidyverse)
library(randomForest)
library(cowplot)
library(glmnet)
library(mice)
library(lme4)
library(lmerTest)
library(plm)
library(wesanderson)
library(forcats)

gss <- read_dta("dataset/gss_data_aqatermpaper_addsex.dta")

# Create generational cohorts
sort(unique(gss$cohort))

gss <- gss %>% 
  mutate(gen_coh = ifelse(cohort %in% 1883:1900, "Lost Generation",
                          ifelse(cohort %in% 1901:1927, "Greatest Generation",
                                 ifelse(cohort %in% 1928:1945, "Silent Generation",
                                        ifelse(cohort %in% 1946:1964, "Baby Boomers",
                                               ifelse(cohort %in% 1965:1980, "Generation X",
                                                      ifelse(cohort %in% 1981:1996, "Millenials",
                                                             ifelse(cohort %in% 1997:2000, "Generation Z", NA))))))))
# 3-year Generations
gss <- gss %>% 
  mutate(gen_coh3 = ifelse(cohort %in% 1888:1911, "1888-1911",
                           ifelse(cohort %in% 1912:1914, "1912-1914",
                                  ifelse(cohort %in% 1915:1917, "1915-1917",
                                         ifelse(cohort %in% 1918:1920, "1918-1920",
                                                ifelse(cohort %in% 1921:1923, "1921-1923",
                                                       ifelse(cohort %in% 1924:1926, "1924-1926",
                                                              ifelse(cohort %in% 1927:1929, "1927-1929",
                                                                     ifelse(cohort %in% 1930:1932, "1930-1932",
                                                                            ifelse(cohort %in% 1933:1935, "1933-1935",
                                                                                   ifelse(cohort %in% 1936:1938, "1936-1938",
                                                                                          ifelse(cohort %in% 1939:1941, "1939-1941",
                                                                                                 ifelse(cohort %in% 1942:1944, "1942-1944",
                                                                                                        ifelse(cohort %in% 1945:1947, "1945-1947",
                                                                                                               ifelse(cohort %in% 1948:1950, "1948-1950",
                                                                                                                      ifelse(cohort %in% 1951:1953, "1951-1953",
                                                                                                                             ifelse(cohort %in% 1954:1956, "1954-1956",
                                                                                                                                    ifelse(cohort %in% 1957:1959, "1957-1959",
                                                                                                                                           ifelse(cohort %in% 1960:1962, "1960-1962",
                                                                                                                                                  ifelse(cohort %in% 1963:1965, "1963-1965",
                                                                                                                                                         ifelse(cohort %in% 1966:1968, "1966-1968",
                                                                                                                                                                ifelse(cohort %in% 1969:1971, "1969-1971",
                                                                                                                                                                       ifelse(cohort %in% 1972:1974, "1972-1974",
                                                                                                                                                                              ifelse(cohort %in% 1975:1977, "1975-1977",
                                                                                                                                                                                     ifelse(cohort %in% 1978:1980, "1978-1980",
                                                                                                                                                                                            ifelse(cohort %in% 1981:1983, "1981-1983",
                                                                                                                                                                                                   ifelse(cohort %in% 1984:1986, "1984-1986",
                                                                                                                                                                                                          ifelse(cohort %in% 1987:1989, "1987-1989",
                                                                                                                                                                                                                 ifelse(cohort %in% 1990:1992, "1990-1992",
                                                                                                                                                                                                                        ifelse(cohort %in% 1993:1995, "1993-1995",
                                                                                                                                                                                                                               ifelse(cohort %in% 1996:1998, "1996-1998",
                                                                                                                                                                                                                                      ifelse(cohort %in% 1999:2000, "1999-2000", NA))))))))))))))))))))))))))))))))



# 5-year Generations
gss <- gss %>% 
  mutate(gen_coh5 = ifelse(cohort %in% 1888:1911, "1888-1911",
                           ifelse(cohort %in% 1912:1916, "1912-1916",
                                  ifelse(cohort %in% 1917:1921, "1917-1921",
                                         ifelse(cohort %in% 1922:1926, "1922-1926",
                                                ifelse(cohort %in% 1927:1931, "1927-1931",
                                                       ifelse(cohort %in% 1932:1936, "1932-1936",
                                                              ifelse(cohort %in% 1937:1941, "1937-1941",
                                                                     ifelse(cohort %in% 1942:1946, "1942-1946",
                                                                            ifelse(cohort %in% 1947:1951, "1947-1951",
                                                                                   ifelse(cohort %in% 1952:1956, "1952-1956",
                                                                                          ifelse(cohort %in% 1957:1961, "1957-1961",
                                                                                                 ifelse(cohort %in% 1962:1966, "1962-1966",
                                                                                                        ifelse(cohort %in% 1967:1971, "1967-1971",
                                                                                                               ifelse(cohort %in% 1972:1976, "1972-1976",
                                                                                                                      ifelse(cohort %in% 1977:1981, "1977-1981",
                                                                                                                             ifelse(cohort %in% 1982:1986, "1982-1986",
                                                                                                                                    ifelse(cohort %in% 1987:1991, "1987:1991",
                                                                                                                                           ifelse(cohort %in% 1992:1996, "1992-1996",
                                                                                                                                                  ifelse(cohort %in% 1997:2000, "1997-2000", NA))))))))))))))))))))


# 10-year Generations
gss <- gss %>% 
  mutate(gen_coh10 = ifelse(cohort %in% 1888:1911, "1888-1911",
                            ifelse(cohort %in% 1912:1921, "1912-1921",
                                   ifelse(cohort %in% 1922:1931, "1922-1931",
                                          ifelse(cohort %in% 1932:1941, "1932-1941",
                                                 ifelse(cohort %in% 1942:1951, "1942-1951",
                                                        ifelse(cohort %in% 1952:1961, "1952-1961",
                                                               ifelse(cohort %in% 1962:1971, "1962-1971",
                                                                      ifelse(cohort %in% 1972:1981, "1972-1981",
                                                                             ifelse(cohort %in% 1982:1991, "1982-1991",
                                                                                    ifelse(cohort %in% 1992:2000, "1992-2000", NA)))))))))))

# Recode all variables so that higher number indicates liberal

## class_
unique(gss$class_)
gss2 <- gss %>%
  mutate(class_ = ifelse(class_ %in% c(9, 8, 5, 0), NA, class_))

## sex (1: Male, 0: Female)
unique(gss2$sex)
gss2 <- gss2 %>% 
  mutate(sex = ifelse(sex == 2, 0,
                      ifelse(sex == 1, 1, NA)))

## satjob
unique(gss2$satjob)
gss2 <- gss2 %>% 
  mutate(satjob = ifelse(satjob %in% c(9, 8, 0), NA,
                         ifelse(satjob == 4, 1,
                                ifelse(satjob == 3, 2,
                                       ifelse(satjob == 2, 3, 4)))))

## health
unique(gss2$health)
gss2 <- gss2 %>% 
  mutate(health = ifelse(health %in% c(9, 8, 0), NA,
                         ifelse(health == 4, 1,
                                ifelse(health == 3, 2,
                                       ifelse(health == 2, 3, 4)))))

## happy
unique(gss2$happy)
gss2 <- gss2 %>% 
  mutate(happy = ifelse(happy %in% c(9, 8, 0), NA,
                        ifelse(happy == 3, 1,
                               ifelse(happy == 2, 2, 3))))

## busing (Racial busing)
unique(gss2$busing)
gss2 <- gss2 %>% 
  mutate(busing = ifelse(busing %in% c(9, 8, 0), NA,
                         ifelse(busing == 2, 0, 1)))

## racopen (Vote on open housing law)
unique(gss2$racopen)
gss2 <- gss2 %>% 
  mutate(racopen = ifelse(racopen %in% c(9, 8, 0), NA, racopen))

## racseg
unique(gss2$racseg)
gss2 <- gss2 %>% 
  mutate(racseg = ifelse(racseg %in% c(9, 8, 0), NA, racseg))

## racmar (Favor law againt interracial marriage: 1 no, 0 yes)
unique(gss2$racmar)
gss2 <- gss2 %>% 
  mutate(racmar = ifelse(racmar %in% c(9, 8, 0), NA,
                         ifelse(racmar == 2, 1, 0)))

## postlife (1: yes, 0: no)
unique(gss2$postlife)
gss2 <- gss2 %>% 
  mutate(postlife = ifelse(postlife %in% c(9, 8, 0), NA,
                           ifelse(postlife == 2, 0, 1)))

## reliten (Strength of affiliation, 4: strong, 1: no religion)
unique(gss2$reliten)
gss2 <- gss2 %>% 
  mutate(reliten = ifelse(reliten %in% c(9, 8, 0), NA,
                          ifelse(reliten == 4, 1,
                                 ifelse(reliten == 3, 2,
                                        ifelse(reliten == 2, 3, 4)))))

## attend
unique(gss2$attend)
gss2 <- gss2 %>% 
  mutate(attend = ifelse(attend == 9, NA, attend))

## fund
unique(gss2$fund)
gss2 <- gss2 %>% 
  mutate(fund = ifelse(fund == 9, NA, fund))

## grass (1: legal, 0: not legal)
unique(gss2$grass)
gss2 <- gss2 %>% 
  mutate(grass = ifelse(grass %in% c(9, 8, 0), NA,
                        ifelse(grass == 2, 0, 1)))

## courts (1: about right, 3: too harsh)
unique(gss2$courts)
gss2 <- gss2 %>% 
  mutate(courts = ifelse(courts %in% c(9, 8, 0), NA,
                         ifelse(courts == 3, 1,
                                ifelse(courts == 2, 2, 3))))

## gunlaw (1: favor, 0: oppose)
unique(gss2$gunlaw)
gss2 <- gss2 %>% 
  mutate(gunlaw = ifelse(gunlaw %in% c(9, 8, 0), NA,
                         ifelse(gunlaw == 2, 0, 1)))

## cappun (1: oppose, 0: favor)
unique(gss2$cappun)
gss2 <- gss2 %>% 
  mutate(cappun = ifelse(cappun %in% c(9, 8, 0), NA,
                         ifelse(cappun == 2, 1, 0)))

## libhomo (1: not remove, 0: remove)
unique(gss2$libhomo)
gss2 <- gss2 %>% 
  mutate(libhomo = ifelse(libhomo %in% c(9, 8, 0), NA,
                          ifelse(libhomo == 2, 1, 0)))

## satfin (1: not at all satisfied, 3: satisfied)
unique(gss2$satfin)
gss2 <- gss2 %>% 
  mutate(satfin = ifelse(satfin %in% c(9, 8, 0), NA,
                         ifelse(satfin == 3, 1,
                                ifelse(satfin == 2, 2, 3))))

## finrela
unique(gss2$finrela)
gss2 <- gss2 %>% 
  mutate(finrela = ifelse(finrela %in% c(9, 8, 0), NA, finrela))

## cohort
unique(gss2$cohort)
gss2 <- gss2 %>% 
  mutate(cohort = ifelse(cohort %in% c(9999, 0), NA, cohort))

## commun (feeling about cummunism, 4: good form, 1: worst kind)
unique(gss2$commun)
gss2 <- gss2 %>% 
  mutate(commun = ifelse(commun %in% c(9, 8, 0), NA, commun))

## suicide1 (suicide if incurable disease, 1: yes, 0: no)
unique(gss2$suicide1)
gss2 <- gss2 %>% 
  mutate(suicide1 = ifelse(suicide1 %in% c(9, 8, 0), NA,
                           ifelse(suicide1 == 2, 0, 1)))

## letdie1 (allow incurable patients to die, 1: yes, 0: no)
unique(gss2$letdie1)
gss2 <- gss2 %>% 
  mutate(letdie1 = ifelse(letdie1 %in% c(9, 8, 0), NA,
                          ifelse(letdie1 == 2, 0, 1)))

## xmovie (allow incurable patients to die, 1: yes, 0: no)
unique(gss2$xmovie)
gss2 <- gss2 %>% 
  mutate(xmovie = ifelse(xmovie %in% c(9, 8, 0), NA,
                         ifelse(xmovie == 2, 0, 1)))

## pornlaw (3: legal, 2:illegal under 18, 1: illegal to all)
unique(gss2$pornlaw)
gss2 <- gss2 %>% 
  mutate(pornlaw = ifelse(pornlaw %in% c(9, 8, 0), NA, pornlaw))

## homosex (4: not wrong at all, 1: always wrong)
unique(gss2$homosex)
gss2 <- gss2 %>% 
  mutate(homosex = ifelse(homosex %in% c(9, 8, 5, 0), NA, homosex))

## premarsx (4: not wrong at all, 1: always wrong)
unique(gss2$premarsx)
gss2 <- gss2 %>% 
  mutate(premarsx = ifelse(premarsx %in% c(9, 8, 5, 0), NA, premarsx))

## chldidel
unique(gss2$chldidel)
gss2 <- gss2 %>% 
  mutate(chldidel = ifelse(chldidel %in% c(9, -1), NA, chldidel))

## abany (abortion if women want, 1: yes, 0: no)
unique(gss2$abany)
gss2 <- gss2 %>% 
  mutate(abany = ifelse(abany %in% c(9, 8, 0), NA,
                        ifelse(abany == 2, 0, 1)))

## absingle (abortion if women don't want to marry the man, 1: yes, 0: no)
unique(gss2$absingle)
gss2 <- gss2 %>% 
  mutate(absingle = ifelse(absingle %in% c(9, 8, 0), NA,
                           ifelse(absingle == 2, 0, 1)))

## abhlth (Woman's health seriously endangered, 1: yes, 0: no)
unique(gss2$abhlth)
gss2 <- gss2 %>% 
  mutate(abhlth = ifelse(abhlth %in% c(9, 8, 0), NA,
                         ifelse(abhlth == 2, 0, 1)))

## fepres (Vote for women president, 1: yes, 0: no)
unique(gss2$fepres)
gss2 <- gss2 %>% 
  mutate(fepres = ifelse(fepres %in% c(9, 8, 0), NA,
                         ifelse(fepres %in% c(2, 5), 0, 1)))

## fework (Women should work, 1: approve, 0: disapprove)
unique(gss2$fework)
gss2 <- gss2 %>% 
  mutate(fework = ifelse(fework %in% c(9, 8, 0), NA,
                         ifelse(fework == 2, 0, 1)))

## fehome (Women take care of home not country, 1: disagree, 0: agree)
unique(gss2$fehome)
gss2 <- gss2 %>% 
  mutate(fehome = ifelse(fehome %in% c(9, 8, 0), NA,
                         ifelse(fehome == 2, 1, 0)))

## spkhomo (1: allowed, 0: not allowed)
unique(gss2$spkhomo)
gss2 <- gss2 %>% 
  mutate(spkhomo = ifelse(spkhomo %in% c(9, 8, 0), NA,
                          ifelse(spkhomo == 2, 0, 1)))

## libcom (1: not remove, 0: remove)
unique(gss2$libcom)
gss2 <- gss2 %>% 
  mutate(libcom = ifelse(libcom %in% c(9, 8, 0), NA,
                         ifelse(libcom == 2, 1, 0)))

## spkcom (1: allowed, 0: not allowed)
unique(gss2$spkcom)
gss2 <- gss2 %>% 
  mutate(spkcom = ifelse(spkcom %in% c(9, 8, 0), NA,
                         ifelse(spkcom == 2, 0, 1)))

## educ
unique(gss2$educ)
gss2 <- gss2 %>% 
  mutate(educ = ifelse(educ %in% c(99, 98, 97), NA, educ))

## age
unique(gss2$age)
gss2 <- gss2 %>% 
  mutate(age = ifelse(age %in% c(99, 98), NA, age))

## childs
unique(gss2$childs)
gss2 <- gss2 %>% 
  mutate(childs = ifelse(childs %in% c(9), NA, childs))

## spkrac (allow racist speak, 1: allowed, 0: not allowed)
unique(gss2$spkrac)
gss2 <- gss2 %>% 
  mutate(spkrac = ifelse(spkrac %in% c(9, 8, 0), NA,
                         ifelse(spkrac == 2, 0, 1)))

## libath (anti-religion book in library, 1: not remove, 0: remove)
unique(gss2$libath)
gss2 <- gss2 %>% 
  mutate(libath = ifelse(libath %in% c(9, 8, 0), NA,
                         ifelse(libath == 2, 1, 0)))

## spkath (allow anti-religionist speak, 1: allowed, 0: not allowed)
unique(gss2$spkath)
gss2 <- gss2 %>% 
  mutate(spkath = ifelse(spkath %in% c(9, 8, 0), NA,
                         ifelse(spkath == 2, 0, 1)))

## tax (1: too high, 3: too low)
unique(gss2$tax)
gss2 <- gss2 %>% 
  mutate(tax = ifelse(tax %in% c(9, 8, 4, 0), NA, tax))

## eqwlth (0: no govt action, 6: govt reduce diff)
unique(gss2$eqwlth)
gss2 <- gss2 %>% 
  mutate(eqwlth = ifelse(eqwlth %in% c(9, 8, 0), NA,
                         ifelse(eqwlth == 7, 0,
                                ifelse(eqwlth == 6, 1,
                                       ifelse(eqwlth == 5, 2,
                                              ifelse(eqwlth == 4, 3,
                                                     ifelse(eqwlth == 3, 4,
                                                            ifelse(eqwlth == 2, 5, 6))))))))

## natarmsy (National defence: 3: too much, 1: too little)
unique(gss2$natarmsy)
gss2 <- gss2 %>% 
  mutate(natarmsy = ifelse(natarmsy %in% c(9, 8, 0), NA, natarmsy))

## natfare (Welfare: 3: too little, 1: too much)
unique(gss2$natfare)
gss2 <- gss2 %>% 
  mutate(natfare = ifelse(natfare %in% c(9, 8, 0), NA,
                          ifelse(natfare == 3, 1,
                                 ifelse(natfare == 2, 2, 3))))

## natarms (Military, armaments, and defense: 3: too much, 1: too little)
unique(gss2$natarms)
gss2 <- gss2 %>% 
  mutate(natarms = ifelse(natarms %in% c(9, 8, 0), NA, natarms))

## natrace (Improving the conditions of blacks: 3: too little, 1: too much)
unique(gss2$natrace)
gss2 <- gss2 %>% 
  mutate(natrace = ifelse(natrace %in% c(9, 8, 0), NA,
                          ifelse(natrace == 3, 1,
                                 ifelse(natrace == 2, 2, 3))))

## nateduc (Improving nations education system: 3: too little, 1: too much)
unique(gss2$nateduc)
gss2 <- gss2 %>% 
  mutate(nateduc = ifelse(nateduc %in% c(9, 8, 0), NA,
                          ifelse(nateduc == 3, 1,
                                 ifelse(nateduc == 2, 2, 3))))

## natdrug (Dealing with drug addiction: 3: too little, 1: too much)
unique(gss2$natdrug)
gss2 <- gss2 %>% 
  mutate(natdrug = ifelse(natdrug %in% c(9, 8, 0), NA,
                          ifelse(natdrug == 3, 1,
                                 ifelse(natdrug == 2, 2, 3))))

## natcrime (Halting rising crime rate, 3: too little, 1: too much)
unique(gss2$natcrime)
gss2 <- gss2 %>% 
  mutate(natcrime = ifelse(natcrime %in% c(9, 8, 0), NA,
                           ifelse(natcrime == 3, 1,
                                  ifelse(natcrime == 2, 2, 3))))

## natcrime (Halting rising crime rate, 3: too little, 1: too much)
unique(gss2$natcrime)
gss2 <- gss2 %>% 
  mutate(natcrime = ifelse(natcrime %in% c(9, 8, 0), NA,
                           ifelse(natcrime == 3, 1,
                                  ifelse(natcrime == 2, 2, 3))))

## natcity (Solving problems of big cities, 3: too little, 1: too much)
unique(gss2$natcity)
gss2 <- gss2 %>% 
  mutate(natcity = ifelse(natcity %in% c(9, 8, 0), NA,
                          ifelse(natcity == 3, 1,
                                 ifelse(natcity == 2, 2, 3))))

## natheal (Improving & protecting nations health, 3: too little, 1: too much)
unique(gss2$natheal)
gss2 <- gss2 %>% 
  mutate(natheal = ifelse(natheal %in% c(9, 8, 0), NA,
                          ifelse(natheal == 3, 1,
                                 ifelse(natheal == 2, 2, 3))))

## natenvir (Improving & protecting environment, 3: too little, 1: too much)
unique(gss2$natenvir)
gss2 <- gss2 %>% 
  mutate(natenvir = ifelse(natenvir %in% c(9, 8, 0), NA,
                           ifelse(natenvir == 3, 1,
                                  ifelse(natenvir == 2, 2, 3))))

## polviews (1: extremly conservative, 7: extremely liberal)
unique(gss2$polviews)
gss2 <- gss2 %>% 
  mutate(polviews = ifelse(polviews %in% c(9, 8, 0), NA,
                           ifelse(polviews == 7, 1,
                                  ifelse(polviews == 6, 2,
                                         ifelse(polviews == 5, 3,
                                                ifelse(polviews == 4, 4,
                                                       ifelse(polviews == 3, 5,
                                                              ifelse(polviews == 2, 6, 7))))))))

## partyid
unique(gss2$partyid)
gss2 <- gss2 %>% 
  mutate(partyid = ifelse(partyid %in% c(9, 8, 7), NA,
                          ifelse(partyid == 6, 0,
                                 ifelse(partyid == 5, 1,
                                        ifelse(partyid == 4, 2,
                                               ifelse(partyid == 3, 3,
                                                      ifelse(partyid == 2, 4,
                                                             ifelse(partyid == 1, 5, 6))))))))

# Convert category variables to factor variables
unique(gss2$gen_coh)
nrow(gss2)
table(gss2$gen_coh)
test_sample <- gss2 %>% 
  filter(gen_coh != "Generation Z", gen_coh != "Lost Generation")
table(test_sample$gen_coh)
unique(test_sample$gen_coh)

gss3 <- gss2 %>% 
  filter(gen_coh != "Generation Z", gen_coh != "Lost Generation") %>% 
  mutate_at(vars(busing, racopen, racmar, postlife, reliten, fund, grass, gunlaw, cappun, libhomo, ballot, suicide1, letdie1,
                 xmovie, pornlaw, abany, absingle, abhlth, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath,
                 spkath, year), funs(as.factor(.)))

gss3 <- gss3 %>% 
  select(reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1, pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath, tax, eqwlth, natfare, natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, polviews, year, lnIncome18, lnRincome18, age, finrela, satfin, satjob, educ, childs, busing, racopen, racmar, postlife)

# Drop unobservable cases for POLVIEWS
gss3 <- gss3 %>% 
  drop_na(polviews)

head(gss3)


####################################
# Stage 1: Ridge Regression
# Missing data analysis
# md.pattern(gss3)
imp <- mice(gss3, pred = quickpred(gss3, minpuc = 0.25, mincor = 0.25), seed = 12345, maxit = 5)
gss_imputed1 <- complete(imp, 1)
gss_imputed2 <- complete(imp, 2)
gss_imputed3 <- complete(imp, 3)
gss_imputed4 <- complete(imp, 4)
gss_imputed5 <- complete(imp, 5)

write.csv(gss_imputed1, "gss_imputed1_new3.csv")
write.csv(gss_imputed2, "gss_imputed2_new3.csv")
write.csv(gss_imputed3, "gss_imputed3_new3.csv")
write.csv(gss_imputed4, "gss_imputed4_new3.csv")
write.csv(gss_imputed5, "gss_imputed5_new3.csv")

# Reload the saved imputed sets
gss_imputed1 <- read.csv("gss_imputed1_new.csv")
gss_imputed2 <- read.csv("gss_imputed2_new.csv")
gss_imputed3 <- read.csv("gss_imputed3_new.csv")
gss_imputed4 <- read.csv("gss_imputed4_new.csv")
gss_imputed5 <- read.csv("gss_imputed5_new.csv")

# Min-max normalization
minmax <- function (z) {
  return((z - min(z)) / (max(z) - min(z)))
}

str(gss_imputed1)


# GLMNET: Ridge Regression
# Find the best lambda
test1 <- gss_imputed1 %>% 
  filter()
  mutate_at(vars(courts, commun, homosex, premarsx, chldidel, natfare, natarms, natrace, nateduc, natdrug, natcrime,
                 natcity, natheal, natenvir), funs(minmax))

x <- test1 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test1$polviews

lambda_seq <- 10^seq(1, -1, by = -.1)
cv_ridge1 <- cv.glmnet(x, y, alpha = 0, lambda = lambda_seq)
optimal_lambda <- cv_ridge1$lambda.min
optimal_lambda

fit1 <- glmnet(x, y, alpha = 0)
sort(predict(fit1, type = "coefficients", s = optimal_lambda)[1:20,])


# Imputed 2
test2 <- gss_imputed2 %>% 
  mutate_at(vars(courts, commun, homosex, premarsx, chldidel, natfare, natarms, natrace, nateduc, natdrug, natcrime,
                 natcity, natheal, natenvir), funs(minmax))

# Imputed 3
test3 <- gss_imputed3 %>% 
  mutate_at(vars(courts, commun, homosex, premarsx, chldidel, natfare, natarms, natrace, nateduc, natdrug, natcrime,
                 natcity, natheal, natenvir), funs(minmax))


# Imputed 4
test4 <- gss_imputed4 %>% 
  mutate_at(vars(courts, commun, homosex, premarsx, chldidel, natfare, natarms, natrace, nateduc, natdrug, natcrime,
                 natcity, natheal, natenvir), funs(minmax))


# Imputed 5
test5 <- gss_imputed5 %>% 
  mutate_at(vars(courts, commun, homosex, premarsx, chldidel, natfare, natarms, natrace, nateduc, natdrug, natcrime,
                 natcity, natheal, natenvir), funs(minmax))


# Training and Test set approach
# test 1
head(test1)
x <- test1 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test1$polviews

set.seed(123)
train_rows <- sample(1:nrow(test1), 0.8*nrow(test1))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge1 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test1 <- data.frame(sort(predict(cv_ridge1, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test1$variable <- rownames(df_test1)
colnames(df_test1) <- c("coeff", "Predictor")
predicted1 <- predict(cv_ridge1, s = cv_ridge1$lambda.1se, newx = x_test)
mse1 <- mean((y_test - predicted1)^2)
head(df_test1)
df_test1 <- df_test1 %>% 
  select(Predictor, coeff) %>% 
  mutate(abs_coef = abs(coeff)) %>% 
  arrange(desc(abs_coef))
head(df_test1)

# test 2
head(test2)
x <- test2 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test2$polviews

set.seed(124)
train_rows <- sample(1:nrow(test2), 0.8*nrow(test2))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge2 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test2 <- data.frame(sort(predict(cv_ridge2, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test2$variabl <- rownames(df_test2)
colnames(df_test2) <- c("coeff", "Predictor")
predicted2 <- predict(cv_ridge2, s = cv_ridge2$lambda.1se, newx = x_test)
mse2 <- mean((y_test - predicted2)^2)
df_test2 <- df_test2 %>% 
  select(Predictor, coeff) %>% 
  mutate(abs_coef = abs(coeff)) %>% 
  arrange(desc(abs_coef))
head(df_test2)

# test 3
head(test3)
x <- test3 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test3$polviews

set.seed(125)
train_rows <- sample(1:nrow(test3), 0.8*nrow(test3))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge3 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test3 <- data.frame(sort(predict(cv_ridge3, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test3$variabl <- rownames(df_test3)
colnames(df_test3) <- c("coeff", "Predictor")
predicted3 <- predict(cv_ridge3, s = cv_ridge3$lambda.1se, newx = x_test)
mse3 <- mean((y_test - predicted3)^2)
df_test3 <- df_test3 %>% 
  select(Predictor, coeff) %>% 
  mutate(abs_coef = abs(coeff)) %>% 
  arrange(desc(abs_coef))
head(df_test3)

# test 4
head(test4)
x <- test4 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test4$polviews

set.seed(126)
train_rows <- sample(1:nrow(test4), 0.8*nrow(test4))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge4 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test4 <- data.frame(sort(predict(cv_ridge4, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test4$variabl <- rownames(df_test4)
colnames(df_test4) <- c("coeff", "Predictor")
predicted4 <- predict(cv_ridge4, s = cv_ridge4$lambda.1se, newx = x_test)
mse4 <- mean((y_test - predicted4)^2)
df_test4 <- df_test4 %>% 
  select(Predictor, coeff) %>% 
  mutate(abs_coef = abs(coeff)) %>% 
  arrange(desc(abs_coef))
head(df_test4)


# test 5
head(test5)
x <- test5 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test5$polviews

set.seed(127)
train_rows <- sample(1:nrow(test5), 0.8*nrow(test5))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge5 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test5 <- data.frame(sort(predict(cv_ridge5, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test5$variabl <- rownames(df_test5)
colnames(df_test5) <- c("coef", "Predictor")
predicted5 <- predict(cv_ridge5, s = cv_ridge5$lambda.1se, newx = x_test)
mse5 <- mean((y_test - predicted5)^2)
df_test5 <- df_test5 %>% 
  select(Predictor, coef) %>% 
  mutate(abs_coef = abs(coef)) %>% 
  arrange(desc(abs_coef))

head(df_test5)

df_test <- cbind(df_test1, df_test2, df_test3, df_test4, df_test5)
write.csv(df_test, "df_test4.csv")

mse1
mse2
mse3
mse4
mse5


######################################
# By period
# 1970s (using imputed 1 dataset)
head(test1)
str(test1)
test70s_1 <- test1 %>% 
  filter(year %in% c(1974, 1975, 1976, 1977, 1978, 1979))

x <- test70s_1 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test70s_1$polviews

set.seed(123)
train_rows <- sample(1:nrow(test70s_1), 0.8*nrow(test70s_1))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge70s_1 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test70s_1 <- data.frame(sort(predict(cv_ridge70s_1, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test70s_1$variabl <- rownames(df_test70s_1)
colnames(df_test70s_1) <- c("coeff1", "Predictor")
predicted70s_1 <- predict(cv_ridge70s_1, s = cv_ridge70s_1$lambda.1se, newx = x_test)
mse70s_1 <- mean((y_test - predicted70s_1)^2)
df_test70s_1 <- df_test70s_1 %>% 
  select(Predictor, coeff1)
head(df_test70s_1)


# 1970s (using imputed 2 dataset)
head(test2)
str(test2)
test70s_2 <- test2 %>% 
  filter(year %in% c(1974, 1975, 1976, 1977, 1978, 1979))

x <- test70s_2 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test70s_2$polviews

set.seed(124)
train_rows <- sample(1:nrow(test70s_2), 0.8*nrow(test70s_2))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge70s_2 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test70s_2 <- data.frame(sort(predict(cv_ridge70s_2, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test70s_2$variabl <- rownames(df_test70s_2)
colnames(df_test70s_2) <- c("coeff2", "Predictor")
predicted70s_2 <- predict(cv_ridge70s_2, s = cv_ridge70s_2$lambda.1se, newx = x_test)
mse70s_2 <- mean((y_test - predicted70s_2)^2)
df_test70s_2 <- df_test70s_2 %>% 
  select(Predictor, coeff2)
head(df_test70s_2)

# 1970s (using imputed 3 dataset)
head(test3)
str(test3)
test70s_3 <- test3 %>% 
  filter(year %in% c(1974, 1975, 1976, 1977, 1978, 1979))

x <- test70s_3 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test70s_3$polviews

set.seed(125)
train_rows <- sample(1:nrow(test70s_3), 0.8*nrow(test70s_3))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge70s_3 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test70s_3 <- data.frame(sort(predict(cv_ridge70s_3, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test70s_3$variabl <- rownames(df_test70s_3)
colnames(df_test70s_3) <- c("coeff3", "Predictor")
predicted70s_3 <- predict(cv_ridge70s_3, s = cv_ridge70s_3$lambda.1se, newx = x_test)
mse70s_3 <- mean((y_test - predicted70s_3)^2)
df_test70s_3 <- df_test70s_3 %>% 
  select(Predictor, coeff3)
head(df_test70s_3)


# 1970s (using imputed 4 dataset)
head(test4)
str(test4)
test70s_4 <- test4 %>% 
  filter(year %in% c(1974, 1975, 1976, 1977, 1978, 1979))

x <- test70s_4 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test70s_4$polviews

set.seed(126)
train_rows <- sample(1:nrow(test70s_4), 0.8*nrow(test70s_4))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge70s_4 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test70s_4 <- data.frame(sort(predict(cv_ridge70s_4, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test70s_4$variabl <- rownames(df_test70s_4)
colnames(df_test70s_4) <- c("coeff4", "Predictor")
predicted70s_4 <- predict(cv_ridge70s_4, s = cv_ridge70s_4$lambda.1se, newx = x_test)
mse70s_4 <- mean((y_test - predicted70s_4)^2)
df_test70s_4 <- df_test70s_4 %>% 
  select(Predictor, coeff4)
head(df_test70s_4)


# 1970s (using imputed 5 dataset)
head(test5)
str(test5)
test70s_5 <- test5 %>% 
  filter(year %in% c(1974, 1975, 1976, 1977, 1978, 1979))

x <- test70s_5 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test70s_5$polviews

set.seed(127)
train_rows <- sample(1:nrow(test70s_5), 0.8*nrow(test70s_5))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge70s_5 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test70s_5 <- data.frame(sort(predict(cv_ridge70s_5, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test70s_5$variabl <- rownames(df_test70s_5)
colnames(df_test70s_5) <- c("coeff5", "Predictor")
predicted70s_5 <- predict(cv_ridge70s_5, s = cv_ridge70s_5$lambda.1se, newx = x_test)
mse70s_5 <- mean((y_test - predicted70s_5)^2)
df_test70s_5 <- df_test70s_5 %>% 
  select(Predictor, coeff5)
head(df_test70s_5)

df_test_70s <- merge(df_test70s_1, df_test70s_2, by = "Predictor", all.x = TRUE)
df_test_70s <- merge(df_test_70s, df_test70s_3, by = "Predictor", all.x = TRUE)
df_test_70s <- merge(df_test_70s, df_test70s_4, by = "Predictor", all.x = TRUE)
df_test_70s <- merge(df_test_70s, df_test70s_5, by = "Predictor", all.x = TRUE)
head(df_test_70s)

df_test_70s$max <- apply(df_test_70s[,2:6], 1, max)
df_test_70s$min <- apply(df_test_70s[,2:6], 1, min)

df_test_70s <- df_test_70s[-1,]
  
df_test_70s <- df_test_70s %>% 
  arrange(desc(max))

head(df_test_70s)
cle_70s <- df_test_70s %>% 
  select(Predictor, max, min)
cle_70s <- cle_70s[1:10,]
head(cle_70s)
cle_70s <- gather(cle_70s, category, value, 2:3)

theme_dotplot <- theme_bw(14) +
  theme(axis.text.y = element_text(size = rel(.75)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.75)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5),
        panel.grid.minor.x = element_blank(),
        legend.position = 'none')

cle_70s %>% 
  ggplot(aes(value, Predictor, color = category)) +
  geom_point() +
  theme_dotplot

pal <- wes_palette("Cavalcanti1", 5, type = "discrete")

plot70s <- cle_70s %>% 
  ggplot(aes(value, fct_reorder2(Predictor, category == "max", value, .desc = FALSE), color = category)) +
  geom_point(size = 2) +
  geom_line(aes(group = Predictor), color = "darkgrey") +
  scale_color_manual(values = c("#f4352c", "#042c44")) +
  ylab("")  + xlab("") +
  ggtitle("1970s") +
  xlim(0, 0.53) +
  theme_dotplot


# 1980s
# 1980s (using imputed 1 dataset)
head(test1)
str(test1)
test80s_1 <- test1 %>% 
  filter(year %in% c(1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989))

x <- test80s_1 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test80s_1$polviews

set.seed(123)
train_rows <- sample(1:nrow(test80s_1), 0.8*nrow(test80s_1))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge80s_1 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test80s_1 <- data.frame(sort(predict(cv_ridge80s_1, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test80s_1$variabl <- rownames(df_test80s_1)
colnames(df_test80s_1) <- c("coeff1", "Predictor")
predicted80s_1 <- predict(cv_ridge80s_1, s = cv_ridge80s_1$lambda.1se, newx = x_test)
mse80s_1 <- mean((y_test - predicted80s_1)^2)
df_test80s_1 <- df_test80s_1 %>% 
  select(Predictor, coeff1)
head(df_test80s_1)


# 1980s (using imputed 2 dataset)
head(test2)
str(test2)
test80s_2 <- test2 %>% 
  filter(year %in% c(1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989))

x <- test80s_2 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test80s_2$polviews

set.seed(124)
train_rows <- sample(1:nrow(test80s_2), 0.8*nrow(test80s_2))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge80s_2 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test80s_2 <- data.frame(sort(predict(cv_ridge80s_2, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test80s_2$variabl <- rownames(df_test80s_2)
colnames(df_test80s_2) <- c("coeff2", "Predictor")
predicted80s_2 <- predict(cv_ridge80s_2, s = cv_ridge80s_2$lambda.1se, newx = x_test)
mse80s_2 <- mean((y_test - predicted80s_2)^2)
df_test80s_2 <- df_test80s_2 %>% 
  select(Predictor, coeff2)
head(df_test80s_2)

# 1980s (using imputed 3 dataset)
head(test3)
str(test3)
test80s_3 <- test3 %>% 
  filter(year %in% c(1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989))

x <- test80s_3 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test80s_3$polviews

set.seed(125)
train_rows <- sample(1:nrow(test80s_3), 0.8*nrow(test80s_3))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge80s_3 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test80s_3 <- data.frame(sort(predict(cv_ridge80s_3, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test80s_3$variabl <- rownames(df_test80s_3)
colnames(df_test80s_3) <- c("coeff3", "Predictor")
predicted80s_3 <- predict(cv_ridge80s_3, s = cv_ridge80s_3$lambda.1se, newx = x_test)
mse80s_3 <- mean((y_test - predicted80s_3)^2)
df_test80s_3 <- df_test80s_3 %>% 
  select(Predictor, coeff3)
head(df_test80s_3)


# 1980s (using imputed 4 dataset)
head(test4)
str(test4)
test80s_4 <- test4 %>% 
  filter(year %in% c(1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989))

x <- test80s_4 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test80s_4$polviews

set.seed(126)
train_rows <- sample(1:nrow(test80s_4), 0.8*nrow(test80s_4))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge80s_4 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test80s_4 <- data.frame(sort(predict(cv_ridge80s_4, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test80s_4$variabl <- rownames(df_test80s_4)
colnames(df_test80s_4) <- c("coeff4", "Predictor")
predicted80s_4 <- predict(cv_ridge80s_4, s = cv_ridge80s_4$lambda.1se, newx = x_test)
mse80s_4 <- mean((y_test - predicted80s_4)^2)
df_test80s_4 <- df_test80s_4 %>% 
  select(Predictor, coeff4)
head(df_test80s_4)


# 1980s (using imputed 5 dataset)
head(test5)
str(test5)
test80s_5 <- test5 %>% 
  filter(year %in% c(1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989))

x <- test80s_5 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test80s_5$polviews

set.seed(127)
train_rows <- sample(1:nrow(test80s_5), 0.8*nrow(test80s_5))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge80s_5 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test80s_5 <- data.frame(sort(predict(cv_ridge80s_5, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test80s_5$variabl <- rownames(df_test80s_5)
colnames(df_test80s_5) <- c("coeff5", "Predictor")
predicted80s_5 <- predict(cv_ridge80s_5, s = cv_ridge80s_5$lambda.1se, newx = x_test)
mse80s_5 <- mean((y_test - predicted80s_5)^2)
df_test80s_5 <- df_test80s_5 %>% 
  select(Predictor, coeff5)
head(df_test80s_5)

df_test_80s <- merge(df_test80s_1, df_test80s_2, by = "Predictor", all.x = TRUE)
df_test_80s <- merge(df_test_80s, df_test80s_3, by = "Predictor", all.x = TRUE)
df_test_80s <- merge(df_test_80s, df_test80s_4, by = "Predictor", all.x = TRUE)
df_test_80s <- merge(df_test_80s, df_test80s_5, by = "Predictor", all.x = TRUE)
head(df_test_80s)

df_test_80s$max <- apply(df_test_80s[,2:6], 1, max)
df_test_80s$min <- apply(df_test_80s[,2:6], 1, min)

df_test_80s <- df_test_80s[-1,]

df_test_80s <- df_test_80s %>% 
  arrange(desc(max))

head(df_test_80s)
cle_80s <- df_test_80s %>% 
  select(Predictor, max, min)
cle_80s <- cle_80s[1:10,]
head(cle_80s)
cle_80s <- gather(cle_80s, category, value, 2:3)

plot80s <- cle_80s %>% 
  ggplot(aes(value, fct_reorder2(Predictor, category == "max", value, .desc = FALSE), color = category)) +
  geom_point(size = 2) +
  geom_line(aes(group = Predictor), color = "darkgrey") +
  scale_color_manual(values = c("#f4352c", "#042c44")) +
  ylab("")  + xlab("") +
  ggtitle("1980s") +
  xlim(0, 0.53) +
  theme_dotplot


# 1990s
# 1990s (using imputed 1 dataset)
head(test1)
str(test1)
test90s_1 <- test1 %>% 
  filter(year %in% c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999))

x <- test90s_1 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test90s_1$polviews

set.seed(123)
train_rows <- sample(1:nrow(test90s_1), 0.8*nrow(test90s_1))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge90s_1 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test90s_1 <- data.frame(sort(predict(cv_ridge90s_1, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test90s_1$variabl <- rownames(df_test90s_1)
colnames(df_test90s_1) <- c("coeff1", "Predictor")
predicted90s_1 <- predict(cv_ridge90s_1, s = cv_ridge90s_1$lambda.1se, newx = x_test)
mse90s_1 <- mean((y_test - predicted90s_1)^2)
df_test90s_1 <- df_test90s_1 %>% 
  select(Predictor, coeff1)
head(df_test90s_1)


# 1990s (using imputed 2 dataset)
head(test2)
str(test2)
test90s_2 <- test2 %>% 
  filter(year %in% c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999))

x <- test90s_2 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test90s_2$polviews

set.seed(124)
train_rows <- sample(1:nrow(test90s_2), 0.8*nrow(test90s_2))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge90s_2 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test90s_2 <- data.frame(sort(predict(cv_ridge90s_2, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test90s_2$variabl <- rownames(df_test90s_2)
colnames(df_test90s_2) <- c("coeff2", "Predictor")
predicted90s_2 <- predict(cv_ridge90s_2, s = cv_ridge90s_2$lambda.1se, newx = x_test)
mse90s_2 <- mean((y_test - predicted90s_2)^2)
df_test90s_2 <- df_test90s_2 %>% 
  select(Predictor, coeff2)
head(df_test90s_2)

# 1990s (using imputed 3 dataset)
head(test3)
str(test3)
test90s_3 <- test3 %>% 
  filter(year %in% c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999))

x <- test90s_3 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test90s_3$polviews

set.seed(125)
train_rows <- sample(1:nrow(test90s_3), 0.8*nrow(test90s_3))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge90s_3 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test90s_3 <- data.frame(sort(predict(cv_ridge90s_3, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test90s_3$variabl <- rownames(df_test90s_3)
colnames(df_test90s_3) <- c("coeff3", "Predictor")
predicted90s_3 <- predict(cv_ridge90s_3, s = cv_ridge90s_3$lambda.1se, newx = x_test)
mse90s_3 <- mean((y_test - predicted90s_3)^2)
df_test90s_3 <- df_test90s_3 %>% 
  select(Predictor, coeff3)
head(df_test90s_3)


# 1990s (using imputed 4 dataset)
head(test4)
str(test4)
test90s_4 <- test4 %>% 
  filter(year %in% c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999))

x <- test90s_4 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test90s_4$polviews

set.seed(126)
train_rows <- sample(1:nrow(test90s_4), 0.8*nrow(test90s_4))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge90s_4 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test90s_4 <- data.frame(sort(predict(cv_ridge90s_4, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test90s_4$variabl <- rownames(df_test90s_4)
colnames(df_test90s_4) <- c("coeff4", "Predictor")
predicted90s_4 <- predict(cv_ridge90s_4, s = cv_ridge90s_4$lambda.1se, newx = x_test)
mse90s_4 <- mean((y_test - predicted90s_4)^2)
df_test90s_4 <- df_test90s_4 %>% 
  select(Predictor, coeff4)
head(df_test90s_4)


# 1990s (using imputed 5 dataset)
head(test5)
str(test5)
test90s_5 <- test5 %>% 
  filter(year %in% c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999))

x <- test90s_5 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test90s_5$polviews

set.seed(127)
train_rows <- sample(1:nrow(test90s_5), 0.8*nrow(test90s_5))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge90s_5 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test90s_5 <- data.frame(sort(predict(cv_ridge90s_5, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test90s_5$variabl <- rownames(df_test90s_5)
colnames(df_test90s_5) <- c("coeff5", "Predictor")
predicted90s_5 <- predict(cv_ridge90s_5, s = cv_ridge90s_5$lambda.1se, newx = x_test)
mse90s_5 <- mean((y_test - predicted90s_5)^2)
df_test90s_5 <- df_test90s_5 %>% 
  select(Predictor, coeff5)
head(df_test90s_5)

df_test_90s <- merge(df_test90s_1, df_test90s_2, by = "Predictor", all.x = TRUE)
df_test_90s <- merge(df_test_90s, df_test90s_3, by = "Predictor", all.x = TRUE)
df_test_90s <- merge(df_test_90s, df_test90s_4, by = "Predictor", all.x = TRUE)
df_test_90s <- merge(df_test_90s, df_test90s_5, by = "Predictor", all.x = TRUE)
head(df_test_90s)

df_test_90s$max <- apply(df_test_90s[,2:6], 1, max)
df_test_90s$min <- apply(df_test_90s[,2:6], 1, min)

df_test_90s <- df_test_90s[-1,]

df_test_90s <- df_test_90s %>% 
  arrange(desc(max))

head(df_test_90s)
cle_90s <- df_test_90s %>% 
  select(Predictor, max, min)
cle_90s <- cle_90s[1:10,]
head(cle_90s)
cle_90s <- gather(cle_90s, category, value, 2:3)

plot90s <- cle_90s %>% 
  ggplot(aes(value, fct_reorder2(Predictor, category == "max", value, .desc = FALSE), color = category)) +
  geom_point(size = 2) +
  geom_line(aes(group = Predictor), color = "darkgrey") +
  scale_color_manual(values = c("#f4352c", "#042c44")) +
  ylab("") +
  ggtitle("1990s") +
  xlab(expression(hat(beta)^'Ridge')) +
  xlim(0, 0.53) +
  theme_dotplot


# 2000s
# 2000s (using imputed 1 dataset)
head(test1)
str(test1)
test00s_1 <- test1 %>% 
  filter(year %in% c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009))

x <- test00s_1 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test00s_1$polviews

set.seed(123)
train_rows <- sample(1:nrow(test00s_1), 0.8*nrow(test00s_1))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge00s_1 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test00s_1 <- data.frame(sort(predict(cv_ridge00s_1, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test00s_1$variabl <- rownames(df_test00s_1)
colnames(df_test00s_1) <- c("coeff1", "Predictor")
predicted00s_1 <- predict(cv_ridge00s_1, s = cv_ridge00s_1$lambda.1se, newx = x_test)
mse00s_1 <- mean((y_test - predicted00s_1)^2)
df_test00s_1 <- df_test00s_1 %>% 
  select(Predictor, coeff1)
head(df_test00s_1)


# 2000s (using imputed 2 dataset)
head(test2)
str(test2)
test00s_2 <- test2 %>% 
  filter(year %in% c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009))

x <- test00s_2 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test00s_2$polviews

set.seed(124)
train_rows <- sample(1:nrow(test00s_2), 0.8*nrow(test00s_2))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge00s_2 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test00s_2 <- data.frame(sort(predict(cv_ridge00s_2, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test00s_2$variabl <- rownames(df_test00s_2)
colnames(df_test00s_2) <- c("coeff2", "Predictor")
predicted00s_2 <- predict(cv_ridge00s_2, s = cv_ridge00s_2$lambda.1se, newx = x_test)
mse00s_2 <- mean((y_test - predicted00s_2)^2)
df_test00s_2 <- df_test00s_2 %>% 
  select(Predictor, coeff2)
head(df_test00s_2)

# 2000s (using imputed 3 dataset)
head(test3)
str(test3)
test00s_3 <- test3 %>% 
  filter(year %in% c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009))

x <- test00s_3 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test00s_3$polviews

set.seed(125)
train_rows <- sample(1:nrow(test00s_3), 0.8*nrow(test00s_3))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge00s_3 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test00s_3 <- data.frame(sort(predict(cv_ridge00s_3, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test00s_3$variabl <- rownames(df_test00s_3)
colnames(df_test00s_3) <- c("coeff3", "Predictor")
predicted00s_3 <- predict(cv_ridge00s_3, s = cv_ridge00s_3$lambda.1se, newx = x_test)
mse00s_3 <- mean((y_test - predicted00s_3)^2)
df_test00s_3 <- df_test00s_3 %>% 
  select(Predictor, coeff3)
head(df_test00s_3)


# 2000s (using imputed 4 dataset)
head(test4)
str(test4)
test00s_4 <- test4 %>% 
  filter(year %in% c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009))

x <- test00s_4 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test00s_4$polviews

set.seed(126)
train_rows <- sample(1:nrow(test00s_4), 0.8*nrow(test00s_4))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge00s_4 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test00s_4 <- data.frame(sort(predict(cv_ridge00s_4, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test00s_4$variabl <- rownames(df_test00s_4)
colnames(df_test00s_4) <- c("coeff4", "Predictor")
predicted00s_4 <- predict(cv_ridge00s_4, s = cv_ridge00s_4$lambda.1se, newx = x_test)
mse00s_4 <- mean((y_test - predicted00s_4)^2)
df_test00s_4 <- df_test00s_4 %>% 
  select(Predictor, coeff4)
head(df_test00s_4)


# 2000s (using imputed 5 dataset)
head(test5)
str(test5)
test00s_5 <- test5 %>% 
  filter(year %in% c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009))

x <- test00s_5 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test00s_5$polviews

set.seed(127)
train_rows <- sample(1:nrow(test00s_5), 0.8*nrow(test00s_5))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge00s_5 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test00s_5 <- data.frame(sort(predict(cv_ridge00s_5, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test00s_5$variabl <- rownames(df_test00s_5)
colnames(df_test00s_5) <- c("coeff5", "Predictor")
predicted00s_5 <- predict(cv_ridge00s_5, s = cv_ridge00s_5$lambda.1se, newx = x_test)
mse00s_5 <- mean((y_test - predicted00s_5)^2)
df_test00s_5 <- df_test00s_5 %>% 
  select(Predictor, coeff5)
head(df_test00s_5)

df_test_00s <- merge(df_test00s_1, df_test00s_2, by = "Predictor", all.x = TRUE)
df_test_00s <- merge(df_test_00s, df_test00s_3, by = "Predictor", all.x = TRUE)
df_test_00s <- merge(df_test_00s, df_test00s_4, by = "Predictor", all.x = TRUE)
df_test_00s <- merge(df_test_00s, df_test00s_5, by = "Predictor", all.x = TRUE)
head(df_test_00s)

df_test_00s$max <- apply(df_test_00s[,2:6], 1, max)
df_test_00s$min <- apply(df_test_00s[,2:6], 1, min)

df_test_00s <- df_test_00s[-1,]

df_test_00s <- df_test_00s %>% 
  arrange(desc(max))

head(df_test_00s)
cle_00s <- df_test_00s %>% 
  select(Predictor, max, min)
cle_00s <- cle_00s[1:10,]
head(cle_00s)
cle_00s <- gather(cle_00s, category, value, 2:3)

plot00s <- cle_00s %>% 
  ggplot(aes(value, fct_reorder2(Predictor, category == "max", value, .desc = FALSE), color = category)) +
  geom_point(size = 2) +
  geom_line(aes(group = Predictor), color = "darkgrey") +
  scale_color_manual(values = c("#f4352c", "#042c44")) +
  ylab("") +
  ggtitle("2000s") +
  xlab(expression(hat(beta)^'Ridge')) +
  xlim(0, 0.53) +
  theme_dotplot


# 2010s
# 2010s (using imputed 1 dataset)
head(test1)
str(test1)
test10s_1 <- test1 %>% 
  filter(year %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))

x <- test10s_1 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test10s_1$polviews

set.seed(123)
train_rows <- sample(1:nrow(test10s_1), 0.8*nrow(test10s_1))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge10s_1 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test10s_1 <- data.frame(sort(predict(cv_ridge10s_1, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test10s_1$variabl <- rownames(df_test10s_1)
colnames(df_test10s_1) <- c("coeff1", "Predictor")
predicted10s_1 <- predict(cv_ridge10s_1, s = cv_ridge10s_1$lambda.1se, newx = x_test)
mse10s_1 <- mean((y_test - predicted10s_1)^2)
df_test10s_1 <- df_test10s_1 %>% 
  select(Predictor, coeff1)
head(df_test10s_1)


# 2010s (using imputed 2 dataset)
head(test2)
str(test2)
test10s_2 <- test2 %>% 
  filter(year %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))

x <- test10s_2 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test10s_2$polviews

set.seed(124)
train_rows <- sample(1:nrow(test10s_2), 0.8*nrow(test10s_2))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge10s_2 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test10s_2 <- data.frame(sort(predict(cv_ridge10s_2, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test10s_2$variabl <- rownames(df_test10s_2)
colnames(df_test10s_2) <- c("coeff2", "Predictor")
predicted10s_2 <- predict(cv_ridge10s_2, s = cv_ridge10s_2$lambda.1se, newx = x_test)
mse10s_2 <- mean((y_test - predicted10s_2)^2)
df_test10s_2 <- df_test10s_2 %>% 
  select(Predictor, coeff2)
head(df_test10s_2)

# 2010s (using imputed 3 dataset)
head(test3)
str(test3)
test10s_3 <- test3 %>% 
  filter(year %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))

x <- test10s_3 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test10s_3$polviews

set.seed(125)
train_rows <- sample(1:nrow(test10s_3), 0.8*nrow(test10s_3))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge10s_3 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test10s_3 <- data.frame(sort(predict(cv_ridge10s_3, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test10s_3$variabl <- rownames(df_test10s_3)
colnames(df_test10s_3) <- c("coeff3", "Predictor")
predicted10s_3 <- predict(cv_ridge10s_3, s = cv_ridge10s_3$lambda.1se, newx = x_test)
mse10s_3 <- mean((y_test - predicted10s_3)^2)
df_test10s_3 <- df_test10s_3 %>% 
  select(Predictor, coeff3)
head(df_test10s_3)


# 2010s (using imputed 4 dataset)
head(test4)
str(test4)
test10s_4 <- test4 %>% 
  filter(year %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))

x <- test10s_4 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test10s_4$polviews

set.seed(126)
train_rows <- sample(1:nrow(test10s_4), 0.8*nrow(test10s_4))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge10s_4 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test10s_4 <- data.frame(sort(predict(cv_ridge10s_4, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test10s_4$variabl <- rownames(df_test10s_4)
colnames(df_test10s_4) <- c("coeff4", "Predictor")
predicted10s_4 <- predict(cv_ridge10s_4, s = cv_ridge10s_4$lambda.1se, newx = x_test)
mse10s_4 <- mean((y_test - predicted10s_4)^2)
df_test10s_4 <- df_test10s_4 %>% 
  select(Predictor, coeff4)
head(df_test10s_4)


# 2010s (using imputed 5 dataset)
head(test5)
str(test5)
test10s_5 <- test5 %>% 
  filter(year %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))

x <- test10s_5 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test10s_5$polviews

set.seed(127)
train_rows <- sample(1:nrow(test10s_5), 0.8*nrow(test10s_5))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge10s_5 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test10s_5 <- data.frame(sort(predict(cv_ridge10s_5, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test10s_5$variabl <- rownames(df_test10s_5)
colnames(df_test10s_5) <- c("coeff5", "Predictor")
predicted10s_5 <- predict(cv_ridge10s_5, s = cv_ridge10s_5$lambda.1se, newx = x_test)
mse10s_5 <- mean((y_test - predicted10s_5)^2)
df_test10s_5 <- df_test10s_5 %>% 
  select(Predictor, coeff5)
head(df_test10s_5)

df_test_10s <- merge(df_test10s_1, df_test10s_2, by = "Predictor", all.x = TRUE)
df_test_10s <- merge(df_test_10s, df_test10s_3, by = "Predictor", all.x = TRUE)
df_test_10s <- merge(df_test_10s, df_test10s_4, by = "Predictor", all.x = TRUE)
df_test_10s <- merge(df_test_10s, df_test10s_5, by = "Predictor", all.x = TRUE)
head(df_test_10s)

df_test_10s$max <- apply(df_test_10s[,2:6], 1, max)
df_test_10s$min <- apply(df_test_10s[,2:6], 1, min)

df_test_10s <- df_test_10s[-1,]

df_test_10s <- df_test_10s %>% 
  arrange(desc(max))

head(df_test_10s)
cle_10s <- df_test_10s %>% 
  select(Predictor, max, min)
cle_10s <- cle_10s[1:10,]
head(cle_10s)
cle_10s <- gather(cle_10s, category, value, 2:3)

plot10s <- cle_10s %>% 
  ggplot(aes(value, fct_reorder2(Predictor, category == "max", value, .desc = FALSE), color = category)) +
  geom_point(size = 2) +
  geom_line(aes(group = Predictor), color = "darkgrey") +
  scale_color_manual(values = c("#f4352c", "#042c44")) +
  ylab("") +
  ggtitle("2010s") +
  xlab(expression(hat(beta)^'Ridge')) +
  xlim(0, 0.53) +
  theme_dotplot

# overall
# all (using imputed 1 dataset)
head(test1)
str(test1)
testall_1 <- test1

x <- testall_1 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- testall_1$polviews

set.seed(123)
train_rows <- sample(1:nrow(testall_1), 0.8*nrow(testall_1))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridgeall_1 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_testall_1 <- data.frame(sort(predict(cv_ridgeall_1, type = "coefficients", s = optimal_lambda)[1:39,]))
df_testall_1$variabl <- rownames(df_testall_1)
colnames(df_testall_1) <- c("coeff1", "Predictor")
predictedall_1 <- predict(cv_ridgeall_1, s = cv_ridgeall_1$lambda.1se, newx = x_test)
mseall_1 <- mean((y_test - predictedall_1)^2)
df_testall_1 <- df_testall_1 %>% 
  select(Predictor, coeff1)
head(df_testall_1)


# all (using imputed 2 dataset)
head(test2)
str(test2)
testall_2 <- test2

x <- testall_2 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- testall_2$polviews

set.seed(124)
train_rows <- sample(1:nrow(testall_2), 0.8*nrow(testall_2))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridgeall_2 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_testall_2 <- data.frame(sort(predict(cv_ridgeall_2, type = "coefficients", s = optimal_lambda)[1:39,]))
df_testall_2$variabl <- rownames(df_testall_2)
colnames(df_testall_2) <- c("coeff2", "Predictor")
predictedall_2 <- predict(cv_ridgeall_2, s = cv_ridgeall_2$lambda.1se, newx = x_test)
mseall_2 <- mean((y_test - predictedall_2)^2)
df_testall_2 <- df_testall_2 %>% 
  select(Predictor, coeff2)
head(df_testall_2)

# all (using imputed 3 dataset)
head(test3)
str(test3)
testall_3 <- test3

x <- testall_3 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- testall_3$polviews

set.seed(125)
train_rows <- sample(1:nrow(testall_3), 0.8*nrow(testall_3))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridgeall_3 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_testall_3 <- data.frame(sort(predict(cv_ridgeall_3, type = "coefficients", s = optimal_lambda)[1:39,]))
df_testall_3$variabl <- rownames(df_testall_3)
colnames(df_testall_3) <- c("coeff3", "Predictor")
predictedall_3 <- predict(cv_ridgeall_3, s = cv_ridgeall_3$lambda.1se, newx = x_test)
mseall_3 <- mean((y_test - predictedall_3)^2)
df_testall_3 <- df_testall_3 %>% 
  select(Predictor, coeff3)
head(df_testall_3)


# all (using imputed 4 dataset)
head(test4)
str(test4)
testall_4 <- test4

x <- testall_4 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- testall_4$polviews

set.seed(126)
train_rows <- sample(1:nrow(testall_4), 0.8*nrow(testall_4))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridgeall_4 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_testall_4 <- data.frame(sort(predict(cv_ridgeall_4, type = "coefficients", s = optimal_lambda)[1:39,]))
df_testall_4$variabl <- rownames(df_testall_4)
colnames(df_testall_4) <- c("coeff4", "Predictor")
predictedall_4 <- predict(cv_ridgeall_4, s = cv_ridgeall_4$lambda.1se, newx = x_test)
mseall_4 <- mean((y_test - predictedall_4)^2)
df_testall_4 <- df_testall_4 %>% 
  select(Predictor, coeff4)
head(df_testall_4)


# all (using imputed 5 dataset)
head(test5)
str(test5)
testall_5 <- test5

x <- testall_5 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- testall_5$polviews

set.seed(127)
train_rows <- sample(1:nrow(testall_5), 0.8*nrow(testall_5))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridgeall_5 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_testall_5 <- data.frame(sort(predict(cv_ridgeall_5, type = "coefficients", s = optimal_lambda)[1:39,]))
df_testall_5$variabl <- rownames(df_testall_5)
colnames(df_testall_5) <- c("coeff5", "Predictor")
predictedall_5 <- predict(cv_ridgeall_5, s = cv_ridgeall_5$lambda.1se, newx = x_test)
mseall_5 <- mean((y_test - predictedall_5)^2)
df_testall_5 <- df_testall_5 %>% 
  select(Predictor, coeff5)
head(df_testall_5)

df_test_all <- merge(df_testall_1, df_testall_2, by = "Predictor", all.x = TRUE)
df_test_all <- merge(df_test_all, df_testall_3, by = "Predictor", all.x = TRUE)
df_test_all <- merge(df_test_all, df_testall_4, by = "Predictor", all.x = TRUE)
df_test_all <- merge(df_test_all, df_testall_5, by = "Predictor", all.x = TRUE)
head(df_test_all)

df_test_all$max <- apply(df_test_all[,2:6], 1, max)
df_test_all$min <- apply(df_test_all[,2:6], 1, min)

df_test_all <- df_test_all[-1,]

df_test_all <- df_test_all %>% 
  arrange(desc(max))

head(df_test_all)
cle_all <- df_test_all %>% 
  select(Predictor, max, min)
cle_all <- cle_all[1:10,]
head(cle_all)
cle_all <- gather(cle_all, category, value, 2:3)

plotall <- cle_all %>% 
  ggplot(aes(value, fct_reorder2(Predictor, category == "max", value, .desc = FALSE), color = category)) +
  geom_point(size = 2) +
  geom_line(aes(group = Predictor), color = "darkgrey") +
  scale_color_manual(values = c("#f4352c", "#042c44"), labels = c("Max", "Min")) +
  ylab("") + xlab("") +
  ggtitle("Overall") +
  xlim(0, 0.53) +
  theme_dotplot +
  theme(legend.position = c(0.7, 0.25), legend.key.size = unit(1, 'lines'), legend.title = element_blank())

# Combine plots
gridExtra::grid.arrange(plotall, plot70s, plot80s, plot90s, plot00s, plot10s, nrow = 2, ncol = 3)



##########################################
## Stage 2: Mixed-effect Cohort Analysis##
##########################################

# Cohort 3
df_coh3 <- gss2 %>% 
  mutate_at(vars(gen_coh3, busing, racopen, racmar, postlife, reliten, fund, grass, gunlaw, cappun, libhomo, ballot, 
                 suicide1, letdie1, xmovie, pornlaw, abany, absingle, abhlth, fepres, fework, fehome, spkhomo, libcom, spkcom, 
                 spkrac, libath, spkath, year), funs(as.factor(.)))

df_coh3 <- df_coh3 %>% 
  select(gen_coh3, polviews, year, age, homosex, eqwlth, premarsx, natheal, educ, lnIncome18, lnRincome18, finrela, satfin, satjob, 
         childs, health, happy)

str(df_coh3)
unique(df_coh3$happy)
df_coh3 <- df_coh3 %>% 
  mutate(happy = ifelse(happy == 3, 1,
                        ifelse(happy == 2, 2,
                               ifelse(happy == 1, 3, NA))))

df_coh3 <- df_coh3 %>% 
  dplyr::filter(!is.na(gen_coh3), !is.na(polviews)) %>% 
  dplyr::group_by(gen_coh3, year) %>% 
  dplyr::summarise(N = dplyr::n(),
                   mean_polviews = mean(polviews, na.rm = TRUE),
                   mean_homosex = mean(homosex, na.rm = TRUE),
                   mean_eqwlth = mean(eqwlth, na.rm = TRUE),
                   mean_premarsx = mean(premarsx, na.rm = TRUE),
                   mean_educ = mean(educ, na.rm = TRUE),
                   mean_satfin = mean(satfin, na.rm = TRUE),
                   mean_satjob = mean(satjob, na.rm = TRUE),
                   mean_finrela = mean(finrela, na.rm = TRUE),
                   mean_rinc = mean(lnRincome18, na.rm = TRUE),
                   mean_inc = mean(lnIncome18, na.rm = TRUE),
                   mean_childs = mean(childs, na.rm = TRUE),
                   mean_health = mean(health, na.rm = TRUE),
                   mean_happy = mean(happy, na.rm = TRUE),
                   mean_age = mean(age, na.rm = TRUE)) %>% 
  ungroup()


#normFunc <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}

#test_coh3 <- df_coh3 %>% 
  dplyr::select(mean_polviews, mean_happy, mean_rinc, mean_age, mean_educ, mean_satfin, mean_finrela, mean_satjob, gen_coh3)

#test_coh3[,1:8] <- apply(test_coh3[,1:8], 2, normFunc)

#test2_coh3 <- df_coh3 %>% 
  dplyr::select(mean_polviews, mean_happy, mean_rinc, mean_age, mean_educ, mean_satfin, mean_finrela, mean_satjob, gen_coh3)
#test2_coh3[,1:8] <- apply(test2_coh3[,1:8], 2, minmax)

## Multilevel Mixed-effect model
fit_coh3 <- lmer(mean_polviews ~ I(mean_happy**2) + mean_happy + mean_rinc + mean_finrela + mean_age + mean_educ + (1|gen_coh3), data = df_coh3[df_coh3$mean_happy != 1,])
summary(fit_coh3)

fit_coh3 <- lmer(mean_polviews ~ mean_satfin + I(mean_rinc**2) + mean_rinc + mean_age + mean_educ + (1|gen_coh3), data = test_coh3)

df_coh3 %>% 
  ggplot(aes(mean_happy, mean_polviews)) + geom_point() + geom_smooth(method = 'loess')

df_coh5 %>% 
  ggplot(aes(mean_happy, mean_polviews)) + geom_point() + geom_smooth(method = 'loess')

df_coh10 %>% 
  ggplot(aes(mean_happy, mean_polviews)) + geom_point() + geom_smooth(method = 'loess')

(min(df_coh3$mean_happy) - mean(df_coh3$mean_happy)) / sd(df_coh3$mean_happy)
sd(df_coh3$mean_happy)
min(test_coh3$mean_happy)

# Cohort 5
library(dplyr)
df_coh5 <- gss2 %>% 
  mutate_at(vars(gen_coh5, busing, racopen, racmar, postlife, reliten, fund, grass, gunlaw, cappun, libhomo, ballot, 
                 suicide1, letdie1, xmovie, pornlaw, abany, absingle, abhlth, fepres, fework, fehome, spkhomo, libcom, spkcom, 
                 spkrac, libath, spkath, year), funs(as.factor(.)))

df_coh5 <- df_coh5 %>% 
  select(gen_coh5, polviews, year, age, homosex, eqwlth, premarsx, natheal, educ, lnIncome18, lnRincome18, finrela, satfin, satjob, 
         childs, health, happy)

str(df_coh5)

df_coh5 <- df_coh5 %>% 
  mutate(happy = ifelse(happy == 3, 1,
                        ifelse(happy == 2, 2,
                               ifelse(happy == 1, 3, NA))))

df_coh5 <- df_coh5 %>% 
  dplyr::filter(!is.na(gen_coh5), !is.na(polviews)) %>% 
  dplyr::group_by(gen_coh5, year) %>% 
  dplyr::summarise(N = dplyr::n(),
                   mean_polviews = mean(polviews, na.rm = TRUE),
                   mean_homosex = mean(homosex, na.rm = TRUE),
                   mean_eqwlth = mean(eqwlth, na.rm = TRUE),
                   mean_premarsx = mean(premarsx, na.rm = TRUE),
                   mean_educ = mean(educ, na.rm = TRUE),
                   mean_satfin = mean(satfin, na.rm = TRUE),
                   mean_satjob = mean(satjob, na.rm = TRUE),
                   mean_finrela = mean(finrela, na.rm = TRUE),
                   mean_rinc = mean(lnRincome18, na.rm = TRUE),
                   mean_inc = mean(lnIncome18, na.rm = TRUE),
                   mean_childs = mean(childs, na.rm = TRUE),
                   mean_health = mean(health, na.rm = TRUE),
                   mean_happy = mean(happy, na.rm = TRUE),
                   mean_age = mean(age, na.rm = TRUE)) %>% 
  ungroup()

## Multilevel Mixed-effect model
fit_coh5 <- lmer(mean_polviews ~ I(mean_happy**2) + mean_happy + mean_rinc + mean_age + mean_educ + (1|gen_coh5), data = df_coh5)
summary(fit_coh5)


# Cohort 10
df_coh10 <- gss2 %>% 
  mutate_at(vars(gen_coh10, busing, racopen, racmar, postlife, reliten, fund, grass, gunlaw, cappun, libhomo, ballot, 
                 suicide1, letdie1, xmovie, pornlaw, abany, absingle, abhlth, fepres, fework, fehome, spkhomo, libcom, spkcom,
                 spkrac, libath, spkath, year), funs(as.factor(.)))

df_coh10 <- df_coh10 %>% 
  select(gen_coh10, polviews, year, age, homosex, eqwlth, premarsx, natheal, educ, lnIncome18, lnRincome18, finrela, satfin, satjob, 
         childs, health, happy)

str(df_coh10)

df_coh10 <- df_coh10 %>% 
  mutate(happy = ifelse(happy == 3, 1,
                        ifelse(happy == 2, 2,
                               ifelse(happy == 1, 3, NA))))


df_coh10 <- df_coh10 %>% 
  dplyr::filter(!is.na(gen_coh10), !is.na(polviews)) %>% 
  dplyr::group_by(gen_coh10, year) %>% 
  dplyr::summarise(N = dplyr::n(),
                   mean_polviews = mean(polviews, na.rm = TRUE),
                   mean_homosex = mean(homosex, na.rm = TRUE),
                   mean_eqwlth = mean(eqwlth, na.rm = TRUE),
                   mean_premarsx = mean(premarsx, na.rm = TRUE),
                   mean_educ = mean(educ, na.rm = TRUE),
                   mean_satfin = mean(satfin, na.rm = TRUE),
                   mean_satjob = mean(satjob, na.rm = TRUE),
                   mean_finrela = mean(finrela, na.rm = TRUE),
                   mean_rinc = mean(lnRincome18, na.rm = TRUE),
                   mean_inc = mean(lnIncome18, na.rm = TRUE),
                   mean_childs = mean(childs, na.rm = TRUE),
                   mean_health = mean(health, na.rm = TRUE),
                   mean_happy = mean(happy, na.rm = TRUE),
                   mean_age = mean(age, na.rm = TRUE)) %>% 
  ungroup()

## Multilevel Mixed-effect model
fit_coh10 <- lmer(mean_polviews ~ I(mean_happy**2) + mean_happy + mean_rinc + mean_age + mean_educ + (1|gen_coh10), data = df_coh10)
summary(fit_coh10)


# PEW Cohort
df_coh <- gss2 %>% 
  mutate_at(vars(gen_coh, busing, racopen, racmar, postlife, reliten, fund, grass, gunlaw, cappun, libhomo, ballot, 
                 suicide1, letdie1, xmovie, pornlaw, abany, absingle, abhlth, fepres, fework, fehome, spkhomo, libcom, spkcom,
                 spkrac, libath, spkath, year), funs(as.factor(.)))

df_coh <- df_coh %>% 
  select(gen_coh, polviews, year, age, homosex, eqwlth, premarsx, natheal, educ, lnIncome18, lnRincome18, finrela, satfin, satjob, 
         childs, health, happy)

str(df_coh)

df_coh <- df_coh %>% 
  mutate(happy = ifelse(happy == 3, 1,
                        ifelse(happy == 2, 2,
                               ifelse(happy == 1, 3, NA))))

df_coh <- df_coh %>% 
  dplyr::filter(!is.na(gen_coh), !is.na(polviews)) %>% 
  dplyr::group_by(gen_coh, year) %>% 
  dplyr::summarise(N = dplyr::n(),
                   mean_polviews = mean(polviews, na.rm = TRUE),
                   mean_homosex = mean(homosex, na.rm = TRUE),
                   mean_eqwlth = mean(eqwlth, na.rm = TRUE),
                   mean_premarsx = mean(premarsx, na.rm = TRUE),
                   mean_educ = mean(educ, na.rm = TRUE),
                   mean_satfin = mean(satfin, na.rm = TRUE),
                   mean_satjob = mean(satjob, na.rm = TRUE),
                   mean_finrela = mean(finrela, na.rm = TRUE),
                   mean_rinc = mean(lnRincome18, na.rm = TRUE),
                   mean_inc = mean(lnIncome18, na.rm = TRUE),
                   mean_childs = mean(childs, na.rm = TRUE),
                   mean_health = mean(health, na.rm = TRUE),
                   mean_happy = mean(happy, na.rm = TRUE),
                   mean_age = mean(age, na.rm = TRUE)) %>% 
  ungroup()

## Multilevel Mixed-effect model
fit_coh <- lmer(mean_polviews ~ I(mean_happy**2) + mean_happy + mean_rinc + mean_age + mean_educ + (1|gen_coh), data = df_coh)
summary(fit_coh)


logLik(fit_coh3)
logLik(fit_coh3)

class(fit_coh3)
class(fit_coh3) <- "lmerMod"
class(fit_coh5) <- "lmerMod"
class(fit_coh10) <- "lmerMod"
class(fit_coh) <- "lmerMod"
library(stargazer)
stargazer
stargazer(fit_coh3, fit_coh5, fit_coh10, fit_coh, type = "html", out = "table_new2.doc")



###############
###############
###############
###############
# Plot for result
fit_coh3 <- lmer(mean_homosex ~ mean_happy + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh3), data = df_coh3)
summary(fit_coh3)
head(df_coh3)

df_coh3 %>% 
  ggplot(aes(mean_happy, mean_homosex)) + geom_point()



#######################
### Drawing Figures ###
#######################

# reorder the cohort
gss$gen_coh <- factor(gss$gen_coh, levels = c("Lost Generation", "Greatest Generation", "Silent Generation", "Baby Boomers", "Generation X", "Millenials", "Generation Z"))

gss %>% 
  filter(!is.na(homosex), !is.na(gen_coh)) %>% 
  group_by(gen_coh, homosex) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)*100) %>% 
  ggplot(aes(factor(homosex), perc, fill = gen_coh)) +
  geom_bar(width = 0.9, position = position_dodge(width = 0.9), stat = "identity") +
  theme_bw()


library(tidyverse)

# Summarizing dataset by generation for figure
gss4 <- gss2 %>% 
  select(year, age, homosex, polviews, gen_coh, cappun, premarsx, abany) %>% 
  filter(!is.na(gen_coh), !(is.na(age)), age != 99)

table(gss4$homosex)
table(gss4$cappun)
table(gss4$premarsx)
table(gss4$abany)
table(gss4$polviews)

library(plyr)
sum_gss <- ddply(gss4, c("gen_coh", "age"), summarise,
                 mean_homo = mean(homosex, na.rm = TRUE),
                 sd_homo = sd(homosex, na.rm = TRUE),
                 mean_cappun = mean(cappun, na.rm = TRUE),
                 sd_cappun = sd(cappun, na.rm = TRUE),
                 mean_premarsx = mean(premarsx, na.rm = TRUE),
                 sd_premarsx = sd(premarsx, na.rm = TRUE),
                 mean_abany = mean(abany, na.rm = TRUE),
                 sd_abany = sd(abany, na.rm = TRUE),
                 mean_polviews = mean(polviews, na.rm = TRUE),
                 sd_polviews = sd(polviews, na.rm = TRUE))
head(sum_gss)


# Figure 2
Blues2 <- c("#08306B", "#08519C", "#2171B5", "#4292C6", "#6BAED6", "#9ECAE1", "#C6DBEF")
head(sum_gss)
sum_gss$gen_coh <- factor(sum_gss$gen_coh, levels = c("Generation Z",
                                                      "Millenials",
                                                      "Generation X",
                                                      "Baby Boomers",
                                                      "Silent Generation",
                                                      "Greatest Generation", 
                                                      "Lost Generation"),
                          labels = c("Generation Z (born 1997-2000)",
                                     "Millenials (born 1981-1996)",
                                     "Generation X (born 1965-1980)",
                                     "Baby Boomers (born 1946-1964)",
                                     "Silent Generation (born 1928-1945)",
                                     "Greatest Generation (born 1901-1927)", 
                                     "Lost Generation (born 1883-1900)")) 

names(wes_palettes)
Blues2 <- wes_palette("Darjeeling2", 5, type = "discrete")
Blues2 <- wes_palette("FantasticFox1", 5, type = "discrete")
Blues2 <- wes_palette("Cavalcanti1", 5, type = "discrete")
Blues2 <- wes_palette("Royal2", 5, type = "discrete")

# Plot for polviews
sum_gss %>%
  filter(gen_coh != "Lost Generation (born 1883-1900)", gen_coh != "Generation Z (born 1997-2000)") %>% 
  ggplot(aes(age, mean_polviews, color = gen_coh)) +
  geom_point(aes(shape = gen_coh)) +
  theme_bw() +
  scale_color_manual(values = Blues2) +
  labs(x = "Age", y = "Political Views by Generation") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_shape_manual(values = 0:8) +
  theme(legend.position = c(0.7, 0.83), legend.key.size = unit(1, 'lines'), legend.title = element_blank()) +
  geom_smooth(method = 'loess', se = FALSE)


# Plot for homosex (with line)
sum_gss %>%
  filter(gen_coh != "Lost Generation (born 1883-1900)", gen_coh != "Generation Z (born 1997-2000)") %>% 
  ggplot(aes(age, mean_homo, color = gen_coh)) +
  geom_point(aes(shape = gen_coh)) +
  geom_line(aes(group = gen_coh, linetype = gen_coh)) +
  theme_bw() +
  scale_color_manual(values = Blues2) +
  labs(x = "Age", y = "Attitudes to Homosexual Sex Relations by Generation ") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_shape_manual(values = 0:8) +
  theme(legend.position = c(0.8, 0.83), legend.key.size = unit(1, 'lines'), legend.title = element_blank()) +
  geom_smooth(method = 'loess', se = FALSE)

# Plot for homosex (only loess)
g1 <- sum_gss %>%
  filter(gen_coh != "Lost Generation (born 1883-1900)", gen_coh != "Generation Z (born 1997-2000)") %>% 
  ggplot(aes(age, mean_homo, color = gen_coh)) +
  theme_bw() +
  scale_color_manual(values = Blues2) +
  labs(x = "Age", y = "Homosexual Sex Relations", title = "Homosexuality (1: Always wrong, 4:Not wrong at all)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  scale_shape_manual(values = 0:8) +
  geom_smooth(method = 'loess', se = FALSE)

# Plot for death penalty
g2 <- sum_gss %>%
  filter(gen_coh != "Lost Generation (born 1883-1900)", gen_coh != "Generation Z (born 1997-2000)") %>% 
  ggplot(aes(age, mean_cappun, color = gen_coh)) +
  theme_bw() +
  scale_color_manual(values = Blues2) +
  labs(x = "Age", y = "Death Penalty for Murder", title = "Capital Punishment (0: Favor, 1: Oppose)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_shape_manual(values = 0:8) +
  theme(legend.position = c(0.66, 0.8), legend.key.size = unit(1, 'lines'), legend.title = element_blank()) +
  geom_smooth(method = 'loess', se = FALSE)

# Plot for premarsx
g3 <- sum_gss %>%
  filter(gen_coh != "Lost Generation (born 1883-1900)", gen_coh != "Generation Z (born 1997-2000)") %>% 
  ggplot(aes(age, mean_premarsx, color = gen_coh)) +
  theme_bw() +
  scale_color_manual(values = Blues2) +
  labs(x = "Age", y = "Sex before Marriage", title = "Premarital Sex (1: Always wrong, 4:Not wrong at all)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  scale_shape_manual(values = 0:8) +
  geom_smooth(method = 'loess', se = FALSE)


# Plot for abortion
g4 <- sum_gss %>%
  filter(gen_coh != "Lost Generation (born 1883-1900)", gen_coh != "Generation Z (born 1997-2000)") %>% 
  ggplot(aes(age, mean_abany, color = gen_coh)) +
  theme_bw() +
  scale_color_manual(values = Blues2) +
  labs(x = "Age", y = "Abortion for Any Reason", title = "Abortion (0: No, 1: Yes)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  scale_shape_manual(values = 0:8) +
  geom_smooth(method = 'loess', se = FALSE)

gridExtra::grid.arrange(g1, g2, g3, g4, nrow = 2, ncol = 2)



##############################
### Descriptive Statistics ###
##############################

# Descriptive statistics
dstat <- df_ols %>% 
  dplyr::select(polviews, homosex, eqwlth, helpsick, gunlaw, abany, letdie1, cappun, grass, tax)

?summary_table
summary_table(dstat)
head(dstat)
install.packages("sjPlot")
install.packages("qwraps2")
library(sjPlot)
library(qwraps2)

stargazer(dstat, type = "html", out = "dstat1.doc", digits = 2)
sjt.df(dstat, file = "dstat2.doc")

str(dstat)
dstat$gunlaw <- as.numeric(dstat$gunlaw)
dstat$abany <- as.numeric(dstat$abany)
dstat$letdie1 <- as.numeric(dstat$letdie1)
dstat$cappun <- as.numeric(dstat$cappun)
dstat$grass <- as.numeric(dstat$grass)

write_dta(dstat, "dstat.dta")
write.csv(dstat, "dstat.csv")

dstat2 <- read.csv("dstat.csv")
str(dstat2)
stargazer(dstat2, type = "html", digits=2, out="dstat3.doc",
          covariate.labels=c("Political orientation","Homosexuality","Equal wealth",
                             "Medical care","Permit for gun buy","Abortion",
                             "Euthanasia","Death penalty","Smoking marijuana",
                             "Income tax"))

head(df_pp_3)
stargazer(df_pp_3, type = "html", digits=2, out="dstat_pp1.doc")


#########################
# Cohort 3
head(df_coh3)

dstat_coh3 <- df_coh3 %>% 
  dplyr::select(gen_coh3, mean_polviews, mean_happy, mean_rinc, mean_finrela, mean_age, mean_educ) %>% 
  dplyr::group_by(gen_coh3) %>% 
  dplyr::summarise(n(), mean(mean_polviews, na.rm=TRUE), mean(mean_happy, na.rm=TRUE), mean(mean_rinc, na.rm=TRUE), 
                   mean(mean_finrela, na.rm=TRUE), mean(mean_age, na.rm=TRUE), mean(mean_educ, , na.rm=TRUE))

head(dstat_coh3)
colnames(dstat_coh3) <- c("Cohorts", "N", "Polviews", "Satisfaction", "Income", "Sub_income", "Age", "Education")

write.csv(dstat_coh3, "dstat_coh3.csv")

# Cohort 5
head(df_coh5)

dstat_coh5 <- df_coh5 %>% 
  dplyr::select(gen_coh5, mean_polviews, mean_happy, mean_rinc, mean_finrela, mean_age, mean_educ) %>% 
  dplyr::group_by(gen_coh5) %>% 
  dplyr::summarise(n(), mean(mean_polviews, na.rm=TRUE), mean(mean_happy, na.rm=TRUE), mean(mean_rinc, na.rm=TRUE), 
                   mean(mean_finrela, na.rm=TRUE), mean(mean_age, na.rm=TRUE), mean(mean_educ, , na.rm=TRUE))

head(dstat_coh5)
colnames(dstat_coh5) <- c("Cohorts", "N", "Polviews", "Satisfaction", "Income", "Sub_income", "Age", "Education")

write.csv(dstat_coh5, "dstat_coh5.csv")


# Cohort 10
head(df_coh10)

dstat_coh10 <- df_coh10 %>% 
  dplyr::select(gen_coh10, mean_polviews, mean_happy, mean_rinc, mean_finrela, mean_age, mean_educ) %>% 
  dplyr::group_by(gen_coh10) %>% 
  dplyr::summarise(n(), mean(mean_polviews, na.rm=TRUE), mean(mean_happy, na.rm=TRUE), mean(mean_rinc, na.rm=TRUE), 
                   mean(mean_finrela, na.rm=TRUE), mean(mean_age, na.rm=TRUE), mean(mean_educ, , na.rm=TRUE))

head(dstat_coh10)
colnames(dstat_coh10) <- c("Cohorts", "N", "Polviews", "Satisfaction", "Income", "Sub_income", "Age", "Education")

write.csv(dstat_coh10, "dstat_coh10.csv")


# Pew Cohort
head(df_coh)

dstat_coh <- df_coh %>% 
  dplyr::select(gen_coh, mean_polviews, mean_happy, mean_rinc, mean_finrela, mean_age, mean_educ) %>% 
  dplyr::group_by(gen_coh) %>% 
  dplyr::summarise(n(), mean(mean_polviews, na.rm=TRUE), mean(mean_happy, na.rm=TRUE), mean(mean_rinc, na.rm=TRUE), 
                   mean(mean_finrela, na.rm=TRUE), mean(mean_age, na.rm=TRUE), mean(mean_educ, , na.rm=TRUE))

head(dstat_coh)
colnames(dstat_coh) <- c("Cohorts", "N", "Polviews", "Satisfaction", "Income", "Sub_income", "Age", "Education")

write.csv(dstat_coh, "dstat_coh.csv")



##################################
## Upgrade the cleveland Figure ##
##################################

# By period
# 1970s (using imputed 1 dataset)
head(test1)
str(test1)
test70s_1 <- test1 %>% 
  filter(year %in% c(1974, 1975, 1976, 1977, 1978, 1979))

x <- test70s_1 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test70s_1$polviews

set.seed(123)
train_rows <- sample(1:nrow(test70s_1), 0.8*nrow(test70s_1))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge70s_1 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test70s_1 <- data.frame(sort(predict(cv_ridge70s_1, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test70s_1$variabl <- rownames(df_test70s_1)
colnames(df_test70s_1) <- c("coeff1", "Predictor")
predicted70s_1 <- predict(cv_ridge70s_1, s = cv_ridge70s_1$lambda.1se, newx = x_test)
mse70s_1 <- mean((y_test - predicted70s_1)^2)
df_test70s_1 <- df_test70s_1 %>% 
  select(Predictor, coeff1)
head(df_test70s_1)


# 1970s (using imputed 2 dataset)
head(test2)
str(test2)
test70s_2 <- test2 %>% 
  filter(year %in% c(1974, 1975, 1976, 1977, 1978, 1979))

x <- test70s_2 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test70s_2$polviews

set.seed(124)
train_rows <- sample(1:nrow(test70s_2), 0.8*nrow(test70s_2))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge70s_2 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test70s_2 <- data.frame(sort(predict(cv_ridge70s_2, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test70s_2$variabl <- rownames(df_test70s_2)
colnames(df_test70s_2) <- c("coeff2", "Predictor")
predicted70s_2 <- predict(cv_ridge70s_2, s = cv_ridge70s_2$lambda.1se, newx = x_test)
mse70s_2 <- mean((y_test - predicted70s_2)^2)
df_test70s_2 <- df_test70s_2 %>% 
  select(Predictor, coeff2)
head(df_test70s_2)

# 1970s (using imputed 3 dataset)
head(test3)
str(test3)
test70s_3 <- test3 %>% 
  filter(year %in% c(1974, 1975, 1976, 1977, 1978, 1979))

x <- test70s_3 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test70s_3$polviews

set.seed(125)
train_rows <- sample(1:nrow(test70s_3), 0.8*nrow(test70s_3))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge70s_3 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test70s_3 <- data.frame(sort(predict(cv_ridge70s_3, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test70s_3$variabl <- rownames(df_test70s_3)
colnames(df_test70s_3) <- c("coeff3", "Predictor")
predicted70s_3 <- predict(cv_ridge70s_3, s = cv_ridge70s_3$lambda.1se, newx = x_test)
mse70s_3 <- mean((y_test - predicted70s_3)^2)
df_test70s_3 <- df_test70s_3 %>% 
  select(Predictor, coeff3)
head(df_test70s_3)


# 1970s (using imputed 4 dataset)
head(test4)
str(test4)
test70s_4 <- test4 %>% 
  filter(year %in% c(1974, 1975, 1976, 1977, 1978, 1979))

x <- test70s_4 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test70s_4$polviews

set.seed(126)
train_rows <- sample(1:nrow(test70s_4), 0.8*nrow(test70s_4))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge70s_4 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test70s_4 <- data.frame(sort(predict(cv_ridge70s_4, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test70s_4$variabl <- rownames(df_test70s_4)
colnames(df_test70s_4) <- c("coeff4", "Predictor")
predicted70s_4 <- predict(cv_ridge70s_4, s = cv_ridge70s_4$lambda.1se, newx = x_test)
mse70s_4 <- mean((y_test - predicted70s_4)^2)
df_test70s_4 <- df_test70s_4 %>% 
  select(Predictor, coeff4)
head(df_test70s_4)


# 1970s (using imputed 5 dataset)
head(test5)
str(test5)
test70s_5 <- test5 %>% 
  filter(year %in% c(1974, 1975, 1976, 1977, 1978, 1979))

x <- test70s_5 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test70s_5$polviews

set.seed(127)
train_rows <- sample(1:nrow(test70s_5), 0.8*nrow(test70s_5))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge70s_5 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test70s_5 <- data.frame(sort(predict(cv_ridge70s_5, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test70s_5$variabl <- rownames(df_test70s_5)
colnames(df_test70s_5) <- c("coeff5", "Predictor")
predicted70s_5 <- predict(cv_ridge70s_5, s = cv_ridge70s_5$lambda.1se, newx = x_test)
mse70s_5 <- mean((y_test - predicted70s_5)^2)
df_test70s_5 <- df_test70s_5 %>% 
  select(Predictor, coeff5)
head(df_test70s_5)

df_test_70s <- merge(df_test70s_1, df_test70s_2, by = "Predictor", all.x = TRUE)
df_test_70s <- merge(df_test_70s, df_test70s_3, by = "Predictor", all.x = TRUE)
df_test_70s <- merge(df_test_70s, df_test70s_4, by = "Predictor", all.x = TRUE)
df_test_70s <- merge(df_test_70s, df_test70s_5, by = "Predictor", all.x = TRUE)
head(df_test_70s)

df_test_70s$mean <- apply(df_test_70s[,2:6], 1, mean)
df_test_70s$sd <- apply(df_test_70s[,2:6], 1, sd)

df_test_70s <- df_test_70s[-1,]

df_test_70s <- df_test_70s %>% 
  arrange(desc(mean))

head(df_test_70s)
cle_70s <- df_test_70s %>% 
  select(Predictor, mean, sd)
cle_70s <- cle_70s[1:10,]
head(cle_70s)
#cle_70s <- gather(cle_70s, category, value, 2:3)

theme_dotplot <- theme_bw(14) +
  theme(axis.text.y = element_text(size = rel(.75)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.75)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5),
        panel.grid.minor.x = element_blank(),
        legend.position = 'none')

pal <- wes_palette("Cavalcanti1", 5, type = "discrete")

plot70s <- cle_70s %>% 
  ggplot(aes(fct_reorder(Predictor, mean, .desc = FALSE), mean)) +
  geom_point(size = 2, color = "#f4352c") + 
  geom_errorbar(aes(ymin =  mean-(1.96*sd), ymax = mean+(1.96*sd), color = "darkgrey", width = .2)) +
  coord_flip()
            

geom_errorbar(aes(ymin=mean-1.96*boot_se, ymax=mean+1.96*boot_se), width=.5, color = "grey50", position = pd) +
?geom_crossbar
            +
  scale_color_manual(values = c("#f4352c", "#042c44")) +
  ylab("")  + xlab("") +
  ggtitle("1970s") +
  xlim(0, 0.53) +
  theme_dotplot


# 1980s
# 1980s (using imputed 1 dataset)
head(test1)
str(test1)
test80s_1 <- test1 %>% 
  filter(year %in% c(1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989))

x <- test80s_1 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test80s_1$polviews

set.seed(123)
train_rows <- sample(1:nrow(test80s_1), 0.8*nrow(test80s_1))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge80s_1 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test80s_1 <- data.frame(sort(predict(cv_ridge80s_1, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test80s_1$variabl <- rownames(df_test80s_1)
colnames(df_test80s_1) <- c("coeff1", "Predictor")
predicted80s_1 <- predict(cv_ridge80s_1, s = cv_ridge80s_1$lambda.1se, newx = x_test)
mse80s_1 <- mean((y_test - predicted80s_1)^2)
df_test80s_1 <- df_test80s_1 %>% 
  select(Predictor, coeff1)
head(df_test80s_1)


# 1980s (using imputed 2 dataset)
head(test2)
str(test2)
test80s_2 <- test2 %>% 
  filter(year %in% c(1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989))

x <- test80s_2 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test80s_2$polviews

set.seed(124)
train_rows <- sample(1:nrow(test80s_2), 0.8*nrow(test80s_2))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge80s_2 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test80s_2 <- data.frame(sort(predict(cv_ridge80s_2, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test80s_2$variabl <- rownames(df_test80s_2)
colnames(df_test80s_2) <- c("coeff2", "Predictor")
predicted80s_2 <- predict(cv_ridge80s_2, s = cv_ridge80s_2$lambda.1se, newx = x_test)
mse80s_2 <- mean((y_test - predicted80s_2)^2)
df_test80s_2 <- df_test80s_2 %>% 
  select(Predictor, coeff2)
head(df_test80s_2)

# 1980s (using imputed 3 dataset)
head(test3)
str(test3)
test80s_3 <- test3 %>% 
  filter(year %in% c(1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989))

x <- test80s_3 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test80s_3$polviews

set.seed(125)
train_rows <- sample(1:nrow(test80s_3), 0.8*nrow(test80s_3))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge80s_3 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test80s_3 <- data.frame(sort(predict(cv_ridge80s_3, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test80s_3$variabl <- rownames(df_test80s_3)
colnames(df_test80s_3) <- c("coeff3", "Predictor")
predicted80s_3 <- predict(cv_ridge80s_3, s = cv_ridge80s_3$lambda.1se, newx = x_test)
mse80s_3 <- mean((y_test - predicted80s_3)^2)
df_test80s_3 <- df_test80s_3 %>% 
  select(Predictor, coeff3)
head(df_test80s_3)


# 1980s (using imputed 4 dataset)
head(test4)
str(test4)
test80s_4 <- test4 %>% 
  filter(year %in% c(1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989))

x <- test80s_4 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test80s_4$polviews

set.seed(126)
train_rows <- sample(1:nrow(test80s_4), 0.8*nrow(test80s_4))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge80s_4 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test80s_4 <- data.frame(sort(predict(cv_ridge80s_4, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test80s_4$variabl <- rownames(df_test80s_4)
colnames(df_test80s_4) <- c("coeff4", "Predictor")
predicted80s_4 <- predict(cv_ridge80s_4, s = cv_ridge80s_4$lambda.1se, newx = x_test)
mse80s_4 <- mean((y_test - predicted80s_4)^2)
df_test80s_4 <- df_test80s_4 %>% 
  select(Predictor, coeff4)
head(df_test80s_4)


# 1980s (using imputed 5 dataset)
head(test5)
str(test5)
test80s_5 <- test5 %>% 
  filter(year %in% c(1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989))

x <- test80s_5 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test80s_5$polviews

set.seed(127)
train_rows <- sample(1:nrow(test80s_5), 0.8*nrow(test80s_5))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge80s_5 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test80s_5 <- data.frame(sort(predict(cv_ridge80s_5, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test80s_5$variabl <- rownames(df_test80s_5)
colnames(df_test80s_5) <- c("coeff5", "Predictor")
predicted80s_5 <- predict(cv_ridge80s_5, s = cv_ridge80s_5$lambda.1se, newx = x_test)
mse80s_5 <- mean((y_test - predicted80s_5)^2)
df_test80s_5 <- df_test80s_5 %>% 
  select(Predictor, coeff5)
head(df_test80s_5)

df_test_80s <- merge(df_test80s_1, df_test80s_2, by = "Predictor", all.x = TRUE)
df_test_80s <- merge(df_test_80s, df_test80s_3, by = "Predictor", all.x = TRUE)
df_test_80s <- merge(df_test_80s, df_test80s_4, by = "Predictor", all.x = TRUE)
df_test_80s <- merge(df_test_80s, df_test80s_5, by = "Predictor", all.x = TRUE)
head(df_test_80s)

df_test_80s$max <- apply(df_test_80s[,2:6], 1, max)
df_test_80s$min <- apply(df_test_80s[,2:6], 1, min)

df_test_80s <- df_test_80s[-1,]

df_test_80s <- df_test_80s %>% 
  arrange(desc(max))

head(df_test_80s)
cle_80s <- df_test_80s %>% 
  select(Predictor, max, min)
cle_80s <- cle_80s[1:10,]
head(cle_80s)
cle_80s <- gather(cle_80s, category, value, 2:3)

plot80s <- cle_80s %>% 
  ggplot(aes(value, fct_reorder2(Predictor, category == "max", value, .desc = FALSE), color = category)) +
  geom_point(size = 2) +
  geom_line(aes(group = Predictor), color = "darkgrey") +
  scale_color_manual(values = c("#f4352c", "#042c44")) +
  ylab("")  + xlab("") +
  ggtitle("1980s") +
  xlim(0, 0.53) +
  theme_dotplot


# 1990s
# 1990s (using imputed 1 dataset)
head(test1)
str(test1)
test90s_1 <- test1 %>% 
  filter(year %in% c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999))

x <- test90s_1 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test90s_1$polviews

set.seed(123)
train_rows <- sample(1:nrow(test90s_1), 0.8*nrow(test90s_1))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge90s_1 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test90s_1 <- data.frame(sort(predict(cv_ridge90s_1, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test90s_1$variabl <- rownames(df_test90s_1)
colnames(df_test90s_1) <- c("coeff1", "Predictor")
predicted90s_1 <- predict(cv_ridge90s_1, s = cv_ridge90s_1$lambda.1se, newx = x_test)
mse90s_1 <- mean((y_test - predicted90s_1)^2)
df_test90s_1 <- df_test90s_1 %>% 
  select(Predictor, coeff1)
head(df_test90s_1)


# 1990s (using imputed 2 dataset)
head(test2)
str(test2)
test90s_2 <- test2 %>% 
  filter(year %in% c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999))

x <- test90s_2 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test90s_2$polviews

set.seed(124)
train_rows <- sample(1:nrow(test90s_2), 0.8*nrow(test90s_2))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge90s_2 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test90s_2 <- data.frame(sort(predict(cv_ridge90s_2, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test90s_2$variabl <- rownames(df_test90s_2)
colnames(df_test90s_2) <- c("coeff2", "Predictor")
predicted90s_2 <- predict(cv_ridge90s_2, s = cv_ridge90s_2$lambda.1se, newx = x_test)
mse90s_2 <- mean((y_test - predicted90s_2)^2)
df_test90s_2 <- df_test90s_2 %>% 
  select(Predictor, coeff2)
head(df_test90s_2)

# 1990s (using imputed 3 dataset)
head(test3)
str(test3)
test90s_3 <- test3 %>% 
  filter(year %in% c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999))

x <- test90s_3 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test90s_3$polviews

set.seed(125)
train_rows <- sample(1:nrow(test90s_3), 0.8*nrow(test90s_3))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge90s_3 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test90s_3 <- data.frame(sort(predict(cv_ridge90s_3, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test90s_3$variabl <- rownames(df_test90s_3)
colnames(df_test90s_3) <- c("coeff3", "Predictor")
predicted90s_3 <- predict(cv_ridge90s_3, s = cv_ridge90s_3$lambda.1se, newx = x_test)
mse90s_3 <- mean((y_test - predicted90s_3)^2)
df_test90s_3 <- df_test90s_3 %>% 
  select(Predictor, coeff3)
head(df_test90s_3)


# 1990s (using imputed 4 dataset)
head(test4)
str(test4)
test90s_4 <- test4 %>% 
  filter(year %in% c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999))

x <- test90s_4 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test90s_4$polviews

set.seed(126)
train_rows <- sample(1:nrow(test90s_4), 0.8*nrow(test90s_4))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge90s_4 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test90s_4 <- data.frame(sort(predict(cv_ridge90s_4, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test90s_4$variabl <- rownames(df_test90s_4)
colnames(df_test90s_4) <- c("coeff4", "Predictor")
predicted90s_4 <- predict(cv_ridge90s_4, s = cv_ridge90s_4$lambda.1se, newx = x_test)
mse90s_4 <- mean((y_test - predicted90s_4)^2)
df_test90s_4 <- df_test90s_4 %>% 
  select(Predictor, coeff4)
head(df_test90s_4)


# 1990s (using imputed 5 dataset)
head(test5)
str(test5)
test90s_5 <- test5 %>% 
  filter(year %in% c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999))

x <- test90s_5 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test90s_5$polviews

set.seed(127)
train_rows <- sample(1:nrow(test90s_5), 0.8*nrow(test90s_5))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge90s_5 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test90s_5 <- data.frame(sort(predict(cv_ridge90s_5, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test90s_5$variabl <- rownames(df_test90s_5)
colnames(df_test90s_5) <- c("coeff5", "Predictor")
predicted90s_5 <- predict(cv_ridge90s_5, s = cv_ridge90s_5$lambda.1se, newx = x_test)
mse90s_5 <- mean((y_test - predicted90s_5)^2)
df_test90s_5 <- df_test90s_5 %>% 
  select(Predictor, coeff5)
head(df_test90s_5)

df_test_90s <- merge(df_test90s_1, df_test90s_2, by = "Predictor", all.x = TRUE)
df_test_90s <- merge(df_test_90s, df_test90s_3, by = "Predictor", all.x = TRUE)
df_test_90s <- merge(df_test_90s, df_test90s_4, by = "Predictor", all.x = TRUE)
df_test_90s <- merge(df_test_90s, df_test90s_5, by = "Predictor", all.x = TRUE)
head(df_test_90s)

df_test_90s$max <- apply(df_test_90s[,2:6], 1, max)
df_test_90s$min <- apply(df_test_90s[,2:6], 1, min)

df_test_90s <- df_test_90s[-1,]

df_test_90s <- df_test_90s %>% 
  arrange(desc(max))

head(df_test_90s)
cle_90s <- df_test_90s %>% 
  select(Predictor, max, min)
cle_90s <- cle_90s[1:10,]
head(cle_90s)
cle_90s <- gather(cle_90s, category, value, 2:3)

plot90s <- cle_90s %>% 
  ggplot(aes(value, fct_reorder2(Predictor, category == "max", value, .desc = FALSE), color = category)) +
  geom_point(size = 2) +
  geom_line(aes(group = Predictor), color = "darkgrey") +
  scale_color_manual(values = c("#f4352c", "#042c44")) +
  ylab("") +
  ggtitle("1990s") +
  xlab(expression(hat(beta)^'Ridge')) +
  xlim(0, 0.53) +
  theme_dotplot


# 2000s
# 2000s (using imputed 1 dataset)
head(test1)
str(test1)
test00s_1 <- test1 %>% 
  filter(year %in% c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009))

x <- test00s_1 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test00s_1$polviews

set.seed(123)
train_rows <- sample(1:nrow(test00s_1), 0.8*nrow(test00s_1))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge00s_1 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test00s_1 <- data.frame(sort(predict(cv_ridge00s_1, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test00s_1$variabl <- rownames(df_test00s_1)
colnames(df_test00s_1) <- c("coeff1", "Predictor")
predicted00s_1 <- predict(cv_ridge00s_1, s = cv_ridge00s_1$lambda.1se, newx = x_test)
mse00s_1 <- mean((y_test - predicted00s_1)^2)
df_test00s_1 <- df_test00s_1 %>% 
  select(Predictor, coeff1)
head(df_test00s_1)


# 2000s (using imputed 2 dataset)
head(test2)
str(test2)
test00s_2 <- test2 %>% 
  filter(year %in% c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009))

x <- test00s_2 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test00s_2$polviews

set.seed(124)
train_rows <- sample(1:nrow(test00s_2), 0.8*nrow(test00s_2))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge00s_2 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test00s_2 <- data.frame(sort(predict(cv_ridge00s_2, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test00s_2$variabl <- rownames(df_test00s_2)
colnames(df_test00s_2) <- c("coeff2", "Predictor")
predicted00s_2 <- predict(cv_ridge00s_2, s = cv_ridge00s_2$lambda.1se, newx = x_test)
mse00s_2 <- mean((y_test - predicted00s_2)^2)
df_test00s_2 <- df_test00s_2 %>% 
  select(Predictor, coeff2)
head(df_test00s_2)

# 2000s (using imputed 3 dataset)
head(test3)
str(test3)
test00s_3 <- test3 %>% 
  filter(year %in% c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009))

x <- test00s_3 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test00s_3$polviews

set.seed(125)
train_rows <- sample(1:nrow(test00s_3), 0.8*nrow(test00s_3))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge00s_3 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test00s_3 <- data.frame(sort(predict(cv_ridge00s_3, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test00s_3$variabl <- rownames(df_test00s_3)
colnames(df_test00s_3) <- c("coeff3", "Predictor")
predicted00s_3 <- predict(cv_ridge00s_3, s = cv_ridge00s_3$lambda.1se, newx = x_test)
mse00s_3 <- mean((y_test - predicted00s_3)^2)
df_test00s_3 <- df_test00s_3 %>% 
  select(Predictor, coeff3)
head(df_test00s_3)


# 2000s (using imputed 4 dataset)
head(test4)
str(test4)
test00s_4 <- test4 %>% 
  filter(year %in% c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009))

x <- test00s_4 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test00s_4$polviews

set.seed(126)
train_rows <- sample(1:nrow(test00s_4), 0.8*nrow(test00s_4))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge00s_4 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test00s_4 <- data.frame(sort(predict(cv_ridge00s_4, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test00s_4$variabl <- rownames(df_test00s_4)
colnames(df_test00s_4) <- c("coeff4", "Predictor")
predicted00s_4 <- predict(cv_ridge00s_4, s = cv_ridge00s_4$lambda.1se, newx = x_test)
mse00s_4 <- mean((y_test - predicted00s_4)^2)
df_test00s_4 <- df_test00s_4 %>% 
  select(Predictor, coeff4)
head(df_test00s_4)


# 2000s (using imputed 5 dataset)
head(test5)
str(test5)
test00s_5 <- test5 %>% 
  filter(year %in% c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009))

x <- test00s_5 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test00s_5$polviews

set.seed(127)
train_rows <- sample(1:nrow(test00s_5), 0.8*nrow(test00s_5))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge00s_5 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test00s_5 <- data.frame(sort(predict(cv_ridge00s_5, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test00s_5$variabl <- rownames(df_test00s_5)
colnames(df_test00s_5) <- c("coeff5", "Predictor")
predicted00s_5 <- predict(cv_ridge00s_5, s = cv_ridge00s_5$lambda.1se, newx = x_test)
mse00s_5 <- mean((y_test - predicted00s_5)^2)
df_test00s_5 <- df_test00s_5 %>% 
  select(Predictor, coeff5)
head(df_test00s_5)

df_test_00s <- merge(df_test00s_1, df_test00s_2, by = "Predictor", all.x = TRUE)
df_test_00s <- merge(df_test_00s, df_test00s_3, by = "Predictor", all.x = TRUE)
df_test_00s <- merge(df_test_00s, df_test00s_4, by = "Predictor", all.x = TRUE)
df_test_00s <- merge(df_test_00s, df_test00s_5, by = "Predictor", all.x = TRUE)
head(df_test_00s)

df_test_00s$max <- apply(df_test_00s[,2:6], 1, max)
df_test_00s$min <- apply(df_test_00s[,2:6], 1, min)

df_test_00s <- df_test_00s[-1,]

df_test_00s <- df_test_00s %>% 
  arrange(desc(max))

head(df_test_00s)
cle_00s <- df_test_00s %>% 
  select(Predictor, max, min)
cle_00s <- cle_00s[1:10,]
head(cle_00s)
cle_00s <- gather(cle_00s, category, value, 2:3)

plot00s <- cle_00s %>% 
  ggplot(aes(value, fct_reorder2(Predictor, category == "max", value, .desc = FALSE), color = category)) +
  geom_point(size = 2) +
  geom_line(aes(group = Predictor), color = "darkgrey") +
  scale_color_manual(values = c("#f4352c", "#042c44")) +
  ylab("") +
  ggtitle("2000s") +
  xlab(expression(hat(beta)^'Ridge')) +
  xlim(0, 0.53) +
  theme_dotplot


# 2010s
# 2010s (using imputed 1 dataset)
head(test1)
str(test1)
test10s_1 <- test1 %>% 
  filter(year %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))

x <- test10s_1 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test10s_1$polviews

set.seed(123)
train_rows <- sample(1:nrow(test10s_1), 0.8*nrow(test10s_1))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge10s_1 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test10s_1 <- data.frame(sort(predict(cv_ridge10s_1, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test10s_1$variabl <- rownames(df_test10s_1)
colnames(df_test10s_1) <- c("coeff1", "Predictor")
predicted10s_1 <- predict(cv_ridge10s_1, s = cv_ridge10s_1$lambda.1se, newx = x_test)
mse10s_1 <- mean((y_test - predicted10s_1)^2)
df_test10s_1 <- df_test10s_1 %>% 
  select(Predictor, coeff1)
head(df_test10s_1)


# 2010s (using imputed 2 dataset)
head(test2)
str(test2)
test10s_2 <- test2 %>% 
  filter(year %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))

x <- test10s_2 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test10s_2$polviews

set.seed(124)
train_rows <- sample(1:nrow(test10s_2), 0.8*nrow(test10s_2))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge10s_2 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test10s_2 <- data.frame(sort(predict(cv_ridge10s_2, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test10s_2$variabl <- rownames(df_test10s_2)
colnames(df_test10s_2) <- c("coeff2", "Predictor")
predicted10s_2 <- predict(cv_ridge10s_2, s = cv_ridge10s_2$lambda.1se, newx = x_test)
mse10s_2 <- mean((y_test - predicted10s_2)^2)
df_test10s_2 <- df_test10s_2 %>% 
  select(Predictor, coeff2)
head(df_test10s_2)

# 2010s (using imputed 3 dataset)
head(test3)
str(test3)
test10s_3 <- test3 %>% 
  filter(year %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))

x <- test10s_3 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test10s_3$polviews

set.seed(125)
train_rows <- sample(1:nrow(test10s_3), 0.8*nrow(test10s_3))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge10s_3 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test10s_3 <- data.frame(sort(predict(cv_ridge10s_3, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test10s_3$variabl <- rownames(df_test10s_3)
colnames(df_test10s_3) <- c("coeff3", "Predictor")
predicted10s_3 <- predict(cv_ridge10s_3, s = cv_ridge10s_3$lambda.1se, newx = x_test)
mse10s_3 <- mean((y_test - predicted10s_3)^2)
df_test10s_3 <- df_test10s_3 %>% 
  select(Predictor, coeff3)
head(df_test10s_3)


# 2010s (using imputed 4 dataset)
head(test4)
str(test4)
test10s_4 <- test4 %>% 
  filter(year %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))

x <- test10s_4 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test10s_4$polviews

set.seed(126)
train_rows <- sample(1:nrow(test10s_4), 0.8*nrow(test10s_4))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge10s_4 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test10s_4 <- data.frame(sort(predict(cv_ridge10s_4, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test10s_4$variabl <- rownames(df_test10s_4)
colnames(df_test10s_4) <- c("coeff4", "Predictor")
predicted10s_4 <- predict(cv_ridge10s_4, s = cv_ridge10s_4$lambda.1se, newx = x_test)
mse10s_4 <- mean((y_test - predicted10s_4)^2)
df_test10s_4 <- df_test10s_4 %>% 
  select(Predictor, coeff4)
head(df_test10s_4)


# 2010s (using imputed 5 dataset)
head(test5)
str(test5)
test10s_5 <- test5 %>% 
  filter(year %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))

x <- test10s_5 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test10s_5$polviews

set.seed(127)
train_rows <- sample(1:nrow(test10s_5), 0.8*nrow(test10s_5))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridge10s_5 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_test10s_5 <- data.frame(sort(predict(cv_ridge10s_5, type = "coefficients", s = optimal_lambda)[1:39,]))
df_test10s_5$variabl <- rownames(df_test10s_5)
colnames(df_test10s_5) <- c("coeff5", "Predictor")
predicted10s_5 <- predict(cv_ridge10s_5, s = cv_ridge10s_5$lambda.1se, newx = x_test)
mse10s_5 <- mean((y_test - predicted10s_5)^2)
df_test10s_5 <- df_test10s_5 %>% 
  select(Predictor, coeff5)
head(df_test10s_5)

df_test_10s <- merge(df_test10s_1, df_test10s_2, by = "Predictor", all.x = TRUE)
df_test_10s <- merge(df_test_10s, df_test10s_3, by = "Predictor", all.x = TRUE)
df_test_10s <- merge(df_test_10s, df_test10s_4, by = "Predictor", all.x = TRUE)
df_test_10s <- merge(df_test_10s, df_test10s_5, by = "Predictor", all.x = TRUE)
head(df_test_10s)

df_test_10s$max <- apply(df_test_10s[,2:6], 1, max)
df_test_10s$min <- apply(df_test_10s[,2:6], 1, min)

df_test_10s <- df_test_10s[-1,]

df_test_10s <- df_test_10s %>% 
  arrange(desc(max))

head(df_test_10s)
cle_10s <- df_test_10s %>% 
  select(Predictor, max, min)
cle_10s <- cle_10s[1:10,]
head(cle_10s)
cle_10s <- gather(cle_10s, category, value, 2:3)

plot10s <- cle_10s %>% 
  ggplot(aes(value, fct_reorder2(Predictor, category == "max", value, .desc = FALSE), color = category)) +
  geom_point(size = 2) +
  geom_line(aes(group = Predictor), color = "darkgrey") +
  scale_color_manual(values = c("#f4352c", "#042c44")) +
  ylab("") +
  ggtitle("2010s") +
  xlab(expression(hat(beta)^'Ridge')) +
  xlim(0, 0.53) +
  theme_dotplot

# overall
# all (using imputed 1 dataset)
head(test1)
str(test1)
testall_1 <- test1

x <- testall_1 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- testall_1$polviews

set.seed(123)
train_rows <- sample(1:nrow(testall_1), 0.8*nrow(testall_1))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridgeall_1 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_testall_1 <- data.frame(sort(predict(cv_ridgeall_1, type = "coefficients", s = optimal_lambda)[1:39,]))
df_testall_1$variabl <- rownames(df_testall_1)
colnames(df_testall_1) <- c("coeff1", "Predictor")
predictedall_1 <- predict(cv_ridgeall_1, s = cv_ridgeall_1$lambda.1se, newx = x_test)
mseall_1 <- mean((y_test - predictedall_1)^2)
df_testall_1 <- df_testall_1 %>% 
  select(Predictor, coeff1)
head(df_testall_1)


# all (using imputed 2 dataset)
head(test2)
str(test2)
testall_2 <- test2

x <- testall_2 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- testall_2$polviews

set.seed(124)
train_rows <- sample(1:nrow(testall_2), 0.8*nrow(testall_2))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridgeall_2 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_testall_2 <- data.frame(sort(predict(cv_ridgeall_2, type = "coefficients", s = optimal_lambda)[1:39,]))
df_testall_2$variabl <- rownames(df_testall_2)
colnames(df_testall_2) <- c("coeff2", "Predictor")
predictedall_2 <- predict(cv_ridgeall_2, s = cv_ridgeall_2$lambda.1se, newx = x_test)
mseall_2 <- mean((y_test - predictedall_2)^2)
df_testall_2 <- df_testall_2 %>% 
  select(Predictor, coeff2)
head(df_testall_2)

# all (using imputed 3 dataset)
head(test3)
str(test3)
testall_3 <- test3

x <- testall_3 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- testall_3$polviews

set.seed(125)
train_rows <- sample(1:nrow(testall_3), 0.8*nrow(testall_3))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridgeall_3 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_testall_3 <- data.frame(sort(predict(cv_ridgeall_3, type = "coefficients", s = optimal_lambda)[1:39,]))
df_testall_3$variabl <- rownames(df_testall_3)
colnames(df_testall_3) <- c("coeff3", "Predictor")
predictedall_3 <- predict(cv_ridgeall_3, s = cv_ridgeall_3$lambda.1se, newx = x_test)
mseall_3 <- mean((y_test - predictedall_3)^2)
df_testall_3 <- df_testall_3 %>% 
  select(Predictor, coeff3)
head(df_testall_3)


# all (using imputed 4 dataset)
head(test4)
str(test4)
testall_4 <- test4

x <- testall_4 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- testall_4$polviews

set.seed(126)
train_rows <- sample(1:nrow(testall_4), 0.8*nrow(testall_4))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridgeall_4 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_testall_4 <- data.frame(sort(predict(cv_ridgeall_4, type = "coefficients", s = optimal_lambda)[1:39,]))
df_testall_4$variabl <- rownames(df_testall_4)
colnames(df_testall_4) <- c("coeff4", "Predictor")
predictedall_4 <- predict(cv_ridgeall_4, s = cv_ridgeall_4$lambda.1se, newx = x_test)
mseall_4 <- mean((y_test - predictedall_4)^2)
df_testall_4 <- df_testall_4 %>% 
  select(Predictor, coeff4)
head(df_testall_4)


# all (using imputed 5 dataset)
head(test5)
str(test5)
testall_5 <- test5

x <- testall_5 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- testall_5$polviews

set.seed(127)
train_rows <- sample(1:nrow(testall_5), 0.8*nrow(testall_5))
x_train <- x[train_rows,]
x_test <- x[-train_rows,]
y_train <- y[train_rows]
y_test <- y[-train_rows]

cv_ridgeall_5 <- cv.glmnet(x_train, y_train, alpha = 0, type.measure = "mse", family = "gaussian")
df_testall_5 <- data.frame(sort(predict(cv_ridgeall_5, type = "coefficients", s = optimal_lambda)[1:39,]))
df_testall_5$variabl <- rownames(df_testall_5)
colnames(df_testall_5) <- c("coeff5", "Predictor")
predictedall_5 <- predict(cv_ridgeall_5, s = cv_ridgeall_5$lambda.1se, newx = x_test)
mseall_5 <- mean((y_test - predictedall_5)^2)
df_testall_5 <- df_testall_5 %>% 
  select(Predictor, coeff5)
head(df_testall_5)

df_test_all <- merge(df_testall_1, df_testall_2, by = "Predictor", all.x = TRUE)
df_test_all <- merge(df_test_all, df_testall_3, by = "Predictor", all.x = TRUE)
df_test_all <- merge(df_test_all, df_testall_4, by = "Predictor", all.x = TRUE)
df_test_all <- merge(df_test_all, df_testall_5, by = "Predictor", all.x = TRUE)
head(df_test_all)

df_test_all$max <- apply(df_test_all[,2:6], 1, max)
df_test_all$min <- apply(df_test_all[,2:6], 1, min)

df_test_all <- df_test_all[-1,]

df_test_all <- df_test_all %>% 
  arrange(desc(max))

head(df_test_all)
cle_all <- df_test_all %>% 
  select(Predictor, max, min)
cle_all <- cle_all[1:10,]
head(cle_all)
cle_all <- gather(cle_all, category, value, 2:3)

plotall <- cle_all %>% 
  ggplot(aes(value, fct_reorder2(Predictor, category == "max", value, .desc = FALSE), color = category)) +
  geom_point(size = 2) +
  geom_line(aes(group = Predictor), color = "darkgrey") +
  scale_color_manual(values = c("#f4352c", "#042c44"), labels = c("Max", "Min")) +
  ylab("") + xlab("") +
  ggtitle("Overall") +
  xlim(0, 0.53) +
  theme_dotplot +
  theme(legend.position = c(0.7, 0.25), legend.key.size = unit(1, 'lines'), legend.title = element_blank())

# Combine plots
gridExtra::grid.arrange(plotall, plot70s, plot80s, plot90s, plot00s, plot10s, nrow = 2, ncol = 3)


#####
# Descriptive statistics

# Table A5.
head(df_testall_1)
df_testall_1 <- df_testall_1 %>% 
  mutate(abs_coef = abs(coeff1)) %>% 
  arrange(desc(abs_coef))

df_testall_2 <- df_testall_2 %>% 
  mutate(abs_coef = abs(coeff2)) %>% 
  arrange(desc(abs_coef))

df_testall_3 <- df_testall_3 %>% 
  mutate(abs_coef = abs(coeff3)) %>% 
  arrange(desc(abs_coef))

df_testall_4 <- df_testall_4 %>% 
  mutate(abs_coef = abs(coeff4)) %>% 
  arrange(desc(abs_coef))

df_testall_5 <- df_testall_5 %>% 
  mutate(abs_coef = abs(coeff5)) %>% 
  arrange(desc(abs_coef))

df_testall_combined <- cbind(df_testall_1, df_testall_2, df_testall_3, df_testall_4, df_testall_5)
write_csv(df_testall_combined, "combined_desc.csv")

mse1
mse2
mse3
mse4
mse5

##############################################
## Multilevel analysis with imputed dataset ##
##############################################


# Cohort 3
df_coh3 <- gss2 %>% 
  mutate_at(vars(gen_coh3, busing, racopen, racmar, postlife, reliten, fund, grass, gunlaw, cappun, libhomo, ballot, 
                 suicide1, letdie1, xmovie, pornlaw, abany, absingle, abhlth, fepres, fework, fehome, spkhomo, libcom, spkcom, 
                 spkrac, libath, spkath, year), funs(as.factor(.)))

df_coh3 <- df_coh3 %>% 
  select(gen_coh3, polviews, year, age, homosex, eqwlth, premarsx, natheal, educ, lnIncome18, lnRincome18, finrela, satfin, satjob, 
         childs, health, happy)

str(df_coh3)
unique(df_coh3$happy)
df_coh3 <- df_coh3 %>% 
  mutate(happy = ifelse(happy == 3, 1,
                        ifelse(happy == 2, 2,
                               ifelse(happy == 1, 3, NA))))

df_coh3 <- df_coh3 %>% 
  dplyr::filter(!is.na(gen_coh3), !is.na(polviews)) %>% 
  dplyr::group_by(gen_coh3, year) %>% 
  dplyr::summarise(N = dplyr::n(),
                   mean_polviews = mean(polviews, na.rm = TRUE),
                   mean_homosex = mean(homosex, na.rm = TRUE),
                   mean_eqwlth = mean(eqwlth, na.rm = TRUE),
                   mean_premarsx = mean(premarsx, na.rm = TRUE),
                   mean_educ = mean(educ, na.rm = TRUE),
                   mean_satfin = mean(satfin, na.rm = TRUE),
                   mean_satjob = mean(satjob, na.rm = TRUE),
                   mean_finrela = mean(finrela, na.rm = TRUE),
                   mean_rinc = mean(lnRincome18, na.rm = TRUE),
                   mean_inc = mean(lnIncome18, na.rm = TRUE),
                   mean_childs = mean(childs, na.rm = TRUE),
                   mean_health = mean(health, na.rm = TRUE),
                   mean_happy = mean(happy, na.rm = TRUE),
                   mean_age = mean(age, na.rm = TRUE)) %>% 
  ungroup()


#normFunc <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}

df_coh3 <- df_coh3 %>% 
dplyr::select(mean_polviews, mean_happy, mean_rinc, mean_age, mean_educ, mean_satfin, mean_finrela, mean_satjob, gen_coh3)

df_coh3[,1:8] <- apply(test_coh3[,1:8], 2, normFunc)

## Multilevel Mixed-effect model
fit_coh3 <- lmer(mean_polviews ~ I(mean_happy**2) + mean_happy + mean_rinc + mean_finrela + mean_age + mean_educ + (1|gen_coh3), data = df_coh3)
summary(fit_coh3)

test_coh3[test_coh3$mean_happy > -5,] %>% 
  ggplot(aes(mean_happy, mean_polviews)) + geom_point() + geom_smooth(method = 'loess')


# Cohort 5
df_coh5 <- gss2 %>% 
  mutate_at(vars(gen_coh5, busing, racopen, racmar, postlife, reliten, fund, grass, gunlaw, cappun, libhomo, ballot, 
                 suicide1, letdie1, xmovie, pornlaw, abany, absingle, abhlth, fepres, fework, fehome, spkhomo, libcom, spkcom, 
                 spkrac, libath, spkath, year), funs(as.factor(.)))

df_coh5 <- df_coh5 %>% 
  select(gen_coh5, polviews, year, age, homosex, eqwlth, premarsx, natheal, educ, lnIncome18, lnRincome18, finrela, satfin, satjob, 
         childs, health, happy)

str(df_coh5)
unique(df_coh5$happy)
df_coh5 <- df_coh5 %>% 
  mutate(happy = ifelse(happy == 3, 1,
                        ifelse(happy == 2, 2,
                               ifelse(happy == 1, 3, NA))))

df_coh5 <- df_coh5 %>% 
  dplyr::filter(!is.na(gen_coh5), !is.na(polviews)) %>% 
  dplyr::group_by(gen_coh5, year) %>% 
  dplyr::summarise(N = dplyr::n(),
                   mean_polviews = mean(polviews, na.rm = TRUE),
                   mean_homosex = mean(homosex, na.rm = TRUE),
                   mean_eqwlth = mean(eqwlth, na.rm = TRUE),
                   mean_premarsx = mean(premarsx, na.rm = TRUE),
                   mean_educ = mean(educ, na.rm = TRUE),
                   mean_satfin = mean(satfin, na.rm = TRUE),
                   mean_satjob = mean(satjob, na.rm = TRUE),
                   mean_finrela = mean(finrela, na.rm = TRUE),
                   mean_rinc = mean(lnRincome18, na.rm = TRUE),
                   mean_inc = mean(lnIncome18, na.rm = TRUE),
                   mean_childs = mean(childs, na.rm = TRUE),
                   mean_health = mean(health, na.rm = TRUE),
                   mean_happy = mean(happy, na.rm = TRUE),
                   mean_age = mean(age, na.rm = TRUE)) %>% 
  ungroup()


#normFunc <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}

df_coh5 <- df_coh5 %>% 
  dplyr::select(mean_polviews, mean_happy, mean_rinc, mean_age, mean_educ, mean_satfin, mean_finrela, mean_satjob, gen_coh5)

df_coh5[,1:8] <- apply(df_coh5[,1:8], 2, normFunc)

## Multilevel Mixed-effect model
fit_coh5 <- lmer(mean_polviews ~ I(mean_happy**2) + mean_happy + mean_rinc + mean_finrela + mean_age + mean_educ + (1|gen_coh5), data = df_coh5)
summary(fit_coh5)

test_coh5 %>% 
  ggplot(aes(mean_happy, mean_polviews)) + geom_point() + geom_smooth(method = 'loess')

test_coh5 <- df_coh5 %>% 
  dplyr::mutate_at(1:8,funs(ifelse(.>=-3 & .<=3,.,NA)))

fit_coh5 <- lmer(mean_polviews ~ I(mean_happy**2) + mean_happy + mean_rinc + mean_finrela + mean_age + mean_educ + (1|gen_coh5), data = test_coh5)
summary(fit_coh5)


# Cohort 10
df_coh10 <- gss2 %>% 
  mutate_at(vars(gen_coh10, busing, racopen, racmar, postlife, reliten, fund, grass, gunlaw, cappun, libhomo, ballot, 
                 suicide1, letdie1, xmovie, pornlaw, abany, absingle, abhlth, fepres, fework, fehome, spkhomo, libcom, spkcom, 
                 spkrac, libath, spkath, year), funs(as.factor(.)))

df_coh10 <- df_coh10 %>% 
  select(gen_coh10, polviews, year, age, homosex, eqwlth, premarsx, natheal, educ, lnIncome18, lnRincome18, finrela, satfin, satjob, 
         childs, health, happy)

str(df_coh10)
unique(df_coh10$happy)
df_coh10 <- df_coh10 %>% 
  mutate(happy = ifelse(happy == 3, 1,
                        ifelse(happy == 2, 2,
                               ifelse(happy == 1, 3, NA))))

df_coh10 <- df_coh10 %>% 
  dplyr::filter(!is.na(gen_coh10), !is.na(polviews)) %>% 
  dplyr::group_by(gen_coh10, year) %>% 
  dplyr::summarise(N = dplyr::n(),
                   mean_polviews = mean(polviews, na.rm = TRUE),
                   mean_homosex = mean(homosex, na.rm = TRUE),
                   mean_eqwlth = mean(eqwlth, na.rm = TRUE),
                   mean_premarsx = mean(premarsx, na.rm = TRUE),
                   mean_educ = mean(educ, na.rm = TRUE),
                   mean_satfin = mean(satfin, na.rm = TRUE),
                   mean_satjob = mean(satjob, na.rm = TRUE),
                   mean_finrela = mean(finrela, na.rm = TRUE),
                   mean_rinc = mean(lnRincome18, na.rm = TRUE),
                   mean_inc = mean(lnIncome18, na.rm = TRUE),
                   mean_childs = mean(childs, na.rm = TRUE),
                   mean_health = mean(health, na.rm = TRUE),
                   mean_happy = mean(happy, na.rm = TRUE),
                   mean_age = mean(age, na.rm = TRUE)) %>% 
  ungroup()


#normFunc <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}

df_coh10 <- df_coh10 %>% 
  dplyr::select(mean_polviews, mean_happy, mean_rinc, mean_age, mean_educ, mean_satfin, mean_finrela, mean_satjob, gen_coh10)

df_coh10[,1:8] <- apply(df_coh10[,1:8], 2, normFunc)

## Multilevel Mixed-effect model
fit_coh10 <- lmer(mean_polviews ~ I(mean_happy**2) + mean_happy + mean_rinc + mean_finrela + mean_age + mean_educ + (1|gen_coh10), data = df_coh10)
summary(fit_coh10)

df_coh10 %>% 
  ggplot(aes(mean_happy, mean_polviews)) + geom_point() + geom_smooth(method = 'loess')


# PEW Generational Cohort
df_coh <- gss2 %>% 
  mutate_at(vars(gen_coh, busing, racopen, racmar, postlife, reliten, fund, grass, gunlaw, cappun, libhomo, ballot, 
                 suicide1, letdie1, xmovie, pornlaw, abany, absingle, abhlth, fepres, fework, fehome, spkhomo, libcom, spkcom, 
                 spkrac, libath, spkath, year), funs(as.factor(.)))

df_coh <- df_coh %>% 
  select(gen_coh, polviews, year, age, homosex, eqwlth, premarsx, natheal, educ, lnIncome18, lnRincome18, finrela, satfin, satjob, 
         childs, health, happy)

str(df_coh)
unique(df_coh$happy)
df_coh <- df_coh %>% 
  mutate(happy = ifelse(happy == 3, 1,
                        ifelse(happy == 2, 2,
                               ifelse(happy == 1, 3, NA))))

df_coh <- df_coh %>% 
  dplyr::filter(!is.na(gen_coh), !is.na(polviews)) %>% 
  dplyr::group_by(gen_coh, year) %>% 
  dplyr::summarise(N = dplyr::n(),
                   mean_polviews = mean(polviews, na.rm = TRUE),
                   mean_homosex = mean(homosex, na.rm = TRUE),
                   mean_eqwlth = mean(eqwlth, na.rm = TRUE),
                   mean_premarsx = mean(premarsx, na.rm = TRUE),
                   mean_educ = mean(educ, na.rm = TRUE),
                   mean_satfin = mean(satfin, na.rm = TRUE),
                   mean_satjob = mean(satjob, na.rm = TRUE),
                   mean_finrela = mean(finrela, na.rm = TRUE),
                   mean_rinc = mean(lnRincome18, na.rm = TRUE),
                   mean_inc = mean(lnIncome18, na.rm = TRUE),
                   mean_childs = mean(childs, na.rm = TRUE),
                   mean_health = mean(health, na.rm = TRUE),
                   mean_happy = mean(happy, na.rm = TRUE),
                   mean_age = mean(age, na.rm = TRUE)) %>% 
  ungroup()


#normFunc <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}

df_coh <- df_coh %>% 
  dplyr::select(mean_polviews, mean_happy, mean_rinc, mean_age, mean_educ, mean_satfin, mean_finrela, mean_satjob, gen_coh)

df_coh[,1:8] <- apply(df_coh[,1:8], 2, normFunc)

## Multilevel Mixed-effect model
fit_coh <- lmer(mean_polviews ~ I(mean_happy**2) + mean_happy + mean_rinc + mean_finrela + mean_age + mean_educ + (1|gen_coh), data = df_coh)
summary(fit_coh)

df_coh %>% 
  ggplot(aes(mean_happy, mean_polviews)) + geom_point() + geom_smooth(method = 'loess')

class(fit_coh3) <- "lmerMod"
class(fit_coh5) <- "lmerMod"
class(fit_coh10) <- "lmerMod"
class(fit_coh) <- "lmerMod"
library(stargazer)
stargazer
stargazer(fit_coh3, fit_coh5, fit_coh10, fit_coh, type = "html", out = "table_new4.doc")
