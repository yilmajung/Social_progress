library(haven)
library(tidyverse)
library(randomForest)
library(cowplot)
library(glmnet)
library(mice)
library(lme4)
library(lmerTest)
library(plm)

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
gss3 <- gss2 %>% 
  mutate_at(vars(busing, racopen, racmar, postlife, reliten, fund, grass, gunlaw, cappun, libhomo, ballot, suicide1, letdie1,
                xmovie, pornlaw, abany, absingle, abhlth, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath,
                spkath, year), funs(as.factor(.)))

gss3 <- gss3 %>% 
  select(reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1, pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath, tax, eqwlth, natfare, natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, polviews, year, lnIncome18, lnRincome18, age, finrela, satfin, satjob, educ, childs, busing, racopen, racmar, postlife)

# Drop unobservable cases for POLVIEWS
gss3 <- gss3 %>% 
  drop_na(polviews)


####################################
# Stage 1: Ridge Regression
# Missing data analysis
md.pattern(gss3)
imp <- mice(gss3, pred = quickpred(gss3, minpuc = 0.25, mincor = 0.25), seed = 123, maxit = 5)
gss_imputed1 <- complete(imp, 1)
gss_imputed2 <- complete(imp, 2)
gss_imputed3 <- complete(imp, 3)
gss_imputed4 <- complete(imp, 4)
gss_imputed5 <- complete(imp, 5)

write.csv(gss_imputed1, "gss_imputed1_new.csv")
write.csv(gss_imputed2, "gss_imputed2_new.csv")
write.csv(gss_imputed3, "gss_imputed3_new.csv")
write.csv(gss_imputed4, "gss_imputed4_new.csv")
write.csv(gss_imputed5, "gss_imputed5_new.csv")

# Min-max normalization
minmax <- function (z) {
  return((z - min(z)) / (max(z) - min(z)))
}

str(gss_imputed1)


# GLMNET: Ridge Regression
# Imputed 1
test1 <- gss_imputed1 %>% 
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

x <- test2 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test2$polviews

lambda_seq <- 10^seq(1, -1, by = -.1)
cv_ridge2 <- cv.glmnet(x, y, alpha = 0, lambda = lambda_seq)
optimal_lambda <- cv_ridge2$lambda.min
optimal_lambda

fit2 <- glmnet(x, y, alpha = 0)
sort(predict(fit2, type = "coefficients", s = optimal_lambda)[1:20,])


# Imputed 3
test3 <- gss_imputed3 %>% 
  mutate_at(vars(courts, commun, homosex, premarsx, chldidel, natfare, natarms, natrace, nateduc, natdrug, natcrime,
                 natcity, natheal, natenvir), funs(minmax))

x <- test3 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test3$polviews

lambda_seq <- 10^seq(1, -1, by = -.1)
cv_ridge3 <- cv.glmnet(x, y, alpha = 0, lambda = lambda_seq)
optimal_lambda <- cv_ridge3$lambda.min
optimal_lambda

fit3 <- glmnet(x, y, alpha = 0)
sort(predict(fit3, type = "coefficients", s = optimal_lambda)[1:20,])


# Imputed 4
test4 <- gss_imputed4 %>% 
  mutate_at(vars(courts, commun, homosex, premarsx, chldidel, natfare, natarms, natrace, nateduc, natdrug, natcrime,
                 natcity, natheal, natenvir), funs(minmax))

x <- test4 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test4$polviews

lambda_seq <- 10^seq(1, -1, by = -.1)
cv_ridge4 <- cv.glmnet(x, y, alpha = 0, lambda = lambda_seq)
optimal_lambda <- cv_ridge4$lambda.min
optimal_lambda

fit4 <- glmnet(x, y, alpha = 0)
sort(predict(fit4, type = "coefficients", s = optimal_lambda)[1:20,])


# Imputed 5
test5 <- gss_imputed5 %>% 
  mutate_at(vars(courts, commun, homosex, premarsx, chldidel, natfare, natarms, natrace, nateduc, natdrug, natcrime,
                 natcity, natheal, natenvir), funs(minmax))

x <- test5 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test5$polviews

lambda_seq <- 10^seq(1, -1, by = -.1)
cv_ridge5 <- cv.glmnet(x, y, alpha = 0, lambda = lambda_seq)
optimal_lambda <- cv_ridge5$lambda.min
optimal_lambda

fit5 <- glmnet(x, y, alpha = 0)
data.frame(predict(fit5, type = "coefficients", s = optimal_lambda)[1:20,])

unique(test1$year)


# Training and Test set approach
# test 1
head(test1)
x <- test1 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
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


# test 2
head(test2)
x <- test2 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test2$polviews

set.seed(123)
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




# test 3
head(test3)
x <- test3 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test3$polviews

set.seed(123)
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


# test 4
head(test4)
x <- test4 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test4$polviews

set.seed(123)
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



# test 5
head(test5)
x <- test5 %>% 
  select(busing, racopen, racmar, postlife, reliten, fund, grass, courts, gunlaw, cappun, libhomo, commun, suicide1, letdie1,
         pornlaw, homosex, premarsx, chldidel, abany, fepres, fework, fehome, spkhomo, libcom, spkcom, spkrac, libath, spkath,
         natfare, natfare, natarms, natrace, nateduc, natdrug, natcrime, natcity, natheal, natenvir, year) %>% 
  data.matrix()
y <- test5$polviews

set.seed(123)
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

head(df_test1)

df_test <- cbind(df_test1, df_test2, df_test3, df_test4, df_test5)
write.csv(df_test, "df_test3.csv")

mse1
mse2
mse3
mse4
mse5


########################################
# Stage 2: Mixed-effect Cohort Analysis

# Cohort 5
df_coh5 <- gss2 %>% 
  mutate_at(vars(gen_coh5, busing, racopen, racmar, postlife, reliten, fund, grass, gunlaw, cappun, libhomo, ballot, 
                 suicide1, letdie1, xmovie, pornlaw, abany, absingle, abhlth, fepres, fework, fehome, spkhomo, libcom, spkcom, 
                 spkrac, libath, spkath, year), funs(as.factor(.)))

df_coh5 <- df_coh5 %>% 
  select(gen_coh5, year, age, homosex, eqwlth, premarsx, natheal, educ, lnIncome18, lnRincome18, finrela, satfin, satjob, 
         childs, health, happy)

str(df_coh5)

df_coh5 <- df_coh5 %>% 
  filter(!is.na(gen_coh5)) %>% 
  group_by(gen_coh5, year) %>% 
  summarise(N = dplyr::n(),
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
test <- df_coh5 %>% 
  drop_na()

# Equal wealth
fit1 <- lmer(mean_eqwlth ~ mean_satfin + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh5), data = test)
summary(fit1)

fit2 <- lmer(mean_eqwlth ~ mean_satjob + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh5), data = df_coh5)
summary(fit2)

fit3 <- lmer(mean_eqwlth ~ mean_happy + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh5), data = df_coh5)
summary(fit3)

fit4 <- lmer(mean_eqwlth ~ mean_health + I(mean_rinc**2) + mean_rinc + mean_finrela + mean_age + mean_educ + (1|gen_coh5), data = df_coh5)
summary(fit4)


# Homosexual
fit1 <- lmer(mean_homosex ~ mean_satfin + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh5), data = df_coh5)
summary(fit1)

fit2 <- lmer(mean_homosex ~ mean_satjob + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh5), data = test)
summary(fit2)

fit3 <- lmer(mean_homosex ~ mean_happy + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh5), data = test)
summary(fit3)

fit4 <- lmer(mean_homosex ~ mean_health + I(mean_rinc**2) + mean_rinc + mean_finrela + mean_age + mean_educ + (1|gen_coh5), data = test)
summary(fit4)


# Cohort 10
df_coh10 <- gss2 %>% 
  mutate_at(vars(gen_coh10, busing, racopen, racmar, postlife, reliten, fund, grass, gunlaw, cappun, libhomo, ballot, 
                 suicide1, letdie1, xmovie, pornlaw, abany, absingle, abhlth, fepres, fework, fehome, spkhomo, libcom, spkcom,
                 spkrac, libath, spkath, year), funs(as.factor(.)))

df_coh10 <- df_coh10 %>% 
  select(gen_coh10, year, age, homosex, eqwlth, premarsx, natheal, educ, lnIncome18, lnRincome18, finrela, satfin, satjob, 
         childs, health, happy)

str(df_coh10)

df_coh10 <- df_coh10 %>% 
  filter(!is.na(gen_coh10)) %>% 
  group_by(gen_coh10, year) %>% 
  summarise(N = dplyr::n(),
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
test <- df_coh10 %>% 
  drop_na()

# Equal wealth
fit1 <- lmer(mean_eqwlth ~ mean_satfin + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh10), data = df_coh10)
summary(fit1)

fit2 <- lmer(mean_eqwlth ~ mean_satjob + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh10), data = df_coh10)
summary(fit2)

fit3 <- lmer(mean_eqwlth ~ mean_happy + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh10), data = df_coh10)
summary(fit3)

fit4 <- lmer(mean_eqwlth ~ mean_health + I(mean_rinc**2) + mean_rinc + mean_finrela + mean_age + mean_educ + (1|gen_coh10), data = df_coh10)
summary(fit4)

fit5 <- lmer(mean_eqwlth ~ mean_satfin + mean_satjob + mean_health + I(mean_rinc**2) + mean_rinc + mean_finrela + mean_age + mean_educ + (1|gen_coh10), data = df_coh10)
summary(fit5)


# Homosexual
fit1 <- lmer(mean_homosex ~ mean_satfin + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh10), data = df_coh10)
summary(fit1)

fit2 <- lmer(mean_homosex ~ mean_satjob + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh10), data = df_coh10)
summary(fit2)

fit3 <- lmer(mean_homosex ~ mean_happy + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh10), data = df_coh10)
summary(fit3)

fit4 <- lmer(mean_homosex ~ mean_health + I(mean_rinc**2) + mean_rinc + mean_finrela + mean_age + mean_educ + (1|gen_coh10), data = df_coh10)
summary(fit4)


# PEW Cohort
df_coh <- gss2 %>% 
  mutate_at(vars(gen_coh, busing, racopen, racmar, postlife, reliten, fund, grass, gunlaw, cappun, libhomo, ballot, 
                 suicide1, letdie1, xmovie, pornlaw, abany, absingle, abhlth, fepres, fework, fehome, spkhomo, libcom, spkcom,
                 spkrac, libath, spkath, year), funs(as.factor(.)))

df_coh <- df_coh %>% 
  select(gen_coh, year, age, homosex, eqwlth, premarsx, natheal, educ, lnIncome18, lnRincome18, finrela, satfin, satjob, 
         childs, health, happy)

str(df_coh)

df_coh <- df_coh %>% 
  filter(!is.na(gen_coh)) %>% 
  group_by(gen_coh, year) %>% 
  summarise(N = dplyr::n(),
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

# Equal wealth
fit1 <- lmer(mean_eqwlth ~ mean_satfin + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh), data = df_coh)
summary(fit1)

fit2 <- lmer(mean_eqwlth ~ mean_satjob + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh), data = df_coh)
summary(fit2)

fit3 <- lmer(mean_eqwlth ~ mean_happy + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh), data = df_coh)
summary(fit3)

fit4 <- lmer(mean_eqwlth ~ mean_health + I(mean_rinc**2) + mean_rinc + mean_finrela + mean_age + mean_educ + (1|gen_coh), data = df_coh)
summary(fit4)

fit5 <- lmer(mean_eqwlth ~ mean_satfin + mean_satjob + mean_health + I(mean_rinc**2) + mean_rinc + mean_finrela + mean_age + mean_educ + (1|gen_coh), data = df_coh)
summary(fit5)


# Homosexual
fit1 <- lmer(mean_homosex ~ mean_satfin + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh), data = df_coh)
summary(fit1)

fit2 <- lmer(mean_homosex ~ mean_satjob + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh), data = df_coh)
summary(fit2)

fit3 <- lmer(mean_homosex ~ mean_happy + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh), data = df_coh)
summary(fit3)

fit4 <- lmer(mean_homosex ~ mean_health + I(mean_rinc**2) + mean_rinc + mean_finrela + mean_age + mean_educ + (1|gen_coh), data = df_coh)
summary(fit4)


# Cohort 3
df_coh3 <- gss2 %>% 
  mutate_at(vars(gen_coh3, busing, racopen, racmar, postlife, reliten, fund, grass, gunlaw, cappun, libhomo, ballot, 
                 suicide1, letdie1, xmovie, pornlaw, abany, absingle, abhlth, fepres, fework, fehome, spkhomo, libcom, spkcom, 
                 spkrac, libath, spkath, year), funs(as.factor(.)))

df_coh3 <- df_coh3 %>% 
  select(gen_coh3, year, age, homosex, eqwlth, premarsx, natheal, educ, lnIncome18, lnRincome18, finrela, satfin, satjob, 
         childs, health, happy)

str(df_coh3)

df_coh3 <- df_coh3 %>% 
  filter(!is.na(gen_coh3)) %>% 
  group_by(gen_coh3, year) %>% 
  summarise(N = dplyr::n(),
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

# Equal wealth
fit1 <- lmer(mean_eqwlth ~ mean_satfin + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh3), data = test)
summary(fit1)

fit2 <- lmer(mean_eqwlth ~ mean_satjob + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh3), data = df_coh3)
summary(fit2)

fit3 <- lmer(mean_eqwlth ~ mean_happy + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh3), data = df_coh3)
summary(fit3)

fit4 <- lmer(mean_eqwlth ~ mean_health + I(mean_rinc**2) + mean_rinc + mean_finrela + mean_age + mean_educ + (1|gen_coh3), data = df_coh3)
summary(fit4)


# Homosexual
fit1 <- lmer(mean_homosex ~ mean_satfin + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh3), data = df_coh3)
summary(fit1)

fit2 <- lmer(mean_homosex ~ mean_satjob + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh3), data = df_coh3)
summary(fit2)

fit3 <- lmer(mean_homosex ~ mean_happy + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh3), data = df_coh3)
summary(fit3)

fit4 <- lmer(mean_homosex ~ mean_health + I(mean_rinc**2) + mean_rinc + mean_finrela + mean_age + mean_educ + (1|gen_coh3), data = df_coh3)
summary(fit4)


################
# Final Round (using HAPPY as a key factor)
fit_coh3 <- lmer(mean_homosex ~ mean_happy + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh3), data = df_coh3)
summary(fit_coh3)

fit_coh5 <- lmer(mean_homosex ~ mean_happy + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh5), data = df_coh5)
summary(fit_coh5)

fit_coh10 <- lmer(mean_homosex ~ mean_happy + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh10), data = df_coh10)
summary(fit_coh10)

fit_coh <- lmer(mean_homosex ~ mean_happy + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh), data = df_coh)
summary(fit_coh)

logLik(fit_coh3)
logLik(fit_coh3)


class(fit_coh3) <- "lmerMod"
class(fit_coh5) <- "lmerMod"
class(fit_coh10) <- "lmerMod"
class(fit_coh) <- "lmerMod"
library(stargazer)
stargazer
stargazer(fit_coh3, fit_coh5, fit_coh10, fit_coh, type = "html", out = "table3_new.doc")

# Plot for result
fit_coh3 <- lmer(mean_homosex ~ mean_happy + I(mean_rinc**2) + mean_rinc + mean_finrela  + mean_age + mean_educ + (1|gen_coh3), data = df_coh3)
summary(fit_coh3)
head(df_coh3)

df_coh3 %>% 
  ggplot(aes(mean_happy, mean_homosex)) + geom_point()

#####################
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
# Summarizing dataset by generation
gss4 <- gss2 %>% 
  select(year, age, homosex, polviews, gen_coh) %>% 
  filter(!is.na(gen_coh), !(is.na(age)), age != 99)

table(gss4$homosex)
library(plyr)
sum_gss <- ddply(gss4, c("gen_coh", "age"), summarise,
                  mean_homo = mean(homosex, na.rm = TRUE),
                  sd = sd(homosex, na.rm = TRUE))
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


sum_gss %>%
  ggplot(aes(age, mean_homo, color = gen_coh)) +
  geom_point(aes(shape = gen_coh)) +
  geom_line(aes(group = gen_coh, linetype = gen_coh)) +
  theme_bw() +
  scale_color_manual(values = Blues2) +
  labs(x = "Age", y = "Attitudes to Homosexual Sex Relations by Generation ") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_shape_manual(values = 0:8) +
  theme(legend.position = c(0.8, 0.83), legend.key.size = unit(1, 'lines'), legend.title = element_blank())


#################
# Descriptive Statistics

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
  dplyr::select(gen_coh3, mean_homosex, mean_happy, mean_rinc, mean_finrela, mean_age, mean_educ) %>% 
  dplyr::group_by(gen_coh3) %>% 
  dplyr::summarise(n(), mean(mean_homosex, na.rm=TRUE), mean(mean_happy, na.rm=TRUE), mean(mean_rinc, na.rm=TRUE), 
                   mean(mean_finrela, na.rm=TRUE), mean(mean_age, na.rm=TRUE), mean(mean_educ, , na.rm=TRUE))

head(dstat_coh3)
colnames(dstat_coh3) <- c("Cohorts", "N", "Homosex", "Satisfaction", "Income", "Sub_income", "Age", "Education")

write.csv(dstat_coh3, "dstat_coh3.csv")

# Cohort 5
head(df_coh5)

dstat_coh5 <- df_coh5 %>% 
  dplyr::select(gen_coh5, mean_homosex, mean_happy, mean_rinc, mean_finrela, mean_age, mean_educ) %>% 
  dplyr::group_by(gen_coh5) %>% 
  dplyr::summarise(n(), mean(mean_homosex, na.rm=TRUE), mean(mean_happy, na.rm=TRUE), mean(mean_rinc, na.rm=TRUE), 
                   mean(mean_finrela, na.rm=TRUE), mean(mean_age, na.rm=TRUE), mean(mean_educ, , na.rm=TRUE))

head(dstat_coh5)
colnames(dstat_coh5) <- c("Cohorts", "N", "Homosex", "Satisfaction", "Income", "Sub_income", "Age", "Education")

write.csv(dstat_coh5, "dstat_coh5.csv")


# Cohort 10
head(df_coh10)

dstat_coh10 <- df_coh10 %>% 
  dplyr::select(gen_coh10, mean_homosex, mean_happy, mean_rinc, mean_finrela, mean_age, mean_educ) %>% 
  dplyr::group_by(gen_coh10) %>% 
  dplyr::summarise(n(), mean(mean_homosex, na.rm=TRUE), mean(mean_happy, na.rm=TRUE), mean(mean_rinc, na.rm=TRUE), 
                   mean(mean_finrela, na.rm=TRUE), mean(mean_age, na.rm=TRUE), mean(mean_educ, , na.rm=TRUE))

head(dstat_coh10)
colnames(dstat_coh10) <- c("Cohorts", "N", "Homosex", "Satisfaction", "Income", "Sub_income", "Age", "Education")

write.csv(dstat_coh10, "dstat_coh10.csv")


# Pew Cohort
head(df_coh)

dstat_coh <- df_coh %>% 
  dplyr::select(gen_coh, mean_homosex, mean_happy, mean_rinc, mean_finrela, mean_age, mean_educ) %>% 
  dplyr::group_by(gen_coh) %>% 
  dplyr::summarise(n(), mean(mean_homosex, na.rm=TRUE), mean(mean_happy, na.rm=TRUE), mean(mean_rinc, na.rm=TRUE), 
                   mean(mean_finrela, na.rm=TRUE), mean(mean_age, na.rm=TRUE), mean(mean_educ, , na.rm=TRUE))

head(dstat_coh)
colnames(dstat_coh) <- c("Cohorts", "N", "Homosex", "Satisfaction", "Income", "Sub_income", "Age", "Education")

write.csv(dstat_coh, "dstat_coh.csv")


