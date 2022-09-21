# questions:
## why do participants have multiple IMD scores?
## I am using mother's PHQ8 score. If dad has a NSSEC, I take max of mum & dad's

library(data.table) # for reading and manipulating data
library(viridisLite) # colour palette
library(MASS) # for negative binomial distribution
library(lme4) # fitting mixed models using ML / REML
library(devEMF) # enhanced metafile graphics device
library(performance) # for icc

cols <- turbo(5)

meanci <- function (x) { # mean and confidence interval using `base::t.test`
  tt <- t.test(x)
  `names<-`(c(tt$estimate, tt$conf.int[1:2]), c('mn', 'lower', 'upper'))
}

setwd("H:/bib_school_mh")

# ---------
# read data
# ---------

child <- fread('data/BiB_GrowingUp_Child_Vars.csv')
imd <- fread('data/BiB_Geographical_House_Vars.csv')
ethnicity <- fread('data/BiB_CohortInfo_Ethnicity.csv')
gender <- fread('data/BiB_CohortInfo_Vars.csv')
adult <- fread('data/BiB_GrowingUp_Adult_Vars.csv')
hh <- fread('data/BiB_GrowingUp_HouseholdSize.csv')
link <- fread('data/BiB_CohortInfo_IDLinkage.csv')

# -------------
# join datasets
# -------------

# ethnicity

child[, guadtEthnicGroup := NULL]
child <- ethnicity[, c('BiBPersonID', 'ethnicity16', 'ethnicity5')][child, on = 'BiBPersonID']
child[, ethnicity16 := factor(ethnicity16, 1:16, c('White British', 'Irish', 'Any other white', 'White and Black Carribean', 'White and Black African', 'White and Asian', 'Any other mixed', 'Indian', 'Pakistani', 'Bangladeshi', 'Chinese', 'Any other Asian', 'African', 'Carribean', 'Other Black, African, or Carribean', 'Other'))]
child[, ethnicity5 := factor(ethnicity5, 1:5, c('White', 'Mixed', 'Asian', 'Black', 'Other'))]
child[, eth := as.character(ethnicity5)]
child$eth[child$ethnicity16 == 'Pakistani'] <- 'Pakistani'
xe <- table(child$eth)
child[, eth := factor(eth, names(xe)[order(-xe)])]

# choose random IMD per individual

imd[, rnd := rnorm(.N, 100, 100)]
imd <- imd[order(BiBPersonID, rnd)]
imd[, i := rowid(BiBPersonID)]
imd <- imd[i == 1, c('BiBPersonID', 'IMD_2019_decile')]
child <- imd[child, on = 'BiBPersonID']
child[, imd5 := findInterval(IMD_2019_decile, c(1, 3, 5, 7, 9))]
child$imd5[is.na(child$imd5)] <- 'missing'
child[, imd5 := factor(imd5, c(1:5, 'missing'))]
child <- gender[child, on = 'BiBPersonID']

# max NS_SEC & PHQ8 for parents

adult[, NSSEC := pmin(guadtNS_SEC, guadtPartNS_SEC, na.rm = T)]
adult$NSSEC[is.na(adult$NSSEC)] <- 6L
adult[, NSSEC := factor(NSSEC, 1:6, c('Managerial, administrative and professional occupations', 'Intermediate occupations', 'Small employers and own account workers', 'Lower supervisory and technical occupations', 'Semi-routine and routine occupations', 'Missing'))]
adult <- unique(link[, .(BiBPersonID = BiBMotherID, BiBChildID = BiBChildID)])[adult, on = 'BiBPersonID']
adult$BiBPersonID <- NULL
setnames(adult, c('BiBChildID', 'guadtPHQ8_Score'), c('BiBPersonID', 'phq8'))
child <- adult[, c('BiBPersonID', 'NSSEC', 'phq8')][child, on = 'BiBPersonID']
child[, phq8_group := findInterval(phq8, c(0, 1, 3, 5, 10))]
child$phq8_group[is.na(child$phq8_group)] <- 'missing'
child[, phq8_group := factor(phq8_group, c(1:5, 'missing'), c('0', '1-2', '3-4', '5-9', '10+', 'missing'))]

# household size

child <- hh[child, on = 'BiBPersonID']
hhsizes <- c(0, 4, 6, 8)
child[, hhs := findInterval(guadtNoPeopleHouse, hhsizes)]
child[, hhs := factor(hhs, 1:4, c('1-3', '4-5', '6-7', '8+'))]

# format variables

child[, ageYears := AgeMonths / 12]
child[, Date := as.Date(Date, origin = '1970-01-01')]
child[, mthComp := factor(month(Date), 1:12, month.abb)]
child[, season := factor(as.character(mthComp), month.abb, c('Winter', 'Winter', 'Spring', 'Spring', 'Spring', 'Summer', 'Summer', 'Summer', 'Autumn', 'Autumn', 'Autumn', 'Winter'))]
child[, ageInt := factor(AgeYears, c(7:12, 6), c(7:11, c('6 or 12', '6 or 12')))]

# --------------------
# calculate SDQ scores
# --------------------

# methodology: https://www.sdqinfo.org/py/sdqinfo/c0.py

# emotional
# ---------

sdq_emotional <- c('guadtSDQ1_3', 'guadtSDQ1_8', 'guadtSDQ1_13', 'guadtSDQ1_16', 'guadtSDQ1_24')
sdq_emotional_inverse <- c(F, F, F, F, F)
sdq_emotional_score <- child[, sdq_emotional, with = F] - 1
if (any(sdq_emotional_inverse)) {
  sdq_emotional_score[, (sdq_emotional[sdq_emotional_inverse]) := lapply(.SD, function (x) 2 - x), .SDcols = sdq_emotional[sdq_emotional_inverse]]
}
child[, sdq_emotional := rowSums(sdq_emotional_score)]

# conduct
# -------

sdq_conduct <- c('guadtSDQ1_5', 'guadtSDQ1_7', 'guadtSDQ1_12', 'guadtSDQ1_18', 'guadtSDQ1_22')
sdq_conduct_inverse <- c(F, T, F, F, F)
sdq_conduct_score <- child[, sdq_conduct, with = F] - 1
if (any(sdq_conduct_inverse)) {
  sdq_conduct_score[, (sdq_conduct[sdq_conduct_inverse]) := lapply(.SD, function (x) 2 - x), .SDcols = sdq_conduct[sdq_conduct_inverse]]
}
child[, sdq_conduct := rowSums(sdq_conduct_score)]

# hyperactivity
# -------------

sdq_hyper <- c('guadtSDQ1_2', 'guadtSDQ1_10', 'guadtSDQ1_15', 'guadtSDQ1_21', 'guadtSDQ1_25')
sdq_hyper_inverse <- c(F, F, F, T, T)
sdq_hyper_score <- child[, sdq_hyper, with = F] - 1
if (any(sdq_hyper_inverse)) {
  sdq_hyper_score[, (sdq_hyper[sdq_hyper_inverse]) := lapply(.SD, function (x) 2 - x), .SDcols = sdq_hyper[sdq_hyper_inverse]]
}
child[, sdq_hyper := rowSums(sdq_hyper_score)]

# peer
# ----

sdq_peer <- c('guadtSDQ1_6', 'guadtSDQ1_11', 'guadtSDQ1_14', 'guadtSDQ1_19', 'guadtSDQ1_23')
sdq_peer_inverse <- c(F, T, T, F, F)
sdq_peer_score <- child[, sdq_peer, with = F] - 1
if (any(sdq_peer_inverse)) {
  sdq_peer_score[, (sdq_peer[sdq_peer_inverse]) := lapply(.SD, function (x) 2 - x), .SDcols = sdq_peer[sdq_peer_inverse]]
}
child[, sdq_peer := rowSums(sdq_peer_score)]

# total
# -----

child[, sdq_total := sdq_emotional + sdq_conduct + sdq_hyper + sdq_peer]
child[, sdq_group := findInterval(sdq_total, c(0, 5, 10, 20))]
child$sdq_group[is.na(child$sdq_group)] <- 'missing'
child[, sdq_group := factor(sdq_group, c(1:4, 'missing'), c('0-4', '5-9', '10-19', '20+', 'missing'))]

# -------
# table 1
# -------

med_iqr <- function (var, digs = 2) {
  m <- quantile(child[, get(var)], probs = c(0.5, 0.25, 0.75), na.rm = T)
  m <- format(round(m, digs), digits = digs, nsmall = digs)
  m <- paste0(m[1], ' [', m[2], '-', m[3], ']')
  m <- gsub(' ', '', m)
  m <- gsub('\\[', ' [', m)
  data.frame(var = var, lev = 'median [IQR]', n = m)
}

pc <- function (var, digs = 2) {
  x <- table(child[, get(var)])
  pc <- x / sum(x) * 100
  pc <- format(round(pc, digs), digits = digs, nsmall = digs)
  x <- formatC(x, big.mark = ',')
  n <- paste0(x, ' (', pc, ')')
  n <- gsub(' ', '', n)
  n <- gsub('\\(', ' (', n)
  data.frame(var = c(var, rep('', length(x)-1)), lev = names(x), n = n)
}

child[, total := 1]

table1 <- rbind(
  pc('total'),
  pc('ageInt'),
  med_iqr('ageYears'),
  pc('Gender'),
  pc('eth'),
  pc('imd5'),
  pc('hhs'),
  pc('season'),
  med_iqr('sdq_total'),
  pc('sdq_group'),
  pc('NSSEC'),
  med_iqr('phq8'),
  pc('phq8_group')
)

fwrite(table1, 'table1_21September2022.csv')

# ---------------
# model SDQ score 
# ---------------

# histogram

sdq_bins <- data.table(lower = 0:39, upper = 1:40)
sdq_bins[, n := sapply(upper, function (x) sum(child$sdq_total == x, na.rm = T))]

emf('SDQ_histogram.emf', height = 5, width = 5, units = 'in', family = 'Bitter Light')

plot(1, type = 'n', xlim = c(0, 40), ylim = c(0, 500), axes = F, xlab = NA, ylab = NA)
with(sdq_bins, rect(lower, 0, upper, n, col = cols[2]))
axis(1, 0:8 * 5, pos = 0, labels = T)
axis(2, 0:5 * 100, pos = 0, las = 2)
rect(0, 0, 40, 500)
title(ylab = 'Number of participants')
title(xlab = 'Total SDQ score', line = 3)

dev.off()

# -------------
# simulate data
# -------------

# modelled scores - negative binomial distribution with dispersion parameter of 3 looks good

child[, dummy := 1]
summary(glm.nb(sdq_total ~ dummy, data = child))
mn <- mean(child$sdq_total, na.rm = T)

# test model
# ----------

m <- lm(sdq_total ~ poly(ageYears, 2) + Gender + eth + imd5 + hhs + season + NSSEC + phq8, data = child)

# simulate school data
# --------------------

number_schools <- 141
mean_school_size <- nrow(child) / number_schools
effect_sd <- 2 # standard deviation of school effects
mean_difficulties <- mean(child$sdq_total, na.rm = T) # 'normal' is considered 0-13 https://acamh.onlinelibrary.wiley.com/doi/epdf/10.1111/j.1469-7610.1997.tb01545.x
sdq_disp <- 3 # dispersion parameter for SDQ

sim <- function (effect_sd = 2, number_schools = 141, mean_school_size = 36.46099, mean_difficulties = 10, sdq_disp = 2) {
  # generate school-level data
  school_sizes <- rnbinom(number_schools, mu = mean_school_size, size = 1000)
  schools <- data.frame(school = seq_len(number_schools), pupils = school_sizes)
  schools$effect <- rnorm(number_schools, mean = 0, sd = effect_sd)
  schools$meanSDQ <- mean_difficulties + schools$effect
  # generate pupil-level data
  sdqs <- mapply(rnbinom, n = schools$pupils, mu = schools$meanSDQ, size = sdq_disp)
  data.frame(school = rep(schools$school, schools$pupils), sdq = unlist(sdqs))
}

pwrs <- seq(0.2, 1, 0.1)
set.seed(12)
b <- lapply(pwrs, function (y, nSim = 1000L) {
  sapply(seq_len(nSim), function (x) {
    print(paste0(y, ': ', x))
    dat <- sim(y)
    mR <- lmer(sdq ~ (1|school), data = dat, REML = F)
    mF <- lm(sdq ~ 1, data = dat)
    a <- anova(mR, mF)
    c(as.data.frame(VarCorr(mR))[1,'sdcor'], a$`Pr(>Chisq)`[2])
  })
})

# where model is singular, assume results are not significant

p5 <- sapply(b, function (x) sum(x[2,] < 0.05) / ncol(x))
mp <- glm(p5 ~ pwrs, family = 'binomial') # error message about non-integer successes expected
pwrs2 <- seq(0.2, 1, 0.01)
pred <- predict(mp, newdata = data.frame(pwrs = pwrs2), type = 'response')
p80 <- pwrs2[which.min(abs(pred - 0.8))]

emf('power.emf', height = 4, width = 4, units = 'in', family = 'Bitter Light')

par(mar = c(4, 4, 1, 1))
plot(1, type = 'n', xlim = c(0.2, 1), ylim = c(0, 1), axes = F, xlab = NA, ylab = NA)
points(pwrs, p5, pch = 1)
lines(pwrs2, pred)
segments(0.2, 0.8, x1 = p80, lty = 3)
segments(p80, 0, y1 = 0.8, lty = 3)
axis(1, pwrs, pos = 0)
axis(2, 0:5/5, paste0(0:5 * 20, '%'), pos = 0.2, las = 2)
rect(0.2, 0, 1, 1)
title(xlab = 'Effect size (sd in school-level effects)', line = 2.5)
title(ylab = 'Power', line = 3)

dev.off()

#p80 = 0.83

# --------
# examples
# --------

# bootstrap function
# ------------------

B <- function (nB = 1000L, dat) {
  BInd <- sample(nrow(dat), nB * nrow(dat), replace = T)
  Bs <- dat[BInd,]
  Bs <- split(Bs, f = rep(seq_len(nB), each = nrow(dat)))
  mBs <- sapply(seq_along(Bs), function (x) {
    print(x)
    m <- lmer(sdq ~ (1|school), data = Bs[[x]], REML = F)
    ranef(m)$school[,1]
  })
  Best <- t(apply(mBs, 1, quantile, probs = c(0.025, 0.5, 0.975)))
  Best <- as.data.frame.matrix(Best)
  names(Best) <- c('Blwr', 'Best', 'Bupr')
  Best$cl <- 'black'
  Best$cl[Best$Bupr < 0] <- 'red'
  Best$cl[Best$Blwr > 0] <- 'blue'
  Best$school <- seq_len(nrow(Best))
  Best <- Best[order(Best$Best),]
  Best$x <- seq_len(nrow(Best))
  Best
}

# effect = 0.2 (small variation)
# ..............................

set.seed(12)
egSmall <- sim(effect_sd = 0.2)

# observed mean SDQs
school_means <- sapply(split(egSmall, f = egSmall$school), function (x) meanci(x$sdq))
school_means <- cbind(school = seq_len(ncol(school_means)), as.data.frame.matrix(t(school_means)))

# bootstrap REs
REsmall <- B(dat = egSmall)
REsmall <- merge(REsmall, school_means)

# effect = 0.83 (power of 80%)
# ----------------------------

egM <- sim(effect_sd = p80)

# observed mean SDQs
school_means <- sapply(split(egM, f = egM$school), function (x) meanci(x$sdq))
school_means <- cbind(school = seq_len(ncol(school_means)), as.data.frame.matrix(t(school_means)))

# bootstrap REs
REm <- B(dat = egM)
REm <- merge(REm, school_means)

# effect = 2 (big difference)
# ---------------------------

eg2 <- sim(effect_sd = 2)

# observed mean SDQs
school_means <- sapply(split(eg2, f = eg2$school), function (x) meanci(x$sdq))
school_means <- cbind(school = seq_len(ncol(school_means)), as.data.frame.matrix(t(school_means)))

# bootstrap REs
RE2 <- B(dat = eg2)
RE2 <- merge(RE2, school_means)

# plots
# .....

emf('example_caterpillar.emf', height = 5, width = 10, units = 'in', family = 'Bitter Light')

par(mfrow = c(1, 3), mar = c(0, 0, 0, 0), oma = c(4, 4, 3, 0), xpd = NA)

plot(1, type = 'n', xlim = c(0, 141), ylim = c(-7.5, 7.5), axes = F, xlab = NA, ylab = 'Mean SDQ by school')
with(REsmall, {
  points(x, Best, pch = 19, col = cl, cex = 0.6)
  arrows(x, Blwr, x, Bupr, code = 3, angle = 90, length = 0.015, col = cl)
  points(x, mn - 10, pch = 3, cex = 0.7, col = 'grey70')
  points(x, lower - 10, pch = 4, cex = 0.5, col = 'grey70')
  points(x, upper - 10, pch = 4, cex = 0.5, col = 'grey70')
})
par(xpd = F); abline(h = 0); par(xpd = NA)
box()
axis(2, las = 2)
title(main = 'sd = 0.2', line = 1)

plot(1, type = 'n', xlim = c(0, 141), ylim = c(-7.5, 7.5), axes = F, xlab = NA, ylab = NA)
with(REm, {
  points(x, Best, pch = 19, col = cl, cex = 0.6)
  arrows(x, Blwr, x, Bupr, code = 3, angle = 90, length = 0.015, col = cl)
  points(x, mn - 10, pch = 3, cex = 0.7, col = 'grey70')
  points(x, lower - 10, pch = 4, cex = 0.5, col = 'grey70')
  points(x, upper - 10, pch = 4, cex = 0.5, col = 'grey70')
})
box()
title(main = 'sd = 0.83', line = 1)
par(xpd = F); abline(h = 0); par(xpd = NA)

plot(1, type = 'n', xlim = c(0, 141), ylim = c(-7.5, 7.5), axes = F, xlab = NA, ylab = NA)
with(RE2, {
  points(x, Best, pch = 19, col = cl, cex = 0.6)
  arrows(x, Blwr, x, Bupr, code = 3, angle = 90, length = 0.015, col = cl)
  points(x, mn - 10, pch = 3, cex = 0.7, col = 'grey70')
  points(x, lower - 10, pch = 4, cex = 0.5, col = 'grey70')
  points(x, upper - 10, pch = 4, cex = 0.5, col = 'grey70')
})
par(xpd = F); abline(h = 0); par(xpd = NA)
box()
title(main = 'sd = 2', line = 1)

dev.off()

# ---
# ICC
# ---

m2 <- lmer(sdq ~ (1|school), data = egM, REML = F)
m3 <- lm(sdq ~ 1, data = egM)

performance::icc(m3)

# check that ICC is higher with higher variation in school-level values

performance::icc(lmer(sdq ~ (1|school), data = sim(effect_sd = 0.2)))
performance::icc(lmer(sdq ~ (1|school), data = sim(effect_sd = 1)))
performance::icc(lmer(sdq ~ (1|school), data = sim(effect_sd = 2)))
