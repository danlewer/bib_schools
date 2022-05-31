library(lme4)
library(extrafont)
loadfonts(device = 'win')

meanci <- function (x) { # mean and confidence interval using `base::t.test`
  tt <- t.test(x)
  `names<-`(c(tt$estimate, tt$conf.int[1:2]), c('mn', 'lower', 'upper'))
}

setwd("~/bib")

# number_schools <- 141
# mean_school_size <- 64
# effect_sd <- 2 # standard deviation of school effects
# mean_difficulties <- 10 # 'normal' is considered 0-13 https://acamh.onlinelibrary.wiley.com/doi/epdf/10.1111/j.1469-7610.1997.tb01545.x
# sdq_disp <- 2 # dispersion parameter for SDQ

# :::::
# power
# .....

sim <- function (effect_sd = 2, number_schools = 141, mean_school_size = 64, mean_difficulties = 10, sdq_disp = 2) {
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
mp <- glm(p5 ~ pwrs, family = 'binomial')
pwrs2 <- seq(0.2, 1, 0.01)
pred <- predict(mp, newdata = data.frame(pwrs = pwrs2), type = 'response')
p80 <- pwrs2[which.min(abs(pred - 0.8))]

png('power.png', height = 4, width = 4, units = 'in', res = 300, family = 'Franklin Gothic Book')

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

# ::::::::
# examples
# ........

# bootstrap function
# ..................

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

# effect = 0.6
# ............

set.seed(23)
eg0.6 <- sim(effect_sd = 0.6)

# observed mean SDQs
school_means <- sapply(split(eg0.6, f = eg0.6$school), function (x) meanci(x$sdq))
school_means <- cbind(school = seq_len(ncol(school_means)), as.data.frame.matrix(t(school_means)))

# bootstrap REs
RE0.6 <- B(dat = eg0.6)
RE0.6 <- merge(RE0.6, school_means)

# effect = 2
# ..........

set.seed(414)
eg2 <- sim(effect_sd = 2)

# observed mean SDQs
school_means <- sapply(split(eg2, f = eg2$school), function (x) meanci(x$sdq))
school_means <- cbind(school = seq_len(ncol(school_means)), as.data.frame.matrix(t(school_means)))

# bootstrap REs
RE2 <- B(dat = eg2)
RE2 <- merge(RE2, school_means)

# plots
# .....

png('example_caterpillar.png', height = 5, width = 7, units = 'in', res = 300, family = 'Franklin Gothic Book')

par(mfrow = c(1, 2), mar = c(0, 0, 0, 0), oma = c(4, 4, 3, 0), xpd = NA)

plot(1, type = 'n', xlim = c(0, 141), ylim = c(-7.5, 7.5), axes = F, xlab = NA, ylab = 'School-level effect on SDQ')
with(RE0.6, {
  points(x, Best, pch = 19, col = cl, cex = 0.6)
  arrows(x, Blwr, x, Bupr, code = 3, angle = 90, length = 0.015, col = cl)
  points(x, mn - 10, pch = 3, cex = 0.7, col = 'grey70')
  points(x, lower - 10, pch = 4, cex = 0.5, col = 'grey70')
  points(x, upper - 10, pch = 4, cex = 0.5, col = 'grey70')
})
box()
axis(2, las = 2)
title(xlab = 'School', line = 1)
title(main = 'sd = 0.6', line = 1)
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
title(xlab = 'School', line = 1)

dev.off()
