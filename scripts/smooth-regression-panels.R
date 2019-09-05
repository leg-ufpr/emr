#-----------------------------------------------------------------------
# Extensões de modelos de regressão · CE 092
# web.leg.ufpr.br/ensino/EMR
#
#                                            Prof. Dr. Walmes M. Zeviani
#                                       Prof. Dr. Paulo Justiniano R. Jr
#
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2019-Set-02 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Definições da sessão.

rm(list = ls())

library(rpanel)  # Interfaces em tcl/tk ou gtk.
library(splines) # Funções base para splines: bs() e ns().
library(mgcv)    # Modelos aditivos generalizados: gam().

library(lattice)      # Gráficos.
library(latticeExtra) # Complementos para a `lattice`.

#-----------------------------------------------------------------------
# Prepara conjuntos de dados para conduzir a apresentação.

nile <- data.frame(x = seq_along(Nile), y = as.numeric(Nile))
plot(y ~ x, nile)

sui <- read.table("http://www.leg.ufpr.br/~walmes/data/preabate.txt",
                  header = TRUE, dec = ",")
sui$trat <- factor(-1 * sui$trat)
levels(sui$trat) <- c("Sem aspersão", "Com aspersão")
str(sui)

xyplot(temp ~ hora,
       groups = trat,
       data = sui,
       xlab = "Minutos à partir das 07:00 da manhã",
       ylab = "Temperatura corporal (ºC)",
       auto.key = TRUE) +
    glayer(panel.smoother(..., span = 0.4))

sui2 <- subset(sui, trat == "Com aspersão")

# Modelo a ser batido!!
plot(temp ~ hora, data = sui2)
with(sui2, abline(h = mean(temp)), lty = 3)

#-----------------------------------------------------------------------
# Algumas referências úteis sobre os modelos.

# [PDF] Smoothing splines,Local Regression, and GAMs · Jonathan Taylor.
browseURL("https://web.stanford.edu/class/stats202/content/lec17.pdf")

# [PDF] Cubic splines.
browseURL("http://www.maths.lth.se/na/courses/FMN081/FMN081-06/lecture11.pdf")

# [PDF] Natural cubic splines · Arne Morten Kvarving.
browseURL("https://www.math.ntnu.no/emner/TMA4215/2008h/cubicsplines.pdf")

# [PDF] Nonparametricsmoothing methods · Rob J Hyndman.
browseURL("https://robjhyndman.com/etc5410/splines.pdf")

# [HTML] Cubic and Smoothing Splines in R · Anish Singh Walia.
browseURL("https://datascienceplus.com/cubic-and-smoothing-splines-in-r/")

# [PDF] Smoothing Splines.
browseURL("http://www.stat.cmu.edu/~ryantibs/advmethods/notes/smoothspline.pdf")

# [PDF] Generalized Additive ModelHwang · Charm Lee.
browseURL("http://stat.snu.ac.kr/idea/seminar/20170703/GAM0703.pdf")

# [PDF] Generalized Additive Models · Michael Clark.
browseURL("https://m-clark.github.io/docs/GAMS.pdf")

#-----------------------------------------------------------------------
# (Global) Polynomial regression.

rp.poly <- function(y, x, er = 0) {
    annotations <- function(m0) {
        mtext(side = 3,
              adj = 0,
              line = 1.5,
              text = sprintf("X rank: %i", m0$rank))
        mtext(side = 3, adj = 0, line = 0.5,
              text = sprintf("Residual DF: %i", m0$df.residual))
        press <- sum((residuals(m0)/(1 - hatvalues(m0)))^2)
        mtext(side = 3, adj = 1, line = 0.5,
              text = sprintf("PRESS: %0.2f", press))
        sm0 <- summary(m0)
        lastcoef <- sprintf("High term p-value: %0.5f",
                            sm0$coeff[length(coef(m0)), 4])
        mtext(side = 3, adj = 1, line = 2.5,
              text = lastcoef)
        mtext(side = 3, adj = 1, line = 1.5,
              text = sprintf("R² (adj. R²): %0.2f (%0.2f)",
                             100 * sm0$r.squared,
                             100 * sm0$adj.r.squared))
    }
    draw.poly <- function(panel) {
        with(panel, {
            m0 <- lm(y ~ poly(x, degree = degree))
            xr <- extendrange(x, f = er)
            yr <- extendrange(y, f = er)
            xx <- seq(xr[1], xr[2], length.out = 200)
            cb <- predict(m0,
                          newdata = data.frame(x = xx),
                          interval = "confidence")
            plot(y ~ x, xlim = xr, ylim = yr, type = "n")
            # matlines(xx, cb, lty = c(1, 2, 2), col = 1)
            polygon(c(xx, rev(xx)),
                    c(cb[, 2], rev(cb[, 3])),
                    # col = "orange",
                    col = adjustcolor("orange", alpha.f = 0.5),
                    border = "gray")
            lines(xx, cb[, 1])
            points(y ~ x)
            annotations(m0)
        })
        return(panel)
    }
    maxd <- length(unique(x)) - 1
    panel <- rp.control(x = x, y = y, er = er)
    rp.doublebutton(panel = panel,
                    variable = degree,
                    action = draw.poly,
                    showvalue = TRUE,
                    step = 1,
                    initval = 1,
                    range = c(1, maxd),
                    title = "Degree of polynomial")
    rp.do(panel, action = draw.poly)
}

rp.poly(x = sui2$hora, y = sui2$temp, er = 0.1)

rp.poly(x = nile$x, y = nile$y, er = 0.1)

rp.poly(x = cars$speed, y = cars$dist, er = 0.1)

rp.poly(x = faithful$eruptions, y = faithful$waiting, er = 0.1)

#-----------------------------------------------------------------------
# Regression splines · Regressão por splines.

rp.spline <- function(x, y, er = 0, ...) {
    plotfit <- function(x, y, m0, er) {
        xr <- extendrange(x, f = er)
        xx <- seq(xr[1], xr[2], length.out = 200)
        cb <- predict(m0, newdata = data.frame(x = xx),
                      interval = "confidence")
        # matlines(xx, cb, lty = c(1, 2, 2), col = 1)
        # points(y ~ x)
        polygon(c(xx, rev(xx)),
                c(cb[, 2], rev(cb[, 3])),
                # col = "orange",
                col = adjustcolor("orange", alpha.f = 0.5),
                border = "gray")
        lines(xx, cb[, 1])
        points(y ~ x)
    }
    annotations <- function(m0) {
        mtext(side = 3, adj = 0, line = 1.5,
              text = sprintf("X rank: %i", m0$rank))
        mtext(side = 3, adj = 0, line = 0.5,
              text = sprintf("Residual DF: %i", m0$df.residual))
        press <- sum((residuals(m0)/(1 - hatvalues(m0)))^2)
        mtext(side = 3, adj = 1, line = 0.5,
              text = sprintf("PRESS: %0.2f", press))
        sm0 <- summary(m0)
        mtext(side = 3, adj = 1, line = 1.5,
              text = sprintf("R² (adj. R²): %0.2f (%0.2f)",
                             100 * sm0$r.squared,
                             100 * sm0$adj.r.squared))
    }
    draw.df.degree <- function(panel) {
        with(panel, {
            xr <- extendrange(x, f = er)
            yr <- extendrange(y, f = er)
            plot(y ~ x, xlim = xr, ylim = yr, type = "n", ...)
            nk <- as.integer(df1) - as.integer(degree1)
            nk <- max(c(0, nk))
            if (quantile) {
                s <- seq(0, 1, length.out = 2 + nk)
                q <- quantile(x, probs = s[-c(1, nk + 2)])
                abline(v = q, col = "gray50", lty = 2)
            }
            switch(base1,
                   bs = {
                       m0 <- lm(y ~ bs(x, degree = as.integer(degree1),
                                       df = as.integer(df1)))
                       plotfit(x, y, m0, er)
                       annotations(m0)
                       mtext(side = 3, line = 0.5,
                             text = sprintf(
                                 "Number of internal nodes: %i", nk))
                   },
                   ns = {
                       m0 <- lm(y ~ ns(x, df = as.integer(df1)))
                       plotfit(x, y, m0, er)
                       annotations(m0)
                       mtext(side = 3, line = 0.5,
                             text = sprintf(
                                 "Number of internal nodes: %i", nk))
                   })
        })
        return(panel)
    }
    draw.degree.knots <- function(panel) {
        with(panel, {
            xr <- extendrange(x, f = er)
            yr <- extendrange(y, f = er)
            plot(y ~ x, xlim = xr, ylim = yr, type = "n", ...)
            abline(v = k, col = 2, lty = 2)
            switch(base2,
                   bs = {
                       m0 <- lm(y ~ bs(x, degree = as.integer(degree2),
                                       knots = k))
                       plotfit(x, y, m0, er)
                       annotations(m0)
                   },
                   ns = {
                       if (is.na(k)) {
                           m0 <- lm(y ~ ns(x, df = as.integer(degree2)))
                       } else {
                           m0 <- lm(y ~ ns(x, knots = k))
                       }
                       plotfit(x, y, m0, er)
                       annotations(m0)
                   })
        })
        return(panel)
    }
    reset.draw <- function(panel) {
        k <- locator(type = "p", pch = 19, col = 2)$x
        if (length(k) == 0) {
            panel$k <- NA
        } else {
            panel$k <- k
            abline(v = k, col = 2, lty = 2)
        }
        panel$degree2 <- 1
        rp.do(panel, draw.degree.knots)
        return(panel)
    }
    panel <- rp.control(x = x, y = y, k = NA, ...)
    rp.notebook(panel,
                tabnames = c("DF", "Knots"),
                tabs = c("Control DF", "Choose knots"),
                width = 280,
                height = 200)
    rp.radiogroup(panel, variable = base1, vals = c("bs", "ns"),
                  action = draw.df.degree, title = "Base spline",
                  parentname = "DF")
    rp.checkbox(panel, variable = quantile, action = draw.df.degree,
                title = "Show quantiles of x", parentname = "DF")
    rp.doublebutton(panel, variable = df1, action = draw.df.degree,
                    showvalue = TRUE, step = 1, initval = 3,
                    range = c(1, 10), title = "Degrees of freedom",
                    parentname = "DF")
    rp.doublebutton(panel, variable = degree1, action = draw.df.degree,
                    showvalue = TRUE, step = 1, initval = 3,
                    range = c(1, 10), title = "Polynomial degree",
                    parentname = "DF")
    rp.radiogroup(panel, variable = base2, vals = c("bs", "ns"),
                  action = draw.degree.knots, title = "Base spline",
                  parentname = "Knots")
    rp.doublebutton(panel, variable = degree2,
                    action = draw.degree.knots, showvalue = TRUE,
                    step = 1, initval = 3, range = c(1, 10),
                    title = "Polynomial degree", parentname = "Knots")
    rp.button(panel, action = reset.draw,
              title = "Click to select knots", parentname = "Knots")
    rp.do(panel, action = draw.df.degree)
}

rp.spline(x = sui2$hora, y = sui2$temp, er = 0.05)

rp.spline(x = nile$x, y = nile$y, er = 0.05)

rp.spline(x = cars$speed, y = cars$dist, er = 0.05)

rp.spline(x = faithful$eruptions, y = faithful$waiting, er = 0.05)

#-----------------------------------------------------------------------
# Smoothing splines · Regressão com suavização por splines.

rp.smooth.spline <- function(x, y, er = 0, ...) {
    plotfit <- function(x, y, m0, er, ...) {
        xr <- extendrange(x, f = er)
        yr <- extendrange(y, f = er)
        xx <- seq(xr[1], xr[2], length.out = 200)
        plot(y ~ x, xlim = xr, ylim = yr, ...)
        cb <- predict(m0, x = xx)
        lines(cb$x, cb$y, lty = 1, col = 1)
    }
    annotations <- function(m0, y) {
        mtext(side = 3, adj = 0, line = 1.5,
              text = sprintf("Equiv. DF: %0.1f", m0$df))
        mtext(side = 3, adj = 0, line = 0.5,
              text = sprintf("Minimized crit.: %0.3f", m0$crit))
        mtext(side = 3, adj = 1, line = 0.5,
              text = sprintf("Smoothing parameter: %0.2f", m0$spar))
        r2 <- 100 * cor(fitted(m0), y)^2
        mtext(side = 3, adj = 1, line = 1.5,
              text = sprintf("R²: %0.2f", r2))
    }
    draw.sspline <- function(panel) {
        with(panel, {
            switch(control,
                   df = {
                       m0 <- smooth.spline(x = x, y = y, df = df)
                   },
                   spar = {
                       m0 <- smooth.spline(x = x, y = y, spar = spar)
                   })
            plotfit(x, y, m0, er, ...)
            annotations(m0, y)
        })
        return(panel)
    }
    draw.default <- function(panel) {
        m0 <- smooth.spline(x = x, y = y)
        plotfit(x, y, m0, er, ...)
        annotations(m0, y)
        mtext(side = 3, line = -1.5, text = "Default fit", col = 4)
        return(panel)
    }
    panel <- rp.control(x = x, y = y, er = er)
    rp.do(panel, action = draw.default)
    rp.radiogroup(panel, variable = control,
                  vals = c("spar", "df"),
                  labels = c("Smoothing parameter",
                             "Equivalent degrees of freedom"),
                  title = "Argument in control", action = draw.sspline)
    rp.slider(panel, variable = spar, from = 0, to = 1, initval = 0.5,
              action = draw.sspline, title = "Smoothing parameter",
              resolution = 0.05, showvalue = TRUE)
    rp.doublebutton(panel, variable = df, range = c(1.5, length(x) - 1),
                    initval = 1.5, step = 0.5, showvalue = TRUE,
                    action = draw.sspline,
                    title = "Equivalent DF")
    rp.button(panel, action = draw.default, title = "Default fit")
}

rp.smooth.spline(x = sui2$hora, y = sui2$temp, er = 0.05)

rp.smooth.spline(x = nile$x, y = nile$y, er = 0.05)

rp.smooth.spline(x = cars$speed, y = cars$dist, er = 0.05)

rp.smooth.spline(faithful$eruptions, y = faithful$waiting, er = 0.05)

#-----------------------------------------------------------------------
# Local regression · Regressão local.

rp.loess <- function(y, x, n = 20, er = 0, ...) {
    annotations <- function(m0, y) {
        mtext(side = 3, adj = 0, line = 1.5,
              text = sprintf("H trace: %0.2f", m0$trace.hat))
        mtext(side = 3, adj = 0, line = 0.5,
              text = sprintf("Equivalent num. param.: %0.2f", m0$enp))
        r2 <- 100 * cor(fitted(m0), y)^2
        mtext(side = 3, adj = 1, line = 1.5,
              text = sprintf("R²: %0.2f", r2))
    }
    draw.loess <- function(panel) {
        with(panel, {
            m0 <- loess(y ~ x, span = span, degree = degree,
                        family = "gaussian")
            erx <- extendrange(x, f = er)
            xx <- seq(erx[1], erx[2], length.out = 200)
            yy <- predict(m0,
                          newdata = data.frame(x = xx),
                          se = TRUE)
            a <- abs(x - x0)
            if (span < 1) {
                q <- as.integer(span * length(x))
                d <- sort(a)[q]
            } else {
                q <- length(x)
                d <- max(abs(x - x0)) * sqrt(span)
            }
            w <- rep(0, length(x))
            s <- a <= d
            w[s] <- (1 - (a[s]/d)^3)^3
            i <- as.integer(s)
            xl <- range(x)
            xl[1] <- ifelse(x0 - d > xl[1], x0 - d, xl[1])
            xl[2] <- ifelse(x0 + d < xl[2], x0 + d, xl[2])
            f0 <- function(...) {
                y.pred <- sum(w * y)/sum(w)
                segments(xl[1], y.pred, xl[2], y.pred, col = 2, lwd = 2,
                         ...)
            }
            f1 <- function(...) {
                m <- lm(y ~ poly(x, degree = 1), weights = w)
                y.pred <- predict(m, newdata = list(x = xl))
                segments(xl[1], y.pred[1], xl[2], y.pred[2],
                         col = 2, lwd = 2, ...)
            }
            f2 <- function(...) {
                m <- lm(y ~ poly(x, degree = as.integer(degree)),
                        weights = w)
                x.pred <- seq(xl[1], xl[2], length.out = n)
                y.pred <- predict(m, newdata = list(x = x.pred))
                lines(x.pred, y.pred, col = 2, lwd = 2, ...)
            }
            cb <- cbind(yy$fit,
                        yy$fit - 1.96 * yy$se.fit,
                        yy$fit + 1.96 * yy$se.fit)
            u <- !is.na(yy$se.fit)
            plot(x, y, type = "n",
                 xlim = extendrange(x, f = er),
                 ylim = extendrange(y, f = er))
            polygon(c(xx[u], rev(xx[u])),
                    c(cb[u, 2], rev(cb[u, 3])),
                    # col = "orange",
                    col = adjustcolor("orange", alpha.f = 0.5),
                    border = "gray")
            lines(xx, cb[, 1])
            points(x, y, pch = 2 * (!s) + 1, cex = i * 3 * w + 1, ...)
            abline(v = c(x0, xl), lty = c(2, 3, 3))
            annotations(m0, y)
            mtext(side = 3, adj = 1, line = 0.5,
                  text = sprintf("Number of obs. used/total: %i/%i",
                                 sum(w > 0), length(y)))
            switch(findInterval(degree, c(-Inf, 1, 2, Inf)),
                   `0` = f0(), `1` = f1(), `2` = f2())
        })
        panel
    }
    xr <- extendrange(x, f = er)
    panel <- rp.control(x = x, y = y, n = n, er = er)
    rp.doublebutton(panel, variable = degree, action = draw.loess,
                    showvalue = TRUE, step = 1, initval = 0,
                    range = c(0, 2), title = "Degree")
    rp.slider(panel, variable = x0, action = draw.loess, from = xr[1],
              to = xr[2], initval = median(range(x)), showvalue = TRUE,
              title = "x-value")
    rp.slider(panel, variable = span, action = draw.loess, from = 0,
              to = 1.5, initval = 0.75, showvalue = TRUE,
              title = "span")
    rp.do(panel, action = draw.loess)
}

rp.loess(x = sui2$hora, y = sui2$temp, er = 0.05)

rp.loess(x = nile$x, y = nile$y, er = 0.05)

rp.loess(x = cars$speed, y = cars$dist, n = 25, er = 0.05)

rp.loess(x = faithful$eruptions, y = faithful$waiting, er = 0.05)

#-----------------------------------------------------------------------
# GAM · Modelos aditivos generalizados.

rp.gam <- function(y, x, er = 0, ...) {
    annotations <- function(m0) {
        sm0 <- summary(m0)
        mtext(side = 3, adj = 0, line = 1.5,
              text = sprintf("Equivalent DF: %0.2f", sm0$edf))
        mtext(side = 3, adj = 0, line = 0.5,
              text = sprintf("Residual DF: %0.1f", sm0$residual.df))
        mtext(side = 3, adj = 1, line = 0.5,
              text = sprintf("Smoothing parameter: %0.2f",
                             m0$smooth[[1]]$sp))
        mtext(side = 3, adj = 1, line = 1.5,
              text = sprintf("R²: %0.2f", 100 * sm0$r.sq))
    }
    plotfit <- function(x, y, m0, er, ...) {
        b0 <- coef(m0)[1]
        xr <- extendrange(x, f = er)
        yr <- extendrange(y, f = er)
        xx <- seq(xr[1], xr[2], length.out = 200)
        yy <- predict(m0, newdata = list(x = xx), se.fit = TRUE)
        cb <- cbind(yy$fit,
                    yy$fit - 1.96 * yy$se.fit,
                    yy$fit + 1.96 * yy$se.fit)
        u <- !is.na(yy$se.fit)
        plot(x, y, type = "n", xlim = xr, ylim = yr, ...)
        polygon(c(xx[u], rev(xx[u])),
                c(cb[u, 2], rev(cb[u, 3])),
                col = adjustcolor("orange", alpha.f = 0.5),
                border = "gray")
        lines(xx, cb[, 1])
        points(x, y)
    }
    draw.gam <- function(panel) {
        with(panel, {
            m0 <- gam(y ~ s(x, k = k, sp = sp))
            plotfit(x, y, m0, er, ...)
            annotations(m0)
        })
        return(panel)
    }
    draw.default <- function(panel) {
        with(panel, {
            m0 <- gam(y ~ s(x))
            plotfit(x, y, m0, er, ...)
            annotations(m0)
            mtext(side = 3, line = -1.5, text = "Default fit",
                  col = 4)
        })
        return(panel)
    }
    panel <- rp.control(x = x, y = y, er = er, ...)
    # panel <- rp.control(x = x, y = y, er = er)#, ...)
    maxd <- length(unique(x)) - 1
    rp.doublebutton(panel, variable = k, action = draw.gam,
                    showvalue = TRUE, step = 1, initval = -1,
                    range = c(-1, maxd), title = "Degree of polynomial")
    rp.slider(panel, variable = sp, from = 0, to = 1, initval = 0.1,
              action = draw.gam, title = "Smoothing parameter",
              showvalue = TRUE)
    rp.button(panel, action = draw.default, title = "Default fit")
    rp.do(panel, action = draw.default)
}

rp.gam(x = sui2$hora, y = sui2$temp, er = 0.05)

rp.gam(x = nile$x, y = nile$y, er = 0.05)

rp.gam(x = cars$speed, y = cars$dist, er = 0.05)

rp.gam(faithful$eruptions, y = faithful$waiting, er = 0.05)

#-----------------------------------------------------------------------
# Funções inspecionar as bases construidas com splines.

rp.base <- function(x) {
    draw <- function(panel) {
        with(panel, {
            switch(base,
                   bs = {
                       X <- model.matrix(~bs(x, df = df,
                                             degree = degree))
                       matplot(x = x, y = X[, -1], type = "l")
                       lines(x = x, y = rowSums(X[, -1]),
                             col = 1, lty = 1, lwd = 2)
                   },
                   ns = {
                       X <- model.matrix(~ns(x, df = df))
                       matplot(x = x, y = X[, -1], type = "l")
                       # lines(x = x, y = rowSums(X[, -1]),
                       #       col = 1, lty = 1, lwd = 2)
            })
        })
        return(panel)
    }
    panel <- rp.control(x = x)
    rp.radiogroup(panel, variable = base, vals = c("bs", "ns"),
                  action = draw, title = "Base spline")
    rp.doublebutton(panel, variable = df, action = draw,
                    showvalue = TRUE, step = 1, initval = 3,
                    range = c(1, 10), title = "Degrees of freedom")
    rp.doublebutton(panel, variable = degree, action = draw,
                    showvalue = TRUE, step = 1, initval = 3,
                    range = c(1, 10), title = "Polynomial degree")
}

x <- seq(0, 10, 0.1)
rp.base(x)

#-----------------------------------------------------------------------
# Propriedades das bases.

x <- seq(0, 10, by = 0.25)

#--------------------------------------------
# Polinômio ortogonal.

# Base de termos polinômiais em x com polinômios ortogonais.
X <- poly(x, degree = 5, raw = FALSE)
dim(X)

# Matriz X'X ortonormal.
print.table(crossprod(X), zero.print = ".")

# Gráfico.
matplot(x, X, type = "l")
abline(h = 0, lty = 2, col = "orange")

#--------------------------------------------
# Polinômio natural/ordinário.

# Base de termos polinômiais em x com polinômios ordinários.
X <- poly(x, degree = 5, raw = TRUE)
dim(X)

# Matriz X'X não é ortogonal. Pelo contrário. Tem uma condição muito
# ruim.
print.table(crossprod(X), zero.print = ".")

# Gráfico.
matplot(x, X, type = "l")
abline(h = 0, lty = 2, col = "orange")

#--------------------------------------------
# Bases com splines cúbicos (base splines).

# Variar o número para `degree`.
X <- bs(x, df = 5, degree = 1)
dim(X)

# Um padrão interessante que revela sobreposição entre variáveis
# vizinhas.
print.table(crossprod(X), zero.print = ".")

# O gráfico mostra a sobreposição. Sem a sobreposição gradual concorda
# que não haveria continuidade na função?
matplot(x, X, type = "l")
abline(h = 0, lty = 2, col = "orange")

#--------------------------------------------
# Base splines feitos na mão.

quantile(x, probs = 1:2/3)

ppos_cub <- function(x, c) {
    (x - c)^3 * c(x > c)
}

X <- cbind(x, x^2, x^3, ppos_cub(x, 3.33), ppos_cub(x, 6.67))
dim(X)

# Matriz com uma condição ruim.
print.table(crossprod(X), zero.print = ".")

# Gráfico é dominado pelos termos cúbicos.
matplot(x, X, type = "l")
abline(h = 0, lty = 2, col = "orange")

#--------------------------------------------
# Bases com natural splines.

X <- ns(x, df = 5)
dim(X)

print.table(crossprod(X), zero.print = ".")

matplot(x, X, type = "l")
abline(h = 0, lty = 2, col = "orange")

#-----------------------------------------------------------------------
