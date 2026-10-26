# case_study_public_data.R
#
# Applied cases of the AssumptionsLab article, run on PUBLIC data with the module's own
# analyses, plus a numeric verification of the module's output against independent
# reference implementations.
#
#   groupCheck : lavaan::HolzingerSwineford1939, x1 (visual perception) by school
#   regCheck   : psychTools::epi.bfi, bdi ~ bfneur + bfext + bfopen + bfagree + bfcon
#   pathCheck  : psychTools::epi.bfi, (bfneur, bfext) -> traitanx -> bdi, plus direct paths
#
# Usage (from anywhere):
#   Rscript case_study_public_data.R <path-to-AssumptionsLab-repo>
#   DUMP=1 Rscript case_study_public_data.R <path>   # also prints every result table
#
# Requires: pkgload, lavaan, psychTools, car, lmtest, energy, tseries (plus the module's own
# dependencies). Takes several minutes (regCheck and pathCheck use permutation tests).
# Printed at the end: the comparison table (Reference vs Module) and the session versions.
args <- commandArgs(TRUE); repo <- if (length(args)) args[1] else "."
suppressMessages(pkgload::load_all(repo, quiet = TRUE))
set.seed(2026)
num <- function(x) as.numeric(sub("^<", "", sub("^\\.", "0.", as.character(x))))

# ---- data ----
hs <- lavaan::HolzingerSwineford1939; hs$school <- as.factor(hs$school)
epi <- psychTools::epi.bfi
# ---- module runs ----
g <- groupCheck(data = hs, dep = "x1", group = "school")
noplots <- list(showPlots=FALSE, linShowPlots=FALSE, normShowPlots=FALSE, homoShowPlots=FALSE,
                corrShowIndividual=FALSE, corrShowComparative=FALSE, influenceShowPlots=FALSE)
preds <- c("bfneur","bfext","bfopen","bfagree","bfcon")
r <- do.call(regCheck, c(list(data = epi, dep = "bdi", covs = preds), noplots))
rel <- list(list(i1="bfneur",i2="traitanx"), list(i1="bfext",i2="traitanx"),
            list(i1="bfneur",i2="bdi"), list(i1="bfext",i2="bdi"), list(i1="traitanx",i2="bdi"))
p <- pathCheck(data = epi, vars = c("bfext","bfneur","traitanx","bdi"), relations = rel,
               isFinalized = TRUE, showDiagram = FALSE)

# ---- module values ----
N <- g$normality$asDF; sw <- N[N$test == "Shapiro-Wilk", ]; jb <- N[N$test == "Jarque-Bera", ]
H <- g$homogeneity$asDF
MS <- r$modelSummary$asDF
RN <- r$residualNormality$asDF; HS <- r$homoscedasticity$asDF
MC <- r$multicollinearity$asDF; LP <- r$linearityPredictor$asDF
OT <- p$outlierTable$asDF

# ---- references ----
fit <- lm(bdi ~ bfneur + bfext + bfopen + bfagree + bfcon, data = epi)
gw <- hs$x1[hs$school == "Grant-White"]; pa <- hs$x1[hs$school == "Pasteur"]
sw_gw <- shapiro.test(gw); sw_pa <- shapiro.test(pa)
lev   <- car::leveneTest(x1 ~ school, data = hs, center = mean)
sfit  <- summary(fit)
rsw   <- shapiro.test(residuals(fit))
bp    <- lmtest::bptest(fit)
vifs  <- car::vif(fit)
d_neur <- energy::dcor(epi$bfneur, epi$bdi); d_ext <- energy::dcor(epi$bfext, epi$bdi)
pv <- epi[, c("bfext","bfneur","traitanx","bdi")]
D2 <- mahalanobis(pv, colMeans(pv), cov(pv)); cut <- qchisq(.975, ncol(pv))
jb_ref <- tseries::jarque.bera.test(pa)
# pre-1.5.1 (Bessel) Jarque-Bera, to illustrate the discrepancy that verification originally exposed
m <- mean(pa); n <- length(pa); s_b <- sd(pa)
sk_b <- mean((pa-m)^3)/s_b^3; ku_b <- mean((pa-m)^4)/s_b^4; jb_bessel <- n/6*(sk_b^2 + (ku_b-3)^2/4)

chk <- function(label, ref, mod, digits) { ref <- round(as.numeric(unname(ref)), digits); mod <- round(as.numeric(unname(mod)), digits)
  data.frame(Statistic = label, Reference = ref, Module = mod, Match = isTRUE(all.equal(ref, mod))) }
vmod <- MC[MC$diagnostic == "VIF", ]
rows <- rbind(
  chk("SW x1 Grant-White (W)", sw_gw$statistic, num(sw$value[1]), 4),
  chk("SW x1 Grant-White (p)", sw_gw$p.value,  num(sw$p[1]), 3),
  chk("SW x1 Pasteur (W)", sw_pa$statistic, num(sw$value[2]), 4),
  chk("SW x1 Pasteur (p)", sw_pa$p.value,  num(sw$p[2]), 3),
  chk("Levene (mean) F", lev$`F value`[1], num(H$value[H$test == "Levene"]), 6),
  chk("Levene (mean) p", lev$`Pr(>F)`[1], num(H$p[H$test == "Levene"]), 3),
  chk("regCheck R2", sfit$r.squared, num(MS$value[MS$item == "R²"]), 4),
  chk("regCheck adj R2", sfit$adj.r.squared, num(MS$value[MS$item == "Adjusted R²"]), 4),
  chk("Residual SW (W)", rsw$statistic, num(RN$value[RN$test == "Shapiro-Wilk"]), 4),
  chk("Breusch-Pagan LM", bp$statistic, num(HS$value[grepl("Breusch-Pagan", HS$test)]), 4),
  chk("VIF bfneur", vifs["bfneur"], vmod$value[vmod$item == "bfneur"], 4),
  chk("VIF bfext", vifs["bfext"], vmod$value[vmod$item == "bfext"], 4),
  chk("VIF bfopen", vifs["bfopen"], vmod$value[vmod$item == "bfopen"], 4),
  chk("VIF bfagree", vifs["bfagree"], vmod$value[vmod$item == "bfagree"], 4),
  chk("VIF bfcon", vifs["bfcon"], vmod$value[vmod$item == "bfcon"], 4),
  chk("dCor bfneur-bdi", d_neur, LP$value[LP$predictor == "bfneur" & grepl("dCor", LP$test)], 4),
  chk("dCor bfext-bdi", d_ext, LP$value[LP$predictor == "bfext" & grepl("dCor", LP$test)], 4),
  chk("Mahalanobis D2 > cutoff (count)", sum(D2 > cut), sum(OT$d2 > cut), 0),
  chk("Jarque-Bera Pasteur (JB)", jb_ref$statistic, num(jb$value[2]), 4),
  chk("Jarque-Bera Pasteur (p)", jb_ref$p.value, num(jb$p[2]), 3))
rownames(rows) <- NULL
print(rows, row.names = FALSE)
cat("\nAll match:", all(rows$Match), " (", sum(rows$Match), "/", nrow(rows), ")\n")
cat("\nMahalanobis: ", sum(D2 > cut), "of", nrow(pv), sprintf("(%.1f%%)", 100*mean(D2 > cut)), " cutoff =", round(cut, 2), "\n")
cat("Pre-1.5.1 (Bessel-sd) JB for Pasteur x1:", round(jb_bessel, 4), " p =", signif(pchisq(jb_bessel, 2, lower.tail = FALSE), 3),
    "| tseries/module:", round(unname(jb_ref$statistic), 4), " p =", signif(jb_ref$p.value, 3), "\n")
cat("\nSESSION: AssumptionsLab", as.character(packageVersion("AssumptionsLab")), "| R", as.character(getRversion()),
    "| lavaan", as.character(packageVersion("lavaan")), "| psychTools", as.character(packageVersion("psychTools")),
    "| car", as.character(packageVersion("car")), "| lmtest", as.character(packageVersion("lmtest")),
    "| energy", as.character(packageVersion("energy")), "| tseries", as.character(packageVersion("tseries")), "\n")

# ---- optional: dump every table the module produced (DUMP=1) ----
if (identical(Sys.getenv("DUMP"), "1")) {
  strip <- function(x) { x <- gsub("<[^>]+>", " ", x); x <- gsub("&amp;", "&", x); gsub("[ \t]+", " ", x) }
  dump_all <- function(res, title) {
    cat("\n==================", title, "==================\n")
    for (el in names(res)) {
      obj <- res[[el]]; cat("\n########", el, "########\n")
      tryCatch({
        if (inherits(obj, "Table")) print(obj$asDF, row.names = FALSE, width = 200)
        else if (inherits(obj, "Html")) cat(strip(obj$content), "\n")
      }, error = function(e) cat("[error]", conditionMessage(e), "\n"))
    }
  }
  dump_all(g, "groupCheck: x1 by school"); dump_all(r, "regCheck: bdi ~ Big Five"); dump_all(p, "pathCheck: traitanx / bdi")
}
