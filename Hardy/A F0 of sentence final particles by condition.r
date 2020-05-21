library(readr)
library(ggplot2)
library(nlme)


add.labels <- "yes"
# add.labels <- "no"


dir <- paste(getwd(), "sentence-final-particles/SFP-F0-files", sep="/")

tasks <- c("alf", "ali", "alm", "als", "cl")
# tasks <- c("alf", "ali", "alm", "als", "alw", "cl", "ch1", "ch2", "ch3", "ch4", "ch5")


monophthongs <- c("i", "ɪ", "ɛ", "y", "œ", "ɵ", "ɐ", "aː", "ɔ", "ʊ", "u")
diphthongs <- c("iu", "ei", "ɛu", "ɵy", "ɐi", "ɐu", "aːi", "aːu", "ɔi", "ou", "ui")
vowels <- c(monophthongs)

punctuations <- c(".", "?", ",", "ǃ")

conditions <- c("ADS", "CDS")



in.file1 <- "formants-vowels-monophthongs.csv"
in.file2 <- "times-utterances.csv"
in.file3 <- "times-phonemes.csv"
ref.file <- "list-SFP-items.txt"

out.file1 <- "F0-SFP-vowels.txt"
out.file2 <- "F0-SFP-means-by-talker.txt"
out.file3 <- "F0-SFP-means-by-SFP.txt"
out.file4 <- "F0-SFP-LMER-output.txt"
out.file5 <- "F0-SFP-LMER-estimates.txt"
out.fig.file1 <- "barchart-SFPs-F0-by-talker.png"
out.fig.file2 <- "barchart-SFPs-F0-by-SFP.png"
out.fig.file3 <- "barchart-SFPs-F0-pooled.png"

in.path1 <- paste(dir, in.file1, sep="/")
in.path2 <- paste(dir, in.file2, sep="/")
in.path3 <- paste(dir, in.file3, sep="/")
ref.path <- paste(dir, ref.file, sep="/")
out.path1 <- paste(dir, out.file1, sep="/")
out.path2 <- paste(dir, out.file2, sep="/")
out.path3 <- paste(dir, out.file3, sep="/")
out.path4 <- paste(dir, out.file4, sep="/")
out.path5 <- paste(dir, out.file5, sep="/")
out.fig.path1 <- paste(dir, out.fig.file1, sep="/")
out.fig.path2 <- paste(dir, out.fig.file2, sep="/")
out.fig.path3 <- paste(dir, out.fig.file3, sep="/")


dat.acous <- read_csv(in.path1, col_names=T)
dat.utt <- read_csv(in.path2, col_names=T)
dat.phon <- read_csv(in.path3, col_names=T)

ref.dat <- read_tsv(ref.path, col_names=T)
SFPs <- ref.dat$chinese
SFPs.jp <- ref.dat$jyutping
SFPs.ipa <- ref.dat$IPA
SFPs.lab <- ref.dat$label



if (!(file.exists(out.path1))) {
  df.out <- data.frame(matrix(nrow=0, ncol=8))
  names(df.out) <- c("talker", "recording", "task", "condition", "SFP.chinese", "SFP.jyutping", "SFP.ipa", "F0.hz")
  for (r in 1:nrow(dat.utt)) {
    row <- dat.utt[r, ]
    t <- row$session
    rec <- row$recording
    task <- substring(rec,5,nchar(rec))
    if (task %in% tasks) {
      utt <- row$chinese
      t.ons <- row$time.onset
      t.off <- row$time.offset
      length.utt <- nchar(utt)
      syll.fin <- substring(utt, length.utt, length.utt)
      if (syll.fin %in% punctuations) {
        syll.fin <- substring(utt, length.utt-1, length.utt-1)
      }
      if (syll.fin %in% SFPs) {
        chin <- syll.fin
        jp <- SFPs.jp[match(chin, SFPs)]
        ipa <- SFPs.ipa[match(chin, SFPs)]
        phon.rows <- dat.phon[dat.phon$recording==rec & dat.phon$time.onset>=t.ons & dat.phon$time.offset<=t.off & dat.phon$position=="nucleus", ]
        if (nrow(phon.rows)>0) {
          n.nuclei <- nrow(phon.rows)
          row.fin <- phon.rows[n.nuclei, ]
          last.syll <- row.fin$syllable.chinese
          if (chin==last.syll) {
            phon.no <- row.fin$phoneme.no
            acoust.row <- dat.acous[dat.acous$recording==rec & dat.acous$phoneme.no==phon.no,]
            if (nrow(acoust.row)==1) {
              cond <- acoust.row$condition
              f0 <- acoust.row$F0.hz
              if (is.numeric(f0)) {
                f0 <- round(f0,2)
              }
            } else if (nrow(acoust.row)>1) {
              cond <- unique(acoust.row$condition)
              f0 <- unique(acoust.row$F0.hz)
              if (is.numeric(f0)) {
                f0 <- round(f0,2)
              }
            }
            df.row <- data.frame(talker=t, recording=rec, task=task, condition=cond, SFP.chinese=chin, SFP.jyutping=jp, SFP.ipa=ipa, F0.hz=f0)
            df.out <- rbind(df.out, df.row)
          }
        }
      }
    }
  }
  write_tsv(df.out, out.path1, col_names=T)
  df <- df.out
} else {
  df <- read_tsv(out.path1, col_names=T)
}
talkers <- unique(df$talker)



# Bar charts by talker
for (t in talkers) {
  df.t <- df[df$talker==t, ]
  talker.conds <- as.character(unique(df.t$condition))
  for (cond in conditions) {
    set <- df.t[df.t$talker==t & df.t$condition==cond, ]
    if (nrow(set)>0) {
      n <- nrow(set[!(is.na(set$F0.hz)), ])
      mean <- mean(set$F0.hz, na.rm=T)
      sd <- sd(set$F0.hz, na.rm=T)
      se <- sd/sqrt(n)
    } else {
      n <- 0
      mean <- NA
      sd <- NA
      se <- NA
    }
    summ.row <- data.frame(talker=t, condition=cond, n=n, mean.F0=mean, std.dev.F0=sd, std.err.F0=se)
    if (t==talkers[1] & cond==conditions[1]) {
      df.summ <- summ.row
    } else {
      df.summ <- rbind(df.summ, summ.row)
    }
  }
}
write_tsv(df.summ, out.path2)
dodge <- position_dodge(width=0.85)
limits <- aes(ymax=mean.F0+std.err.F0, ymin=mean.F0-std.err.F0)
g <- ggplot(data=df.summ, aes(x=condition, y=mean.F0, fill=condition)) + theme_bw() + facet_wrap(~talker)
g <- g + geom_bar(stat="identity", color="black", size=0.25, position=dodge, width=0.8) + geom_errorbar(limits, position=dodge, width=0.25, size=0.5)
g <- g + geom_hline(yintercept=0, size=0.5)
if (add.labels=="yes") {
  g <- g + geom_text(aes(x=match(condition,conditions), y=mean.F0/2, label=n), size=3)
  g <- g + geom_text(aes(x=match(condition,conditions), y=mean.F0+60, label=round(mean.F0,1)), size=3)
}
g <- g + scale_x_discrete(name="Condition") + scale_y_continuous(name="Mean F0 (Hz)")
g <- g + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), text=element_text(size=15), axis.title=element_text(size=20, face="bold"))
png(out.fig.path1, width=200, height=200, family="Times New Roman", units="mm", res=300)
print(g)
dev.off()



# Bar charts by SFP
for (sfp in SFPs) {
  sfp.jp <- SFPs.jp[match(sfp, SFPs)]
  sfp.lab <- paste(sfp, sfp.jp)
  df.sfp <- df[df$SFP.chinese==as.character(sfp), ]
  sfp.conds <- as.character(unique(df.sfp$condition))
  for (cond in conditions) {
    set <- df.sfp[df.sfp$SFP.chinese==sfp & df.sfp$condition==cond, ]
    if (nrow(set)>0) {
      n <- nrow(set[!(is.na(set$F0.hz)), ])
      mean <- mean(set$F0.hz, na.rm=T)
      sd <- sd(set$F0.hz, na.rm=T)
      se <- sd/sqrt(n)
    } else {
      n <- 0
      mean <- NA
      sd <- NA
      se <- NA
    }
    summ.row <- data.frame(SFP=sfp, SFP.label=sfp.lab, condition=cond, n=n, mean.F0=mean, std.dev.F0=sd, std.err.F0=se)
    if (sfp==SFPs[1] & cond==conditions[1]) {
      df.summ <- summ.row
    } else {
      df.summ <- rbind(df.summ, summ.row)
    }
  }
}
write_tsv(df.summ, out.path3)
dodge <- position_dodge(width=0.85)
limits <- aes(ymax=mean.F0+std.err.F0, ymin=mean.F0-std.err.F0)
h <- ggplot(data=df.summ, aes(x=condition, y=mean.F0, fill=condition)) + theme_bw() + facet_wrap(~SFP)
h <- h + geom_bar(stat="identity", color="black", size=0.25, position=dodge, width=0.8) + geom_errorbar(limits, position=dodge, width=0.25, size=0.5)
h <- h + geom_hline(yintercept=0, size=0.5)
if (add.labels=="yes") {
  h <- h + geom_text(aes(x=match(condition,conditions), y=mean.F0/2, label=n), size=3)
  h <- h + geom_text(aes(x=match(condition,conditions), y=mean.F0+60, label=round(mean.F0,1)), size=3)
}
h <- h + scale_x_discrete(name="Condition") + scale_y_continuous(name="Mean F0 (Hz)")
h <- h + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), text=element_text(size=15), axis.title=element_text(size=20, face="bold"))
png(out.fig.path2, width=200, height=200, family="Times New Roman", units="mm", res=300)
print(h)
dev.off()




df.lm <- df
df.lm <- df.lm[!(is.na(df.lm$F0.hz)), ]
lm <- lme(F0.hz ~ condition, random=list(talker=~1, SFP.chinese=~1), data=df.lm)
summ.table <- summary(lm)$tTable
write_tsv(data.frame(summ.table), out.path4)
est1 <- summ.table["(Intercept)", "Value"]
se1 <- summ.table["(Intercept)", "Std.Error"]
df1 <- summ.table["(Intercept)", "DF"]
pval <- summ.table["conditionCDS", "p-value"]
if (pval<0.0001) {
  sig.code <- "****"
  pval <- "< 0.0001"
} else {
  if (pval>=0.05) {
    sig.code <- "n.s."
  } else if (pval<0.05 & pval>=0.01) {
    sig.code <- "*"
  } else if (pval<0.01 & pval>=0.001) {
    sig.code <- "**"
  } else if (pval<0.001 & pval>=0.0001) {
    sig.code <- "***"
  }
  pval <- as.character(round(pval, 6))
}
df.lm$condition <- factor(df.lm$condition, levels=rev(conditions))
lm <- lme(F0.hz ~ condition, random=list(talker=~1, SFP.chinese=~1), data=df.lm)
summ.table <- summary(lm)$tTable
est2 <- summ.table["(Intercept)", "Value"]
se2 <- summ.table["(Intercept)", "Std.Error"]
df2 <- summ.table["(Intercept)", "DF"]
df.row1 <- data.frame(condition="ADS", estimate=est1, std.error=se1, df=df1, p.value=pval, sig.code=sig.code)
df.row2 <- data.frame(condition="CDS", estimate=est2, std.error=se2, df=df2, p.value=NA, sig.code=NA)
df.summ <- rbind(df.row1, df.row2)
write_tsv(df.summ, out.path5)
df.summ$condition <- factor(df.summ$condition, levels=conditions)
dodge <- position_dodge(width=0.85)
limits <- aes(ymax=estimate+std.error, ymin=estimate-std.error)
i <- ggplot(data=df.summ, aes(x=condition, y=estimate, fill=condition)) + theme_bw()
i <- i + geom_bar(stat="identity", color="black", size=0.25, position=dodge, width=0.8) + geom_errorbar(limits, position=dodge, width=0.25, size=0.5)
i <- i + geom_hline(yintercept=0, size=0.5)
i <- i + geom_text(aes(x=1.5, y=270, label=sig.code), size=7)
if (add.labels=="yes") {
  i <- i+ geom_text(aes(x=match(condition,conditions), y=estimate+20, label=round(estimate,2)), size=6)
}
i <- i + scale_x_discrete(name="Condition") + scale_y_continuous(name="F0 estimate (Hz)")
i <- i + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), text=element_text(size=15), axis.title=element_text(size=20, face="bold"))
png(out.fig.path3, width=200, height=200, family="Times New Roman", units="mm", res=300)
print(i)
dev.off()
