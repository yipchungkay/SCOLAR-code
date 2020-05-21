library(readr)
library(ggplot2)
library(nlme)

add.labels <- "yes"
# add.labels <- "no"

tasks <- c("alf", "ali", "alm", "als", "cl")
# tasks <- c("alf", "ali", "alm", "als", "alw", "cl", "ch1", "ch2", "ch3", "ch4", "ch5")

dir <- paste(getwd(), "syllable-fusion", sep="/")

conditions <- c("ADS", "CDS")

in.file1 <- "times-utterances.csv"
in.file2 <- "times-phonemes.csv"
out.file1 <- "nuclei-per-utterance.txt"
out.file2 <- "productions-per-utterance-means-by-talker.txt"
out.file3 <- "utterance-duration-LMER-output.txt"
out.file4 <- "utterance-duration-LMER-estimates.txt"
out.file5 <- "syllables-per-utterance-LMER-output.txt"
out.file6 <- "syllables-per-utterance-LMER-estimates.txt"
out.file7 <- "syllable-rate-LMER-output.txt"
out.file8 <- "syllable-rate-LMER-estimates.txt"
out.fig.file1 <- "barchart-duration-per-utterance.png"
out.fig.file2 <- "barchart-nuclei-per-utterance.png"
out.fig.file3 <- "barchart-speech-rate.png"
out.fig.file4 <- "barchart-pooled-duration-per-utterance.png"
out.fig.file5 <- "barchart-pooled-nuclei-per-utterance.png"
out.fig.file6 <- "barchart-pooled-speech-rate.png"

in.path1 <- paste(dir, in.file1, sep="/")
in.path2 <- paste(dir, in.file2, sep="/")
out.path1 <- paste(dir, out.file1, sep="/")
out.path2 <- paste(dir, out.file2, sep="/")
out.path3 <- paste(dir, out.file3, sep="/")
out.path4 <- paste(dir, out.file4, sep="/")
out.path5 <- paste(dir, out.file5, sep="/")
out.path6 <- paste(dir, out.file6, sep="/")
out.path7 <- paste(dir, out.file7, sep="/")
out.path8 <- paste(dir, out.file8, sep="/")
out.fig.path1 <- paste(dir, out.fig.file1, sep="/")
out.fig.path2 <- paste(dir, out.fig.file2, sep="/")
out.fig.path3 <- paste(dir, out.fig.file3, sep="/")
out.fig.path4 <- paste(dir, out.fig.file4, sep="/")
out.fig.path5 <- paste(dir, out.fig.file5, sep="/")
out.fig.path6 <- paste(dir, out.fig.file6, sep="/")

dat.utt <- read_csv(in.path1, col_names=T)
dat.phon <- read_csv(in.path2, col_names=T)


if (!(file.exists(out.path1))) {
  talkers <- unique(dat.utt$session)
  for (t in talkers) {
    dat.t <- dat.utt[dat.utt$session==t, ]
    recordings <- unique(dat.t$recording)
    for (rec in recordings) {
      task <- substring(rec,5,nchar(rec))
      if (task %in% tasks) {
        dat.task <- dat.t[dat.t$recording==rec, ]
        for (r in 1:nrow(dat.task)) {
          row <- dat.task[r, ]
          cond <- row$condition
          utt.no <- row$utterance.no
          utt <- row$chinese
          ipa.act <- row$ipa.actual
          t.ons <- row$time.onset
          t.off <- row$time.offset
          # dur.utt <- t.off-t.ons
          dur.utt <- (t.off-t.ons)*1000
          rows.phon <- dat.phon[dat.phon$recording==rec & dat.phon$time.onset>=t.ons & dat.phon$time.offset<=t.off, ]
          no.nucl <- nrow(rows.phon[rows.phon$position=="nucleus", ])
          rate <- no.nucl/(dur.utt/1000)
          df.row <- data.frame(talker=t, recording=rec, task=task, condition=cond, utterance.no=utt.no, utterance=utt, IPA.actual=ipa.act, duration.ms=dur.utt, no.nuclei=no.nucl, syllables.per.second=rate)
          if (t==talkers[1] & rec==recordings[1] & r==1) {
            df.out <- df.row
          } else {
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
  talkers <- unique(df$talker)
}



for (t in talkers) {
  df.t <- df[df$talker==t, ]
  talker.conds <- as.character(unique(df.t$condition))
  if (length(talker.conds)==length(conditions)) {
    for (cond in conditions) {
      set <- df[df$talker==t & df$condition==cond, ]
      n.dur <- nrow(set[!(is.na(set$duration.ms)), ])
      mean.dur <- mean(set$duration.ms, na.rm=T)
      sd.dur <- sd(set$duration.ms, na.rm=T)
      se.dur <- sd.dur/sqrt(n.dur)
      n.sylls.per.utt <- nrow(set[!(is.na(set$no.nuclei)), ])
      mean.sylls.per.utt <- mean(set$no.nuclei, na.rm=T)
      sd.sylls.per.utt <- sd(set$no.nuclei, na.rm=T)
      se.sylls.per.utt <- sd.sylls.per.utt/sqrt(n.sylls.per.utt)
      n.sylls.per.sec <- nrow(set[!(is.na(set$syllables.per.second)), ])
      mean.sylls.per.sec <- mean(set$syllables.per.second, na.rm=T)
      sd.sylls.per.sec <- sd(set$syllables.per.second, na.rm=T)
      se.sylls.per.sec <- sd.sylls.per.sec/sqrt(n.sylls.per.sec)
      
      summ.row <- data.frame(talker=t, condition=cond, n.duration=n.dur, mean.duration=mean.dur, std.err.duration=se.dur, n.syllables.per.utterance=n.sylls.per.utt, mean.syllables.per.utterance=mean.sylls.per.utt, std.err.syllables.per.utterance=se.sylls.per.utt, n.syllables.per.second=n.sylls.per.sec, mean.syllables.per.second=mean.sylls.per.sec, std.err.syllables.per.second=se.sylls.per.sec)
      if (t==talkers[1] & cond==conditions[1]) {
        df.summ <- summ.row
      } else {
        df.summ <- rbind(df.summ, summ.row)
      }
    }
  }
}
write_tsv(df.summ, out.path2)
dodge <- position_dodge(width=0.85)
limits <- aes(ymax=mean.duration+std.err.duration, ymin=mean.duration-std.err.duration)
g <- ggplot(data=df.summ, aes(x=condition, y=mean.duration, fill=condition)) + theme_bw() + facet_wrap(~talker)
g <- g + geom_bar(stat="identity", color="black", size=0.25, position=dodge, width=0.8) + geom_errorbar(limits, position=dodge, width=0.25, size=0.5)
g <- g + geom_hline(yintercept=0, size=0.5)
if (add.labels=="yes") {
  g <- g + geom_text(aes(x=match(condition,conditions), y=mean.duration/2, label=n.duration), size=2)
  g <- g + geom_text(aes(x=match(condition,conditions), y=mean.duration+std.err.duration+500, label=round(mean.duration,0)), size=3)
}
g <- g + scale_x_discrete(name="Condition") + scale_y_continuous(name="Utterance duration (ms)")
g <- g + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), text=element_text(size=15), axis.title=element_text(size=20, face="bold"))
png(out.fig.path1, width=200, height=200, family="Times New Roman", units="mm", res=300)
print(g)
dev.off()


limits <- aes(ymax=mean.syllables.per.utterance+std.err.syllables.per.utterance, ymin=mean.syllables.per.utterance-std.err.syllables.per.utterance)
h <- ggplot(data=df.summ, aes(x=condition, y=mean.syllables.per.utterance, fill=condition)) + theme_bw() + facet_wrap(~talker)
h <- h + geom_bar(stat="identity", color="black", size=0.25, position=dodge, width=0.8) + geom_errorbar(limits, position=dodge, width=0.25, size=0.5)
h <- h + geom_hline(yintercept=0, size=0.5)
if (add.labels=="yes") {
  h <- h + geom_text(aes(x=match(condition,conditions), y=mean.syllables.per.utterance/2, label=n.syllables.per.utterance), size=2)
  h <- h + geom_text(aes(x=match(condition,conditions), y=mean.syllables.per.utterance+std.err.syllables.per.utterance+2, label=round(mean.syllables.per.utterance,2)), size=3)
}
h <- h + scale_x_discrete(name="Condition") + scale_y_continuous(name="Utterance length (# syllables/utterance)")
h <- h + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), text=element_text(size=15), axis.title=element_text(size=20, face="bold"))
png(out.fig.path2, width=200, height=200, family="Times New Roman", units="mm", res=300)
print(h)
dev.off()


limits <- aes(ymax=mean.syllables.per.second+std.err.syllables.per.second, ymin=mean.syllables.per.second-std.err.syllables.per.second)
i <- ggplot(data=df.summ, aes(x=condition, y=mean.syllables.per.second, fill=condition)) + theme_bw() + facet_wrap(~talker)
i <- i + geom_bar(stat="identity", color="black", size=0.25, position=dodge, width=0.8) + geom_errorbar(limits, position=dodge, width=0.25, size=0.5)
i <- i + geom_hline(yintercept=0, size=0.5)
if (add.labels=="yes") {
  i <- i + geom_text(aes(x=match(condition,conditions), y=mean.syllables.per.second/2, label=n.syllables.per.second), size=3)
  i <- i + geom_text(aes(x=match(condition,conditions), y=mean.syllables.per.second+std.err.syllables.per.second+1, label=round(mean.syllables.per.second,2)), size=3)
}
i <- i + scale_x_discrete(name="Condition") + scale_y_continuous(name="Speech rate (# syllables/second)")
i <- i + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), text=element_text(size=15), axis.title=element_text(size=20, face="bold"))
png(out.fig.path3, width=200, height=200, family="Times New Roman", units="mm", res=300)
print(i)
dev.off()





df.lm <- df
df.lm$value <- df.lm$duration.ms
df.lm <- df.lm[!(is.na(df.lm$value)), ]
# lm <- lme(value ~ condition, random=~1|talker, data=df.lm)
lm <- lme(value ~ condition, random=list(talker=~1, task=~1), data=df.lm)
summ.table <- summary(lm)$tTable
write_tsv(data.frame(summ.table), out.path3)
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
lm <- lme(value ~ condition, random=~1|talker, data=df.lm)
summ.table <- summary(lm)$tTable
est2 <- summ.table["(Intercept)", "Value"]
se2 <- summ.table["(Intercept)", "Std.Error"]
df2 <- summ.table["(Intercept)", "DF"]
df.row1 <- data.frame(condition="ADS", estimate=est1, std.error=se1, df=df2, p.value=pval, sig.code=sig.code)
df.row2 <- data.frame(condition="CDS", estimate=est2, std.error=se2, df=df2, p.value=NA, sig.code=NA)
df.summ <- rbind(df.row1, df.row2)
write_tsv(df.summ, out.path4)
df.summ$condition <- factor(df.summ$condition, levels=conditions)
dodge <- position_dodge(width=0.85)
limits <- aes(ymax=estimate+std.error, ymin=estimate-std.error)
g <- ggplot(data=df.summ, aes(x=condition, y=estimate, fill=condition)) + theme_bw()
g <- g + geom_bar(stat="identity", color="black", size=0.25, position=dodge, width=0.8) + geom_errorbar(limits, position=dodge, width=0.25, size=0.5)
g <- g + geom_hline(yintercept=0, size=0.5)
g <- g + geom_text(aes(x=1.5, y=2250, label=sig.code), size=7)
if (add.labels=="yes") {
  g <- g + geom_text(aes(x=match(condition,conditions), y=estimate+std.error+100, label=round(estimate,0)), size=6)
}
g <- g + scale_x_discrete(name="Condition") + scale_y_continuous(name="Utterance duration (ms)")
g <- g + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), text=element_text(size=15), axis.title=element_text(size=20, face="bold"))
png(out.fig.path4, width=200, height=200, family="Times New Roman", units="mm", res=300)
print(g)
dev.off()


df.lm <- df
df.lm$value <- df.lm$no.nuclei
df.lm <- df.lm[!(is.na(df.lm$value)), ]
lm <- lme(value ~ condition, random=~1|talker, data=df.lm)
summ.table <- summary(lm)$tTable
est1 <- summ.table["(Intercept)", "Value"]
se1 <- summ.table["(Intercept)", "Std.Error"]
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
lm <- lme(value ~ condition, random=~1|talker, data=df.lm)
summ.table <- summary(lm)$tTable
est2 <- summ.table["(Intercept)", "Value"]
se2 <- summ.table["(Intercept)", "Std.Error"]
df.row1 <- data.frame(condition="ADS", estimate=est1, std.error=se1, p.value=pval, sig.code=sig.code)
df.row2 <- data.frame(condition="CDS", estimate=est2, std.error=se2, p.value=NA, sig.code=NA)
df.summ <- rbind(df.row1, df.row2)
df.summ$condition <- factor(df.summ$condition, levels=conditions)
dodge <- position_dodge(width=0.85)
limits <- aes(ymax=estimate+std.error, ymin=estimate-std.error)
h <- ggplot(data=df.summ, aes(x=condition, y=estimate, fill=condition)) + theme_bw()
h <- h + geom_bar(stat="identity", color="black", size=0.25, position=dodge, width=0.8) + geom_errorbar(limits, position=dodge, width=0.25, size=0.5)
h <- h + geom_hline(yintercept=0, size=0.5)
h <- h + geom_text(aes(x=1.5, y=10, label=sig.code), size=7)
if (add.labels=="yes") {
  h <- h + geom_text(aes(x=match(condition,conditions), y=estimate+std.error+0.5, label=round(estimate,3)), size=6)
}
h <- h + scale_x_discrete(name="Condition") + scale_y_continuous(name="Utterance length (# syllables/utterance)")
h <- h + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), text=element_text(size=15), axis.title=element_text(size=20, face="bold"))
png(out.fig.path5, width=200, height=200, family="Times New Roman", units="mm", res=300)
print(h)
dev.off()



df.lm <- df
df.lm$value <- df.lm$syllables.per.second
df.lm <- df.lm[!(is.na(df.lm$value)), ]
lm <- lme(value ~ condition, random=~1|talker, data=df.lm)
summ.table <- summary(lm)$tTable
est1 <- summ.table["(Intercept)", "Value"]
se1 <- summ.table["(Intercept)", "Std.Error"]
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
lm <- lme(value ~ condition, random=~1|talker, data=df.lm)
summ.table <- summary(lm)$tTable
est2 <- summ.table["(Intercept)", "Value"]
se2 <- summ.table["(Intercept)", "Std.Error"]
df.row1 <- data.frame(condition="ADS", estimate=est1, std.error=se1, p.value=pval, sig.code=sig.code)
df.row2 <- data.frame(condition="CDS", estimate=est2, std.error=se2, p.value=NA, sig.code=NA)
df.summ <- rbind(df.row1, df.row2)
df.summ$condition <- factor(df.summ$condition, levels=conditions)
dodge <- position_dodge(width=0.85)
limits <- aes(ymax=estimate+std.error, ymin=estimate-std.error)
i <- ggplot(data=df.summ, aes(x=condition, y=estimate, fill=condition)) + theme_bw()
i <- i + geom_bar(stat="identity", color="black", size=0.25, position=dodge, width=0.8) + geom_errorbar(limits, position=dodge, width=0.25, size=0.5)
i <- i + geom_hline(yintercept=0, size=0.5)
i <- i + geom_text(aes(x=1.5, y=6, label=sig.code), size=7)
if (add.labels=="yes") {
  i <- i + geom_text(aes(x=match(condition,conditions), y=estimate+std.error+0.25, label=round(estimate,3)), size=6)
}
i <- i + scale_x_discrete(name="Condition") + scale_y_continuous(name="Speech rate (# syllables/second)")
i <- i + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), text=element_text(size=15), axis.title=element_text(size=20, face="bold"))
png(out.fig.path6, width=200, height=200, family="Times New Roman", units="mm", res=300)
print(i)
dev.off()

