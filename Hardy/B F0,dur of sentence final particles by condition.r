library(readr)
library(ggplot2)
library(nlme)

# add.labels <- "yes"
add.labels <- "no"


dir <- paste(getwd(), "sentence-final-particles/SFP-F0-files", sep="/")


tasks <- c("ali", "cl")
# tasks <- c("alf", "ali", "alm", "als", "cl")
# tasks <- c("alf", "ali", "alm", "als", "alw", "cl", "ch1", "ch2", "ch3", "ch4", "ch5")


monophthongs <- c("i", "ɪ", "ɛ", "y", "œ", "ɵ", "ɐ", "aː", "ɔ", "ʊ", "u")
diphthongs <- c("iu", "ei", "ɛu", "ɵy", "ɐi", "ɐu", "aːi", "aːu", "ɔi", "ou", "ui")
vowels <- c(monophthongs)

punctuations <- c(".", "?", ",", "ǃ")

conditions <- c("ADS", "CDS")


in.file1 <- "formants-vowels-monophthongs.csv"
in.file2 <- "times-utterances.csv"
in.file3 <- "times-phonemes.csv"
ref.file1 <- "list-SFP-items.txt"
ref.file2 <- "list-SFP-items-short.txt"

out.file1a <- "F0-dur-SFP-vowels.txt"
out.file1b <- "F0-dur-SFP-vowels-abbrev.txt"
out.file2a <- "F0-SFP-LMER-output.txt"
out.file2b <- "F0-SFP-LMER-estimates.txt"
out.file3a <- "dur-SFP-LMER-output.txt"
out.file3b <- "dur-SFP-LMER-estimates.txt"

out.fig.file1 <- "barchart-SFPs-F0-pooled.png"
out.fig.file2 <- "barchart-SFPs-dur-pooled.png"

in.path1 <- paste(dir, in.file1, sep="/")
in.path2 <- paste(dir, in.file2, sep="/")
in.path3 <- paste(dir, in.file3, sep="/")
ref.path1 <- paste(dir, ref.file1, sep="/")
ref.path2 <- paste(dir, ref.file2, sep="/")
out.path1a <- paste(dir, out.file1a, sep="/")
out.path1b <- paste(dir, out.file1b, sep="/")
out.path2a <- paste(dir, out.file2a, sep="/")
out.path2b <- paste(dir, out.file2b, sep="/")
out.path3a <- paste(dir, out.file3a, sep="/")
out.path3b <- paste(dir, out.file3b, sep="/")
out.fig.path1 <- paste(dir, out.fig.file1, sep="/")
out.fig.path2 <- paste(dir, out.fig.file2, sep="/")


dat.acous <- read_csv(in.path1, col_names=T)
dat.utt <- read_csv(in.path2, col_names=T)
dat.phon <- read_csv(in.path3, col_names=T)

ref.dat <- read_tsv(ref.path1, col_names=T)
# ref.dat <- read_tsv(ref.path2, col_names=T)
SFPs <- ref.dat$chinese
SFPs.jp <- ref.dat$jyutping
SFPs.ipa <- ref.dat$IPA
SFPs.lab <- ref.dat$label
if (!(file.exists(out.path1a))) {
  df.out <- data.frame(matrix(nrow=0, ncol=11))
  names(df.out) <- c("talker", "recording", "task", "condition", "SFP.chinese", "SFP.jyutping", "SFP.ipa", "F0.hz", "duration.ms", "mean.F0.utterance", "mean.syll.duration.ms")
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
      dur.utt <- t.off - t.ons
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
            dur <- (row.fin$time.offset - row.fin$time.onset)*1000
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
            phon.nos.utt <- phon.rows$phoneme.no
            acoust.rows.utt <- dat.acous[dat.acous$recording==rec & dat.acous$phoneme.no%in%phon.nos.utt & !(is.na(dat.acous$F0.hz)) & !(dat.acous$F0.hz=="--undefined--"), ]
            no.sylls <- nrow(acoust.rows.utt)
            mean.f0.utt <- mean(acoust.rows.utt$F0.hz)
            mean.syll.dur.utt <- dur.utt*1000/no.sylls
            df.row <- data.frame(talker=t, recording=rec, task=task, condition=cond, SFP.chinese=chin, SFP.jyutping=jp, SFP.ipa=ipa, F0.hz=f0, duration.ms=dur, mean.F0.utterance=mean.f0.utt, mean.syll.duration.ms=mean.syll.dur.utt)
            df.out <- rbind(df.out, df.row)
          }
        }
      }
    }
  }
  write_tsv(df.out, out.path1a, col_names=T)
  df <- df.out
} else {
  df <- read_tsv(out.path1a, col_names=T)
}


talkers <- unique(df$talker)
df <- df[!(is.na(df$F0.hz)) & !(is.na(df$mean.F0.utterance)), ]
lm <- lme(F0.hz ~ condition*mean.F0.utterance, random=list(talker=~condition, SFP.chinese=~condition), data=df)
summ.table <- summary(lm)$tTable
print(summ.table)
write_tsv(data.frame(summ.table), out.path2a)

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
df$condition <- factor(df$condition, levels=rev(conditions))
lm <- lme(F0.hz ~ condition*mean.F0.utterance, random=list(talker=~condition, SFP.chinese=~condition), data=df)
summ.table <- summary(lm)$tTable
est2 <- summ.table["(Intercept)", "Value"]
se2 <- summ.table["(Intercept)", "Std.Error"]
df <- summ.table["(Intercept)", "DF"]
df.row1 <- data.frame(condition="ADS", estimate=est1, std.error=se1, df=df1, p.value=pval, sig.code=sig.code)
df.row2 <- data.frame(condition="CDS", estimate=est2, std.error=se2, df=df, p.value=NA, sig.code=NA)
df.summ <- rbind(df.row1, df.row2)
write_tsv(df.summ, out.path2b)

df.summ$condition <- factor(df.summ$condition, levels=conditions)
dodge <- position_dodge(width=0.85)
limits <- aes(ymax=estimate+std.error, ymin=estimate-std.error)
i <- ggplot(data=df.summ, aes(x=condition, y=estimate, fill=condition)) + theme_bw()
i <- i + geom_bar(stat="identity", color="black", size=0.25, position=dodge, width=0.8) + geom_errorbar(limits, position=dodge, width=0.25, size=0.5)
i <- i + geom_hline(yintercept=0, size=0.5)
i <- i + geom_text(aes(x=1.5, y=90, label=sig.code), size=7)
if (add.labels=="yes") {
  i <- i+ geom_text(aes(x=match(condition,conditions), y=estimate+20, label=round(estimate,2)), size=6)
}
i <- i + scale_x_discrete(name="Condition") + scale_y_continuous(name="F0 estimate (Hz)")
i <- i + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), text=element_text(size=15), axis.title=element_text(size=20, face="bold"))
png(out.fig.path1, width=200, height=200, family="Times New Roman", units="mm", res=300)
print(i)
dev.off()
