library(readr); library(stringr)
library(readxl); library(writexl)
library(nlme)
# library(lme4)
library(ggplot2)


file.suff <- "syllable-fusion"


dir <- paste(getwd(), "syllable-fusion", sep="/")

conditions <- c("ADS", "CDS")
frequency.groups <- c("high", "mid", "low")

in.file <- "times-words.csv"
in.dir <- dir
in.path <- paste(in.dir, in.file, sep="/")

ref.file <- paste("items-", file.suff, ".txt", sep="")
ref.dir <- dir
ref.path <- paste(ref.dir, ref.file, sep="/")

out.file <- paste("data-", file.suff, ".txt", sep="")
# out.file <- paste("data-", file.suff, ".xlsx", sep="")
out.dir <- dir
out.path <- paste(out.dir, out.file, sep="/")

out.file2 <- paste("data", file.suff, "by-item.txt", sep="-")
out.path2 <- paste(out.dir, out.file2, sep="/")

out.file3 <- paste("data", file.suff, "by-talker-and-item.txt", sep="-")
out.path3 <- paste(out.dir, out.file3, sep="/")

out.file4 <- paste("LMER-results-", file.suff, ".txt", sep="")
out.path4 <- paste(out.dir, out.file4, sep="/")

out.file5 <- paste("LMER-estimates-", file.suff, ".txt", sep="")
out.path5 <- paste(out.dir, out.file5, sep="/")

out.file6 <- paste("LMER-", file.suff, "-random-effects-talker.txt", sep="")
out.path6 <- paste(out.dir, out.file5, sep="/")

out.file6 <- paste("LMER-", file.suff, "-random-effects-item.txt", sep="")
out.path6 <- paste(out.dir, out.file6, sep="/")

fig.file <- paste("barchart-rates-",file.suff,".png",sep="")
fig.path <- paste(out.dir, fig.file, sep="/")

fig.file2 <- paste("barchart-LMER-estimates-",file.suff,".png",sep="")
fig.path2 <- paste(out.dir, fig.file2, sep="/")

dat <- read_csv(in.path, col_names=T)
list <- read_tsv(ref.path, col_names=T)

if (!(file.exists(out.path))) {
  df <- data.frame(matrix(nrow=0, ncol=11))
  names(df) <- c("frequency.group", "word", "frequency", "jyutping", "ipa.citation", "ipa.actual", "recording", "talker", "condition", "word.no", "phonetic.match")
  for (q in 1:nrow(list)) {
    row.list <- list[q, ]
    freq.grp <- row.list$FreqGrp
    wd <- as.character(row.list$Word)
    freq <- row.list$Frequency
    
    for (r in 1:nrow(dat)) {
      row.dat <- dat[r, ]
      dat.wd <- row.dat$chinese
      if (dat.wd==wd) {
        rec <- as.character(row.dat$recording)
        talker <- as.character(row.dat$session)
        cond <- as.character(row.dat$condition)
        task <- substring(rec, 5, nchar(rec))
        jp <- as.character(row.dat$jyutping)
        ipa.cit <- as.character(row.dat$ipa.citation)
        ipa.act <- as.character(row.dat$ipa.actual)
        wd.no <- row.dat$word.no
        if (ipa.cit==ipa.act) {
          phon.match <- 1
        } else {
          phon.match <- 0
        }
        df.row <- data.frame(frequency.group=freq.grp, word=wd, frequency=freq, jyutping=jp, ipa.citation=ipa.cit, ipa.actual=ipa.act, recording=rec, talker=talker, condition=cond, word.no=wd.no, phonetic.match=phon.match)
        df <- rbind(df, df.row)
      }
    }
  }
  write_tsv(df, path=out.path)
} else {
  df <- read_tsv(out.path, col_names=T)
}




words <- unique(df$word)
conditions <- unique(df$condition)
for (wd in words) {
  dat.wd <- df[df$word==wd, ]
  jp <- unique(dat.wd$jyutping)
  label <- paste(wd, jp)
  freq.grp <- as.character(unique(dat.wd$frequency.group))
  for (cond in conditions) {
    set <- dat.wd[dat.wd$condition==cond, ]
    matching <- set[set$phonetic.match==1, ]
    nonmatching <- set[set$phonetic.match==0, ]
    no.matching <- nrow(matching)
    no.nonmatching <- nrow(nonmatching)
    no.total <- nrow(set)
    if (no.total==0) {
      percent.diff <- NA
    } else {
      percent.diff <- 100*(no.matching/no.total)
    }
    alt.forms <- paste(unique(nonmatching$ipa.actual), collapse="/")
    summ.row <- data.frame(word=wd, jyutping=jp, frequency.group=freq.grp, condition=cond, percent.different=percent.diff, alternate.forms=alt.forms)
    if (wd==words[1] & cond==conditions[1]) {
      summ <- summ.row
    } else {
      summ <- rbind(summ, summ.row)
    }
  }
}
write_tsv(summ, out.path2, col_names=T)
p <- ggplot(data=summ, aes(x=word, y=percent.different, fill=condition)) + theme_bw() + facet_grid(.~frequency.group,scales="free_x")
p <- p + geom_hline(yintercept=seq(0,100,by=25), size=0.25, linetype=3)
p <- p + geom_bar(stat="identity", positio="dodge", size=0.5, color="black")
p <- p + geom_hline(yintercept=c(0,100), size=0.5)
png(fig.path, width=400, height=200, family="Times New Roman", units="mm", res=300)
print(p)
dev.off()




talkers <- str_sort(unique(df$talker))
words <- unique(df$word)
summ2 <- data.frame(matrix(nrow=0, ncol=6))
names(summ2) <- c("talker", "word", "jyutping", "frequency.group", "condition", "percent.different")
for (t in talkers) {
  for (wd in words) {
    dat.wd <- df[df$talker==t & df$word==wd, ]
    if (nrow(dat.wd)>0) {
      jp <- unique(dat.wd$jyutping)
      label <- paste(wd, jp)
      freq <- unique(dat.wd$frequency.group)
      wd.conds <- unique(dat.wd$condition)
      for (cond in conditions) {
        set <- dat.wd[dat.wd$condition==cond, ]
        matching <- set[set$phonetic.match==1, ]
        nonmatching <- set[set$phonetic.match==0, ]
        no.matching <- nrow(matching)
        no.nonmatching <- nrow(nonmatching)
        no.total <- nrow(set)
        if (no.total>0) {
          percent.diff <- 100*(no.matching/no.total)
          alt.forms <- paste(unique(nonmatching$ipa.actual), collapse="/")
          summ2.row <- data.frame(talker=t, word=wd, jyutping=jp, frequency.group=freq.grp, condition=cond, percent.different=percent.diff)
          summ2 <- rbind(summ2, summ2.row)
        }
      }
    }
  }
}
write_tsv(summ2, out.path3, col_names=T)




df.lm <- summ2
df.lm$talker <- factor(df.lm$talker, levels=talkers)
lm <- lme(percent.different ~ condition, random=list(talker=~1+condition, word=~1+condition), data=df.lm)
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
lm <- lme(percent.different ~ condition, random=list(talker=~1, word=~1), data=df.lm)
summ.table <- summary(lm)$tTable
est2 <- summ.table["(Intercept)", "Value"]
se2 <- summ.table["(Intercept)", "Std.Error"]
df2 <- summ.table["(Intercept)", "DF"]
df.row1 <- data.frame(condition="ADS", estimate=est1, std.error=se1, df=df2, p.value=pval, sig.code=sig.code)
df.row2 <- data.frame(condition="CDS", estimate=est2, std.error=se2, df=df2, p.value=NA, sig.code=NA)
df.summ <- rbind(df.row1, df.row2)
df.summ$condition <- factor(df.summ$condition, levels=conditions)
write_tsv(df.summ, out.path5)

dodge <- position_dodge(width=0.85)
limits <- aes(ymax=estimate+std.error, ymin=estimate-std.error)
k <- ggplot(data=df.summ, aes(x=condition, y=estimate, fill=condition)) + theme_bw()
k <- k + geom_hline(yintercept=seq(0,100,by=25), linetype=3, size=0.25)
k <- k + geom_bar(stat="identity", color="black", size=0.25, position=dodge, width=0.8) + geom_errorbar(limits, position=dodge, width=0.25, size=0.5)
k <- k + geom_hline(yintercept=0, size=0.5)
k <- k + geom_text(aes(x=1.5, y=35, label=sig.code), size=7)
if (add.labels=="yes") {
  k <- k + geom_text(aes(x=match(condition,conditions), y=estimate+std.error+100, label=round(estimate,0)), size=6)
}
k <- k + scale_x_discrete(name="Condition") + scale_y_continuous(name="Fusion Rate (%)")
k <- k + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), text=element_text(size=15), axis.title=element_text(size=20, face="bold"))
png(fig.path2, width=200, height=200, family="Times New Roman", units="mm", res=300)
print(k)
dev.off()




df.lm <- df
for (freq in frequency.groups) {
  freq.order <- c(freq, frequency.groups[!(frequency.groups==freq)])
  df.lm$frequency.group <- factor(df.lm$frequency.group, levels=freq.order)
  for (cond in conditions) {
    cond.order <- c(cond, conditions[!(conditions==cond)])
    df.lm$condition <- factor(df.lm$condition, levels=cond.order)
    model <- glm(phonetic.match ~ condition*frequency.group, data=df.lm)
    coeffs <- summary(model)$coeff
    # write_tsv(data.frame(summ.table), out.path4)
    est <- coeffs["(Intercept)", "Estimate"]*100
    se <- coeffs["(Intercept)", "Std. Error"]*100
    if (cond==conditions[1]) {
      pval <- coeffs["conditionCDS", "Pr(>|t|)"]
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
    } else {
      pval <- NA
      sig.code <- NA
    }
    df.row <- data.frame(frequency.group=freq, condition=cond, estimate=est, std.error=se, p.value=pval, sig.code=sig.code)
    if (freq==frequency.groups[1] & cond==conditions[1]) {
      summ2 <- df.row
    } else {
      summ2 <- rbind(summ2, df.row)
    }
  }
}

dodge <- position_dodge(width=0.85)
limits <- aes(ymax=estimate+std.error, ymin=estimate-std.error)
k <- ggplot(data=summ2, aes(x=condition, y=estimate, fill=condition)) + theme_bw() + facet_wrap(~frequency.group)
k <- k + geom_hline(yintercept=seq(0,100,by=25), linetype=3, size=0.25)
k <- k + geom_bar(stat="identity", color="black", size=0.25, position=dodge, width=0.8) + geom_errorbar(limits, position=dodge, width=0.25, size=0.5)
k <- k + geom_hline(yintercept=0, size=0.5)
k <- k + geom_text(aes(x=1.5, y=90, label=sig.code), size=6)
k <- k + scale_x_discrete(name="Condition") + scale_y_continuous(name="Fusion Rate (%)")
k <- k + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), text=element_text(size=15), axis.title=element_text(size=20, face="bold"))
png(fig.path2, width=200, height=200, family="Times New Roman", units="mm", res=300)
print(k)
dev.off()

