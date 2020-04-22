# First analysis
library(chron)
library(ggplot2)

set.seed(12345)

# 1. The 'coincidence' defences
# 42 trials of 5 seconds each, probability of a cough in 5-second interval is 0.063
# =(19/25)/12 = 0.76/12 ~ 0.0633
# We run 10,000 simulations
results <- c(); for (i in 1:10000){ results <- c(results, sum(rpois(42, 0.063)))}

png('coughs-random-simulation.png')
hist(results, col='black', xlab='Coughs during 42 x 5-second-periods after correct answers', ylab='Number of simulations', main='Simulations assuming random independent coughs', breaks=seq(0, 12, 0.25))
dev.off()

# Now assume that half of all coughs Whittock
# So coughing rate now goes up by ~ factor of 5 (192 coughs total, we were assuming 19 previously)
results <- c(); for (i in 1:10000){ results <- c(results, sum(rpois(42, 0.063*5)))}
png('coughs-random-simulation-half-all-coughs.png')
hist(results, col='black', xlab='Coughs during 42 x 5-second-periods after correct answers', ylab='Number of simulations', main='Simulations assuming half of all coughs are Whittock', breaks=seq(0, 32, 1))
abline(v=19, col='red', lw=2) # Add line for 19 coughs
dev.off()


# 2. Methodology
# Read in cough dataset
# (when coughs happened in the episode)
coughs <- read.csv('millionaire-coughs.csv', 
                   header=T,
                   stringsAsFactors = FALSE)
coughs$time.minutes <- as.numeric(chron(times=coughs$video.time))*24*60
# Information on who coughed (probable for "Phantom")
# Not actually used in analysis, but can be fun to look at. 
coughs$cougher <- ordered(coughs$cougher,
                          levels=c("Unknown", "Diana", "Phantom"))

# Read in answer dataset
# (when an answer was said in the episode)
questions.answers <- read.csv('millionaire-answers.csv',
                              header=T,
                              stringsAsFactors = F)
questions.answers$time.minutes <- as.numeric(chron(times=questions.answers$video.time))*24*60
questions.answers$speaker.short <- ifelse(questions.answers$speaker=="Tarrant", "T", "")


# Strip labeller for plotting
facet.labels <- c("Q8: £8,000\nWho was the second husband of Jacqueline Kennedy?",
                  "Q9: £16,000\nEmmenthal is a cheese from which country?",
                  "Q10: £32,000\nWho had a hit UK album with Born To Do It, released in 2000?",
                  "Q11: £64,000\nGentlemen versus Players was an annual match between amateurs and professionals of which sport?",
                  "Q12: £125,000\nThe Ambassadors in the National Gallery is a painting by which artist?",
                  "Q13: £250,000\nWhat type of garment is an Anthony Eden?",
                  "Q14: £500,000\nBaron Haussman is best known for the planning of which city?",
                  "Q15: £1,000,000\nA number one followed by 100 zeroes is known by what name?")
names(facet.labels) <- as.character(seq(8, 15))
facet.labels <- as_labeller(facet.labels)
facet_labeller <- function(facet){
  return(as.character(facet.labels[facet]))
}

# Function to plot the distribution of coughs and answers for a given questino
plotQuestion <- function(question_number){
  subset.question <- questions.answers[which(questions.answers$question==question_number),]
  subset.question$answer <- ordered(subset.question$answer, 
                                    levels=sort(unique(subset.question$answer), 
                                                decreasing=TRUE))
  ggplot(subset.question, aes(answer, time.minutes, colour=correct))+
    geom_point(shape="|", size=5)+
    theme(axis.text.x=element_text(hjust=1))+
    coord_flip()+
    theme_bw()+
    scale_color_manual(values=c("red", "black"))+
    facet_grid(~question, scales="free", labeller = facet.labels)+
    xlab("")+
    theme(panel.grid = element_blank())+
    ylab("Time through episode (minutes)")+
    theme(axis.text=element_text(colour="black"))+
    geom_rect(data=coughs[which(coughs$question==question_number),], aes(x=0, xmin=0, xmax=5, ymin=time.minutes, ymax=time.minutes+1/60), colour="white", alpha=0.5)+
    geom_point(shape="|", size=5, fill="grey")+
    guides(colour=FALSE)+
    labs(fill="Cougher")+
    geom_text(aes(label=speaker.short), size=3, position=position_nudge(0.15, 1/60), colour="black")
}

# Make the plots for Questions 8 through 15
for (i in seq(8, 15, 1)){
  ggsave(file=paste0('Question-', i, '.png'), 
         width=8, height=4,
         plotQuestion(i))
}

# 4. Putting it together

# Function to calculate smallest distance to an answer (defined as positive 
# because cough is meant to happen after the answer)
# Some careful/ugly handling of different cases
minpositive = function(x) {
  if (length(table(x>0))==2){
    return(min(x[x > 0]))
  }
  else{
    if (names(table(x>0))=="TRUE"){
      return(min(x))
    }
    else{
      return(NA)
    }
  }
}


# Calculate distances for correct answers
distances.correct.answers <- data.frame(time=sapply(coughs$time.minutes[which(coughs$time.minutes>min(questions.answers$time.minutes))], 
                                                    function(x) minpositive(x- questions.answers$time.minutes[which(questions.answers$correct=="Correct")])*60))
distances.correct.answers$correct <- "Correct"

# Calculate distances for incorrect answers
distances.incorrect.answers <- data.frame(time=sapply(coughs$time.minutes[which(coughs$time.minutes>min(questions.answers$time.minutes))], 
                                                      function(x) minpositive(x- questions.answers$time.minutes[which(questions.answers$correct=="Incorrect")])*60))
distances.incorrect.answers$correct <- "Incorrect"

# Combine dataset and plot
distances.answers <- rbind(distances.correct.answers, distances.incorrect.answers)
distances.answers$correct <- ordered(distances.answers$correct, 
                                     levels=c("Incorrect", "Correct"))
histogram.plot <- ggplot(distances.answers, aes(time, fill=correct))+
  geom_histogram(aes(y = ..density..*100), binwidth=2)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  facet_wrap(~correct, nrow=2)+
  ylab("Percentage of coughs")+
  xlab("Time elapsed since last answer was said (seconds)")+
  scale_fill_manual(values=c("black", "red"))
ggsave(file='histogram-distance-coughs-to-answers.png', 
       width=8, height=4,
       histogram.plot)

# Looking at the proportion for randomly distributed coughs
random.proportion.less.five <- c()
for (i in 1:1000){
  print(i)
  random.moments <- runif(length(questions.answers$time.minutes[which(questions.answers$correct=="Incorrect")]), 
                          min=min(questions.answers$time.minutes[which(questions.answers$correct=="Incorrect")]), max = max(questions.answers$time.minutes[which(questions.answers$correct=="Incorrect")]))
  distances.random <- data.frame(time=sapply(coughs$time.minutes[which(coughs$time.minutes>min(questions.answers$time.minutes))], 
                                             function(x) minpositive(x-random.moments))*60)
  distances.random <- distances.random$time[which(!is.na(distances.random$time))] 
  random.proportion.less.five <-c(random.proportion.less.five, length(distances.random[which(distances.random<5)])/length(distances.random) * 100)
}
random.proportion.less.five.df <- data.frame(prop=random.proportion.less.five)
ggplot(random.proportion.less.five.df, aes(prop))+
  geom_histogram(fill='black', aes(y=..density..))+
  theme_bw()
hist(random.proportion.less.five, breaks=seq(0,50,3), col='black')

