---

library(tidyverse)
library(dplyr)

challenge_set <- read.csv("DOAN_2020-08-11_Sentences.csv")

### overall correct ###
SMT_correct <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                        challenge_set$Code == 1))
SMT_perc <- (SMT_correct/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT"))))*100

CNN_correct <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                        challenge_set$Code == 1))
CNN_perc <- (CNN_correct/(nrow(filter(challenge_set,challenge_set$System == "CNN"))))*100

RNN_correct <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                        challenge_set$Code == 1))
RNN_perc <- (RNN_correct/(nrow(filter(challenge_set,challenge_set$System == "RNN"))))*100

Att_correct <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                        challenge_set$Code == 1))
Att_perc <- (Att_correct/(nrow(filter(challenge_set,challenge_set$System == "Attention"))))*100

Google_correct <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                                challenge_set$Code == 1))
Google_perc <- (Google_correct/(nrow(filter(challenge_set,challenge_set$System == "Google"))))*100

DeepL_correct <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                                challenge_set$Code == 1))
DeepL_perc <- (DeepL_correct/(nrow(filter(challenge_set,challenge_set$System == "DeepL"))))*100

### short sentences ###
SMT_short <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                             challenge_set$Short_Long == "S" &
                             challenge_set$Code == 1))
SMT_perc_S <- (SMT_short/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT"))))*100

RNN_short <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                           challenge_set$Short_Long == "S" &
                           challenge_set$Code == 1))
RNN_perc_S <- (RNN_short/(nrow(filter(challenge_set,challenge_set$System == "RNN"))))*100

CNN_short <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                           challenge_set$Short_Long == "S" &
                           challenge_set$Code == 1))
CNN_perc_S <- (CNN_short/(nrow(filter(challenge_set,challenge_set$System == "CNN"))))*100

Att_short <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                           challenge_set$Short_Long == "S" &
                           challenge_set$Code == 1))
Att_perc_S <- (Att_short/(nrow(filter(challenge_set,challenge_set$System == "Attention"))))*100

Google_short <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                           challenge_set$Short_Long == "S" &
                           challenge_set$Code == 1))
Google_perc_S <- (Google_short/(nrow(filter(challenge_set,challenge_set$System == "Google"))))*100

DeepL_short <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                           challenge_set$Short_Long == "S" &
                           challenge_set$Code == 1))
DeepL_perc_S <- (DeepL_short/(nrow(filter(challenge_set,challenge_set$System == "DeepL"))))*100

### long sentences ###
SMT_long <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                          challenge_set$Short_Long == "L" &
                          challenge_set$Code == 1))
SMT_perc_L <- (SMT_long/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT"))))*100

RNN_long <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                          challenge_set$Short_Long == "L" &
                          challenge_set$Code == 1))
RNN_perc_L <- (RNN_long/(nrow(filter(challenge_set,challenge_set$System == "RNN"))))*100

CNN_long <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                          challenge_set$Short_Long == "L" &
                          challenge_set$Code == 1))
CNN_perc_L <- (CNN_long/(nrow(filter(challenge_set,challenge_set$System == "CNN"))))*100

Att_long <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                          challenge_set$Short_Long == "L" &
                          challenge_set$Code == 1))
Att_perc_L <- (Att_long/(nrow(filter(challenge_set, challenge_set$System == "Attention"))))*100

Google_long <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                             challenge_set$Short_Long == "L" &
                             challenge_set$Code == 1))
Google_perc_L <- (Google_long/(nrow(filter(challenge_set,challenge_set$System == "Google"))))*100

DeepL_long <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                            challenge_set$Short_Long == "L" &
                            challenge_set$Code == 1))
DeepL_perc_L <- (DeepL_long/(nrow(filter(challenge_set,challenge_set$System == "DeepL"))))*100

### lexical challenges ###
SMT_lex <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                          challenge_set$Difficulty == "Lexical" &
                          challenge_set$Code == 1))
SMT_perc_lex <- (SMT_lex/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT" &
                                        challenge_set$Difficulty == "Lexical"))))*100

CNN_lex <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                         challenge_set$Difficulty == "Lexical" &
                         challenge_set$Code == 1))
CNN_perc_lex <- (CNN_lex/(nrow(filter(challenge_set,challenge_set$System == "CNN" &
                                        challenge_set$Difficulty == "Lexical"))))*100

RNN_lex <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                         challenge_set$Difficulty == "Lexical" &
                         challenge_set$Code == 1))
RNN_perc_lex <- (RNN_lex/(nrow(filter(challenge_set,challenge_set$System == "RNN" &
                                        challenge_set$Difficulty == "Lexical"))))*100

Att_lex <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                         challenge_set$Difficulty == "Lexical" &
                         challenge_set$Code == 1))
Att_perc_lex <- (Att_lex/(nrow(filter(challenge_set,challenge_set$System == "Attention" &
                                        challenge_set$Difficulty == "Lexical"))))*100

Google_lex <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                         challenge_set$Difficulty == "Lexical" &
                         challenge_set$Code == 1))
Google_perc_lex <- (Google_lex/(nrow(filter(challenge_set,challenge_set$System == "Google" &
                                        challenge_set$Difficulty == "Lexical"))))*100

DeepL_lex <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                         challenge_set$Difficulty == "Lexical" &
                         challenge_set$Code == 1))
DeepL_perc_lex <- (DeepL_lex/(nrow(filter(challenge_set,challenge_set$System == "DeepL" &
                                        challenge_set$Difficulty == "Lexical"))))*100

### syntactic challenges ###
SMT_syn <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                         challenge_set$Difficulty == "Syntactic" &
                         challenge_set$Code == 1))
SMT_perc_syn <- (SMT_syn/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT" &
                                        challenge_set$Difficulty == "Syntactic"))))*100

CNN_syn <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                         challenge_set$Difficulty == "Syntactic" &
                         challenge_set$Code == 1))
CNN_perc_syn <- (CNN_syn/(nrow(filter(challenge_set,challenge_set$System == "CNN" &
                                        challenge_set$Difficulty == "Syntactic"))))*100

RNN_syn <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                         challenge_set$Difficulty == "Syntactic" &
                         challenge_set$Code == 1))
RNN_perc_syn <- (RNN_syn/(nrow(filter(challenge_set,challenge_set$System == "RNN" &
                                        challenge_set$Difficulty == "Syntactic"))))*100

Att_syn <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                         challenge_set$Difficulty == "Syntactic" &
                         challenge_set$Code == 1))
Att_perc_syn <- (Att_syn/(nrow(filter(challenge_set,challenge_set$System == "Attention" &
                                        challenge_set$Difficulty == "Syntactic"))))*100

Google_syn <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                            challenge_set$Difficulty == "Syntactic" &
                            challenge_set$Code == 1))
Google_perc_syn <- (Google_syn/(nrow(filter(challenge_set,challenge_set$System == "Google" &
                                              challenge_set$Difficulty == "Syntactic"))))*100

DeepL_syn <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                           challenge_set$Difficulty == "Syntactic" &
                           challenge_set$Code == 1))
DeepL_perc_syn <- (DeepL_syn/(nrow(filter(challenge_set,challenge_set$System == "DeepL" &
                                            challenge_set$Difficulty == "Syntactic"))))*100

###