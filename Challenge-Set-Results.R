library(tidyverse)
library(dplyr)

challenge_set <- read.csv("DOAN_2020-08-11_Sentences.csv")

### overall correct ###
results_df <- data.frame("System" = unique(challenge_set$System), 
                         "Number of correct" = c(SMT_correct <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                                                                            challenge_set$Code == 1)),
                                                 CNN_correct <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                                                                            challenge_set$Code == 1)),
                                                 RNN_correct <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                                                                            challenge_set$Code == 1)),
                                                 Att_correct <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                                                                            challenge_set$Code == 1)),
                                                 Google_correct <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                                                                               challenge_set$Code == 1)),
                                                 DeepL_correct <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                                                                              challenge_set$Code == 1))),
                         "Perc correct" = c(SMT_perc <- (SMT_correct/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT"))))*100,
                                            CNN_perc <- (CNN_correct/(nrow(filter(challenge_set,challenge_set$System == "CNN"))))*100,
                                            RNN_perc <- (RNN_correct/(nrow(filter(challenge_set,challenge_set$System == "RNN"))))*100,
                                            Att_perc <- (Att_correct/(nrow(filter(challenge_set,challenge_set$System == "Attention"))))*100,
                                            Google_perc <- (Google_correct/(nrow(filter(challenge_set,challenge_set$System == "Google"))))*100,
                                            DeepL_perc <- (DeepL_correct/(nrow(filter(challenge_set,challenge_set$System == "DeepL"))))*100),
                         "Correct short" = c(SMT_short <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                                                                      challenge_set$Short_Long == "S" &
                                                                      challenge_set$Code == 1)),
                                            RNN_short <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                                                                     challenge_set$Short_Long == "S" &
                                                                     challenge_set$Code == 1)),
                                            CNN_short <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                                                                     challenge_set$Short_Long == "S" &
                                                                     challenge_set$Code == 1)),
                                            Att_short <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                                                                     challenge_set$Short_Long == "S" &
                                                                     challenge_set$Code == 1)),
                                            Google_short <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                                                                        challenge_set$Short_Long == "S" &
                                                                        challenge_set$Code == 1)),
                                            DeepL_short <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                                                                       challenge_set$Short_Long == "S" &
                                                                       challenge_set$Code == 1))),
                         "Perc correct short" = c(SMT_perc_S <- (SMT_short/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT"))))*100,
                                                  RNN_perc_S <- (RNN_short/(nrow(filter(challenge_set,challenge_set$System == "RNN"))))*100,
                                                  CNN_perc_S <- (CNN_short/(nrow(filter(challenge_set,challenge_set$System == "CNN"))))*100,
                                                  Att_perc_S <- (Att_short/(nrow(filter(challenge_set,challenge_set$System == "Attention"))))*100,
                                                  Google_perc_S <- (Google_short/(nrow(filter(challenge_set,challenge_set$System == "Google"))))*100,
                                                  DeepL_perc_S <- (DeepL_short/(nrow(filter(challenge_set,challenge_set$System == "DeepL"))))*100),
                         "Correct long" = c(SMT_long <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                                                                    challenge_set$Short_Long == "L" &
                                                                    challenge_set$Code == 1)),
                                            RNN_long <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                                                                    challenge_set$Short_Long == "L" &
                                                                    challenge_set$Code == 1)),
                                            CNN_long <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                                                                    challenge_set$Short_Long == "L" &
                                                                    challenge_set$Code == 1)),
                                            Att_long <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                                                                    challenge_set$Short_Long == "L" &
                                                                    challenge_set$Code == 1)),
                                            Google_long <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                                                                       challenge_set$Short_Long == "L" &
                                                                       challenge_set$Code == 1)),
                                            DeepL_long <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                                                                      challenge_set$Short_Long == "L" &
                                                                      challenge_set$Code == 1))),
                         "Perc correct long" = c(SMT_perc_L <- (SMT_long/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT"))))*100,
                                                 RNN_perc_L <- (RNN_long/(nrow(filter(challenge_set,challenge_set$System == "RNN"))))*100,
                                                 CNN_perc_L <- (CNN_long/(nrow(filter(challenge_set,challenge_set$System == "CNN"))))*100,
                                                 Att_perc_L <- (Att_long/(nrow(filter(challenge_set, challenge_set$System == "Attention"))))*100,
                                                 Google_perc_L <- (Google_long/(nrow(filter(challenge_set,challenge_set$System == "Google"))))*100,
                                                 DeepL_perc_L <- (DeepL_long/(nrow(filter(challenge_set,challenge_set$System == "DeepL"))))*100),
                         "Correct lexical" = c(SMT_lex <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                                                                      challenge_set$Difficulty == "Lexical" &
                                                                      challenge_set$Code == 1)),
                                               CNN_lex <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                                                                      challenge_set$Difficulty == "Lexical" &
                                                                      challenge_set$Code == 1)),
                                               RNN_lex <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                                                                      challenge_set$Difficulty == "Lexical" &
                                                                      challenge_set$Code == 1)),
                                               Att_lex <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                                                                      challenge_set$Difficulty == "Lexical" &
                                                                      challenge_set$Code == 1)),
                                               Google_lex <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                                                                         challenge_set$Difficulty == "Lexical" &
                                                                         challenge_set$Code == 1)),
                                               DeepL_lex <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                                                                        challenge_set$Difficulty == "Lexical" &
                                                                        challenge_set$Code == 1))),
                         "Perc correct lexical" = c(SMT_perc_lex <- (SMT_lex/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT" &
                                                                                          challenge_set$Difficulty == "Lexical"))))*100,
                                                    CNN_perc_lex <- (CNN_lex/(nrow(filter(challenge_set,challenge_set$System == "CNN" &
                                                                                          challenge_set$Difficulty == "Lexical"))))*100,
                                                    RNN_perc_lex <- (RNN_lex/(nrow(filter(challenge_set,challenge_set$System == "RNN" &
                                                                                          challenge_set$Difficulty == "Lexical"))))*100,
                                                    Att_perc_lex <- (Att_lex/(nrow(filter(challenge_set,challenge_set$System == "Attention" &
                                                                                          challenge_set$Difficulty == "Lexical"))))*100,
                                                    Google_perc_lex <- (Google_lex/(nrow(filter(challenge_set,challenge_set$System == "Google" &
                                                                                                challenge_set$Difficulty == "Lexical"))))*100,
                                                    DeepL_perc_lex <- (DeepL_lex/(nrow(filter(challenge_set,challenge_set$System == "DeepL" &
                                                                                              challenge_set$Difficulty == "Lexical"))))*100),
                         "Correct syntactic" = c(SMT_syn <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                                                                        challenge_set$Difficulty == "Syntactic" &
                                                                        challenge_set$Code == 1)),
                                                 CNN_syn <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                                                                        challenge_set$Difficulty == "Syntactic" &
                                                                        challenge_set$Code == 1)),
                                                 RNN_syn <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                                                                        challenge_set$Difficulty == "Syntactic" &
                                                                        challenge_set$Code == 1)),
                                                 Att_syn <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                                                                        challenge_set$Difficulty == "Syntactic" &
                                                                        challenge_set$Code == 1)),
                                                 Google_syn <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                                                                           challenge_set$Difficulty == "Syntactic" &
                                                                           challenge_set$Code == 1)),
                                                 DeepL_syn <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                                                                          challenge_set$Difficulty == "Syntactic" &
                                                                          challenge_set$Code == 1))),
                         "Perc correct syntactic" = c(SMT_perc_syn <- (SMT_syn/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT" &
                                                                                            challenge_set$Difficulty == "Syntactic"))))*100,
                                                      CNN_perc_syn <- (CNN_syn/(nrow(filter(challenge_set,challenge_set$System == "CNN" &
                                                                                            challenge_set$Difficulty == "Syntactic"))))*100,
                                                      RNN_perc_syn <- (RNN_syn/(nrow(filter(challenge_set,challenge_set$System == "RNN" &
                                                                                            challenge_set$Difficulty == "Syntactic"))))*100,
                                                      Att_perc_syn <- (Att_syn/(nrow(filter(challenge_set,challenge_set$System == "Attention" &
                                                                                            challenge_set$Difficulty == "Syntactic"))))*100,
                                                      Google_perc_syn <- (Google_syn/(nrow(filter(challenge_set,challenge_set$System == "Google" &
                                                                                                  challenge_set$Difficulty == "Syntactic"))))*100,
                                                      DeepL_perc_syn <- (DeepL_syn/(nrow(filter(challenge_set,challenge_set$System == "DeepL" &
                                                                                                challenge_set$Difficulty == "Syntactic"))))*100),
                         "Correct lexical ambiguities" = c(SMT_lex_amb <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                                                                                challenge_set$Difficulty == "Lexical" &
                                                                                challenge_set$Classification == "Ambiguity" &
                                                                                challenge_set$Code == 1)),
                                                   CNN_lex_amb <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                                                                                challenge_set$Difficulty == "Lexical" &
                                                                                challenge_set$Classification == "Ambiguity" &
                                                                                challenge_set$Code == 1)),
                                                   RNN_lex_amb <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                                                                                challenge_set$Difficulty == "Lexical" &
                                                                                challenge_set$Classification == "Ambiguity" &
                                                                                challenge_set$Code == 1)),
                                                   Att_lex_amb <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                                                                                challenge_set$Difficulty == "Lexical" &
                                                                                challenge_set$Classification == "Ambiguity" &
                                                                                challenge_set$Code == 1)),
                                                   Google_lex_amb <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                                                                                   challenge_set$Difficulty == "Lexical" &
                                                                                   challenge_set$Classification == "Ambiguity" &
                                                                                   challenge_set$Code == 1)),
                                                   DeepL_lex_amb <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                                                                                  challenge_set$Difficulty == "Lexical" &
                                                                                  challenge_set$Classification == "Ambiguity" &
                                                                                  challenge_set$Code == 1))),
                         "Perc correct lexical ambiguities" = c(SMT_perc_lex_amb <- (SMT_lex_amb/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT" &
                                                                                                             challenge_set$Difficulty == "Lexical" &
                                                                                                             challenge_set$Classification == "Ambiguity"))))*100,
                                                             CNN_perc_lex_amb <- (CNN_lex_amb/(nrow(filter(challenge_set,challenge_set$System == "CNN" &
                                                                                                             challenge_set$Difficulty == "Lexical" &
                                                                                                             challenge_set$Classification == "Ambiguity"))))*100,
                                                             RNN_perc_lex_amb <- (RNN_lex_amb/(nrow(filter(challenge_set,challenge_set$System == "RNN" &
                                                                                                             challenge_set$Difficulty == "Lexical" &
                                                                                                             challenge_set$Classification == "Ambiguity"))))*100,
                                                             Att_perc_lex_amb <- (Att_lex_amb/(nrow(filter(challenge_set,challenge_set$System == "Attention" &
                                                                                                             challenge_set$Difficulty == "Lexical" &
                                                                                                             challenge_set$Classification == "Ambiguity"))))*100,
                                                             Google_perc_lex_amb <- (Google_lex_amb/(nrow(filter(challenge_set,challenge_set$System == "Google" &
                                                                                                                   challenge_set$Difficulty == "Lexical" &
                                                                                                                   challenge_set$Classification == "Ambiguity"))))*100,
                                                             DeepL_perc_lex_amb <- (DeepL_lex_amb/(nrow(filter(challenge_set,challenge_set$System == "DeepL" &
                                                                                                                 challenge_set$Difficulty == "Lexical" &
                                                                                                                 challenge_set$Classification == "Ambiguity"))))*100),
                         "Correct homographs" = c(SMT_homograph <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                                                                                 challenge_set$Difficulty == "Lexical" &
                                                                                 challenge_set$Classification == "Homographs" &
                                                                                 challenge_set$Code == 1)),
                                                  CNN_homograph <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                                                                                 challenge_set$Difficulty == "Lexical" &
                                                                                 challenge_set$Classification == "Homographs" &
                                                                                 challenge_set$Code == 1)),
                                                  RNN_homograph <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                                                                                 challenge_set$Difficulty == "Lexical" &
                                                                                 challenge_set$Classification == "Homographs" &
                                                                                 challenge_set$Code == 1)),
                                                  Att_homograph <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                                                                                 challenge_set$Difficulty == "Lexical" &
                                                                                 challenge_set$Classification == "Homographs" &
                                                                                 challenge_set$Code == 1)),
                                                  Google_homograph <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                                                                                    challenge_set$Difficulty == "Lexical" &
                                                                                    challenge_set$Classification == "Homographs" &
                                                                                    challenge_set$Code == 1)),
                                                  DeepL_homograph <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                                                                                   challenge_set$Difficulty == "Lexical" &
                                                                                   challenge_set$Classification == "Homographs" &
                                                                                   challenge_set$Code == 1))),
                         "Perc correct homographs" = c(SMT_perc_homograph <- (SMT_homograph/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT" &
                                                                                                        challenge_set$Difficulty == "Lexical" &
                                                                                                        challenge_set$Classification == "Homographs"))))*100,
                                                    CNN_perc_homograph <- (CNN_homograph/(nrow(filter(challenge_set,challenge_set$System == "CNN" &
                                                                                                        challenge_set$Difficulty == "Lexical" &
                                                                                                        challenge_set$Classification == "Homographs"))))*100,
                                                    RNN_perc_homograph <- (RNN_homograph/(nrow(filter(challenge_set,challenge_set$System == "RNN" &
                                                                                                        challenge_set$Difficulty == "Lexical" &
                                                                                                        challenge_set$Classification == "Homographs"))))*100,
                                                    Att_perc_homograph <- (Att_homograph/(nrow(filter(challenge_set,challenge_set$System == "Attention" &
                                                                                                        challenge_set$Difficulty == "Lexical" &
                                                                                                        challenge_set$Classification == "Homographs"))))*100,
                                                    Google_perc_homograph <- (Google_homograph/(nrow(filter(challenge_set,challenge_set$System == "Google" &
                                                                                                              challenge_set$Difficulty == "Lexical" &
                                                                                                              challenge_set$Classification == "Homographs"))))*100,
                                                    DeepL_perc_homograph <- (Att_homograph/(nrow(filter(challenge_set,challenge_set$System == "DeepL" &
                                                                                                          challenge_set$Difficulty == "Lexical" &
                                                                                                          challenge_set$Classification == "Homographs"))))*100),
                         "Correct scope-related" = c(SMT_scope <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                                                                                           challenge_set$Difficulty == "Syntactic" &
                                                                                           challenge_set$Classification == "Scope" &
                                                                                           challenge_set$Code == 1)),
                                                                CNN_scope <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                                                                                           challenge_set$Difficulty == "Syntactic" &
                                                                                           challenge_set$Classification == "Scope" &
                                                                                           challenge_set$Code == 1)),
                                                                RNN_scope <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                                                                                           challenge_set$Difficulty == "Syntactic" &
                                                                                           challenge_set$Classification == "Scope" &
                                                                                           challenge_set$Code == 1)),
                                                                Att_scope <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                                                                                           challenge_set$Difficulty == "Syntactic" &
                                                                                           challenge_set$Classification == "Scope" &
                                                                                           challenge_set$Code == 1)),
                                                                Google_scope <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                                                                                              challenge_set$Difficulty == "Syntactic" &
                                                                                              challenge_set$Classification == "Scope" &
                                                                                              challenge_set$Code == 1)),
                                                                DeepL_scope <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                                                                                             challenge_set$Difficulty == "Syntactic" &
                                                                                             challenge_set$Classification == "Scope" &
                                                                                             challenge_set$Code == 1))),
                         "Perc correct scope-related" = c(SMT_perc_scope <- (SMT_scope/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT" &
                                                                                                              challenge_set$Difficulty == "Syntactic" &
                                                                                                              challenge_set$Classification == "Scope"))))*100,
                                                                  CNN_perc_scope <- (CNN_scope/(nrow(filter(challenge_set,challenge_set$System == "CNN" &
                                                                                                              challenge_set$Difficulty == "Syntactic" &
                                                                                                              challenge_set$Classification == "Scope"))))*100,
                                                                  RNN_perc_scope <- (RNN_scope/(nrow(filter(challenge_set,challenge_set$System == "RNN" &
                                                                                                              challenge_set$Difficulty == "Syntactic" &
                                                                                                              challenge_set$Classification == "Scope"))))*100,
                                                                  Att_perc_scope <- (Att_scope/(nrow(filter(challenge_set,challenge_set$System == "Attention" &
                                                                                                              challenge_set$Difficulty == "Syntactic" &
                                                                                                              challenge_set$Classification == "Scope"))))*100,
                                                                  Google_perc_scope <- (Google_scope/(nrow(filter(challenge_set,challenge_set$System == "Google" &
                                                                                                                    challenge_set$Difficulty == "Syntactic" &
                                                                                                                    challenge_set$Classification == "Scope"))))*100,
                                                                  DeepL_perc_scope <- (DeepL_scope/(nrow(filter(challenge_set,challenge_set$System == "DeepL" &
                                                                                                                  challenge_set$Difficulty == "Syntactic" &
                                                                                                                  challenge_set$Classification == "Scope"))))*100),
                         "Correct anaphora" = c(SMT_anaphora <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                                                                              challenge_set$Difficulty == "Syntactic" &
                                                                              challenge_set$Classification == "Anaphora" &
                                                                              challenge_set$Code == 1)),
                                                CNN_anaphora <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                                                                              challenge_set$Difficulty == "Syntactic" &
                                                                              challenge_set$Classification == "Anaphora" &
                                                                              challenge_set$Code == 1)),
                                                RNN_anaphora <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                                                                              challenge_set$Difficulty == "Syntactic" &
                                                                              challenge_set$Classification == "Anaphora" &
                                                                              challenge_set$Code == 1)),
                                                Att_anaphora <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                                                                              challenge_set$Difficulty == "Syntactic" &
                                                                              challenge_set$Classification == "Anaphora" &
                                                                              challenge_set$Code == 1)),
                                                Google_anaphora <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                                                                                 challenge_set$Difficulty == "Syntactic" &
                                                                                 challenge_set$Classification == "Anaphora" &
                                                                                 challenge_set$Code == 1)),
                                                
                                                DeepL_anaphora <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                                                                                challenge_set$Difficulty == "Syntactic" &
                                                                                challenge_set$Classification == "Anaphora" &
                                                                                challenge_set$Code == 1))),
                         "Perc correct anaphora" = c(SMT_perc_anaphora <- (SMT_anaphora/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT" &
                                                                                                    challenge_set$Difficulty == "Syntactic" &
                                                                                                    challenge_set$Classification == "Anaphora"))))*100,
                                                  CNN_perc_anaphora <- (CNN_anaphora/(nrow(filter(challenge_set,challenge_set$System == "CNN" &
                                                                                                    challenge_set$Difficulty == "Syntactic" &
                                                                                                    challenge_set$Classification == "Anaphora"))))*100,
                                                  RNN_perc_anaphora <- (RNN_anaphora/(nrow(filter(challenge_set,challenge_set$System == "RNN" &
                                                                                                    challenge_set$Difficulty == "Syntactic" &
                                                                                                    challenge_set$Classification == "Anaphora"))))*100,
                                                  Att_perc_anaphora <- (Att_anaphora/(nrow(filter(challenge_set,challenge_set$System == "Attention" &
                                                                                                    challenge_set$Difficulty == "Syntactic" &
                                                                                                    challenge_set$Classification == "Anaphora"))))*100,
                                                  Google_perc_anaphora <- (Google_anaphora/(nrow(filter(challenge_set,challenge_set$System == "Google" &
                                                                                                          challenge_set$Difficulty == "Syntactic" &
                                                                                                          challenge_set$Classification == "Anaphora"))))*100,
                                                  DeepL_perc_anaphora <- (DeepL_anaphora/(nrow(filter(challenge_set,challenge_set$System == "DeepL" &
                                                                                                        challenge_set$Difficulty == "Syntactic" &
                                                                                                        challenge_set$Classification == "Anaphora"))))*100),
                         "Correct anaphora with interruption" = c(SMT_anaphora <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                                                                                                challenge_set$Difficulty == "Syntactic" &
                                                                                                challenge_set$Classification == "Anaphora" &
                                                                                                challenge_set$Interrupted == "Y" &
                                                                                                challenge_set$Code == 1)),
                                                                  CNN_anaphora <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                                                                                                challenge_set$Difficulty == "Syntactic" &
                                                                                                challenge_set$Classification == "Anaphora" &
                                                                                                challenge_set$Interrupted == "Y" &
                                                                                                challenge_set$Code == 1)),
                                                                  RNN_anaphora <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                                                                                                challenge_set$Difficulty == "Syntactic" &
                                                                                                challenge_set$Classification == "Anaphora" &
                                                                                                challenge_set$Interrupted == "Y" &
                                                                                                challenge_set$Code == 1)),
                                                                  Att_anaphora <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                                                                                                challenge_set$Difficulty == "Syntactic" &
                                                                                                challenge_set$Classification == "Anaphora" &
                                                                                                challenge_set$Interrupted == "Y" &
                                                                                                challenge_set$Code == 1)),
                                                                  Google_anaphora <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                                                                                                   challenge_set$Difficulty == "Syntactic" &
                                                                                                   challenge_set$Classification == "Anaphora" &
                                                                                                   challenge_set$Interrupted == "Y" &
                                                                                                   challenge_set$Code == 1)),
                                                                  DeepL_anaphora <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                                                                                                  challenge_set$Difficulty == "Syntactic" &
                                                                                                  challenge_set$Classification == "Anaphora" &
                                                                                                  challenge_set$Interrupted == "Y" &
                                                                                                  challenge_set$Code == 1))),
                         "Perc correct anaphora with interruption" = c(SMT_perc_anaphora <- (SMT_anaphora/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT" &
                                                                                                                      challenge_set$Difficulty == "Syntactic" &
                                                                                                                      challenge_set$Classification == "Anaphora" &
                                                                                                                      challenge_set$Interrupted == "Y"))))*100,
                                                                    CNN_perc_anaphora <- (CNN_anaphora/(nrow(filter(challenge_set,challenge_set$System == "CNN" &
                                                                                                                      challenge_set$Difficulty == "Syntactic" &
                                                                                                                      challenge_set$Classification == "Anaphora" &
                                                                                                                      challenge_set$Interrupted == "Y"))))*100,
                                                                    RNN_perc_anaphora <- (RNN_anaphora/(nrow(filter(challenge_set,challenge_set$System == "RNN" &
                                                                                                                      challenge_set$Difficulty == "Syntactic" &
                                                                                                                      challenge_set$Classification == "Anaphora" &
                                                                                                                      challenge_set$Interrupted == "Y"))))*100,
                                                                    Att_perc_anaphora <- (Att_anaphora/(nrow(filter(challenge_set,challenge_set$System == "Attention" &
                                                                                                                      challenge_set$Difficulty == "Syntactic" &
                                                                                                                      challenge_set$Classification == "Anaphora" &
                                                                                                                      challenge_set$Interrupted == "Y"))))*100,
                                                                    Google_perc_anaphora <- (Google_anaphora/(nrow(filter(challenge_set,challenge_set$System == "Google" &
                                                                                                                            challenge_set$Difficulty == "Syntactic" &
                                                                                                                            challenge_set$Classification == "Anaphora" &
                                                                                                                            challenge_set$Interrupted == "Y"))))*100,
                                                                    DeepL_perc_anaphora <- (DeepL_anaphora/(nrow(filter(challenge_set,challenge_set$System == "DeepL" &
                                                                                                                          challenge_set$Difficulty == "Syntactic" &
                                                                                                                          challenge_set$Classification == "Anaphora" &
                                                                                                                          challenge_set$Interrupted == "Y"))))*100),
                         "Correct anaphora without interruption" = c(SMT_anaphora <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                                                                                                   challenge_set$Difficulty == "Syntactic" &
                                                                                                   challenge_set$Classification == "Anaphora" &
                                                                                                   challenge_set$Interrupted == "N" &
                                                                                                   challenge_set$Code == 1)),
                                                                     CNN_anaphora <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                                                                                                   challenge_set$Difficulty == "Syntactic" &
                                                                                                   challenge_set$Classification == "Anaphora" &
                                                                                                   challenge_set$Interrupted == "N" &
                                                                                                   challenge_set$Code == 1)),
                                                                     RNN_anaphora <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                                                                                                   challenge_set$Difficulty == "Syntactic" &
                                                                                                   challenge_set$Classification == "Anaphora" &
                                                                                                   challenge_set$Interrupted == "N" &
                                                                                                   challenge_set$Code == 1)),
                                                                     Att_anaphora <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                                                                                                   challenge_set$Difficulty == "Syntactic" &
                                                                                                   challenge_set$Classification == "Anaphora" &
                                                                                                   challenge_set$Interrupted == "N" &
                                                                                                   challenge_set$Code == 1)),
                                                                     Google_anaphora <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                                                                                                      challenge_set$Difficulty == "Syntactic" &
                                                                                                      challenge_set$Classification == "Anaphora" &
                                                                                                      challenge_set$Interrupted == "N" &
                                                                                                      challenge_set$Code == 1)),
                                                                     DeepL_anaphora <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                                                                                                     challenge_set$Difficulty == "Syntactic" &
                                                                                                     challenge_set$Classification == "Anaphora" &
                                                                                                     challenge_set$Interrupted == "N" &
                                                                                                     challenge_set$Code == 1))),
                         "Perc correct anaphora without interruption" = c(SMT_perc_anaphora <- (SMT_anaphora/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT" &
                                                                                                                         challenge_set$Difficulty == "Syntactic" &
                                                                                                                         challenge_set$Classification == "Anaphora" &
                                                                                                                         challenge_set$Interrupted == "N"))))*100,
                                                                       CNN_perc_anaphora <- (CNN_anaphora/(nrow(filter(challenge_set,challenge_set$System == "CNN" &
                                                                                                                         challenge_set$Difficulty == "Syntactic" &
                                                                                                                         challenge_set$Classification == "Anaphora" &
                                                                                                                         challenge_set$Interrupted == "N"))))*100,
                                                                       RNN_perc_anaphora <- (RNN_anaphora/(nrow(filter(challenge_set,challenge_set$System == "RNN" &
                                                                                                                         challenge_set$Difficulty == "Syntactic" &
                                                                                                                         challenge_set$Classification == "Anaphora" &
                                                                                                                         challenge_set$Interrupted == "N"))))*100,
                                                                       Att_perc_anaphora <- (Att_anaphora/(nrow(filter(challenge_set,challenge_set$System == "Attention" &
                                                                                                                         challenge_set$Difficulty == "Syntactic" &
                                                                                                                         challenge_set$Classification == "Anaphora" &
                                                                                                                         challenge_set$Interrupted == "N"))))*100,
                                                                       Google_perc_anaphora <- (Google_anaphora/(nrow(filter(challenge_set,challenge_set$System == "Google" &
                                                                                                                               challenge_set$Difficulty == "Syntactic" &
                                                                                                                               challenge_set$Classification == "Anaphora" &
                                                                                                                               challenge_set$Interrupted == "N"))))*100,
                                                                       DeepL_perc_anaphora <- (DeepL_anaphora/(nrow(filter(challenge_set,challenge_set$System == "DeepL" &
                                                                                                                             challenge_set$Difficulty == "Syntactic" &
                                                                                                                             challenge_set$Classification == "Anaphora" &
                                                                                                                             challenge_set$Interrupted == "N"))))*100),
                         "Correct ambiguities at beginning" = c(SMT_amb_before <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                                                                                                challenge_set$Classification == "Ambiguity" &
                                                                                                challenge_set$Location == "Before" &
                                                                                                challenge_set$Code == 1)),
                                                                CNN_amb_before <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                                                                                                challenge_set$Classification == "Ambiguity" &
                                                                                                challenge_set$Location == "Before" &
                                                                                                challenge_set$Code == 1)),
                                                                RNN_amb_before <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                                                                                                challenge_set$Classification == "Ambiguity" &
                                                                                                challenge_set$Location == "Before" &
                                                                                                challenge_set$Code == 1)),
                                                                Att_amb_before <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                                                                                                challenge_set$Classification == "Ambiguity" &
                                                                                                challenge_set$Location == "Before" &
                                                                                                challenge_set$Code == 1)),
                                                                Google_amb_before <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                                                                                                   challenge_set$Classification == "Ambiguity" &
                                                                                                   challenge_set$Location == "Before" &
                                                                                                   challenge_set$Code == 1)),
                                                                DeepL_amb_before <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                                                                                                  challenge_set$Classification == "Ambiguity" &
                                                                                                  challenge_set$Location == "Before" &
                                                                                                  challenge_set$Code == 1))),
                         "Perc correct ambiguities at beginning" = c(SMT_perc_amb_before <- (SMT_amb_before/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT" &
                                                                                                                        challenge_set$Classification == "Ambiguity" &
                                                                                                                        challenge_set$Location == "Before"))))*100,
                                                                  CNN_perc_amb_before <- (CNN_amb_before/(nrow(filter(challenge_set,challenge_set$System == "CNN" &
                                                                                                                        challenge_set$Classification == "Ambiguity" &
                                                                                                                        challenge_set$Location == "Before"))))*100,
                                                                  RNN_perc_amb_before <- (RNN_amb_before/(nrow(filter(challenge_set,challenge_set$System == "RNN" &
                                                                                                                        challenge_set$Classification == "Ambiguity" &
                                                                                                                        challenge_set$Location == "Before"))))*100,
                                                                  Att_perc_amb_before <- (Att_amb_before/(nrow(filter(challenge_set,challenge_set$System == "Attention" &
                                                                                                                        challenge_set$Classification == "Ambiguity" &
                                                                                                                        challenge_set$Location == "Before"))))*100,
                                                                  Google_perc_amb_before <- (Google_amb_before/(nrow(filter(challenge_set,challenge_set$System == "Google" &
                                                                                                                              challenge_set$Classification == "Ambiguity" &
                                                                                                                              challenge_set$Location == "Before"))))*100,
                                                                  DeepL_perc_amb_before <- (DeepL_amb_before/(nrow(filter(challenge_set,challenge_set$System == "DeepL" &
                                                                                                                            challenge_set$Classification == "Ambiguity" &
                                                                                                                            challenge_set$Location == "Before"))))*100),
                         "Correct ambiguities mid" = c(SMT_amb_mid <- nrow(filter(challenge_set, challenge_set$System == "Hybrid SMT" &
                                                                                    challenge_set$Classification == "Ambiguity" &
                                                                                    challenge_set$Location == "Mid" &
                                                                                    challenge_set$Code == 1)),
                                                       CNN_amb_mid <- nrow(filter(challenge_set, challenge_set$System == "CNN" &
                                                                                    challenge_set$Classification == "Ambiguity" &
                                                                                    challenge_set$Location == "Mid" &
                                                                                    challenge_set$Code == 1)),
                                                       RNN_amb_mid <- nrow(filter(challenge_set, challenge_set$System == "RNN" &
                                                                                    challenge_set$Classification == "Ambiguity" &
                                                                                    challenge_set$Location == "Mid" &
                                                                                    challenge_set$Code == 1)),
                                                       Att_amb_mid <- nrow(filter(challenge_set, challenge_set$System == "Attention" &
                                                                                    challenge_set$Classification == "Ambiguity" &
                                                                                    challenge_set$Location == "Mid" &
                                                                                    challenge_set$Code == 1)),
                                                       Google_amb_mid <- nrow(filter(challenge_set, challenge_set$System == "Google" &
                                                                                       challenge_set$Classification == "Ambiguity" &
                                                                                       challenge_set$Location == "Mid" &
                                                                                       challenge_set$Code == 1)),
                                                       DeepL_amb_mid <- nrow(filter(challenge_set, challenge_set$System == "DeepL" &
                                                                                      challenge_set$Classification == "Ambiguity" &
                                                                                      challenge_set$Location == "Mid" &
                                                                                      challenge_set$Code == 1))),
                         "Perc correct ambiguities mid" = c(SMT_perc_amb_mid <- (SMT_amb_mid/(nrow(filter(challenge_set,challenge_set$System == "Hybrid SMT" &
                                                                                                         challenge_set$Classification == "Ambiguity" &
                                                                                                         challenge_set$Location == "Mid"))))*100,
                                                         CNN_perc_amb_mid <- (CNN_amb_mid/(nrow(filter(challenge_set,challenge_set$System == "CNN" &
                                                                                                         challenge_set$Classification == "Ambiguity" &
                                                                                                         challenge_set$Location == "Mid"))))*100,
                                                         RNN_perc_amb_mid <- (RNN_amb_mid/(nrow(filter(challenge_set,challenge_set$System == "RNN" &
                                                                                                         challenge_set$Classification == "Ambiguity" &
                                                                                                         challenge_set$Location == "Mid"))))*100,
                                                         Att_perc_amb_mid <- (Att_amb_mid/(nrow(filter(challenge_set,challenge_set$System == "Attention" &
                                                                                                         challenge_set$Classification == "Ambiguity" &
                                                                                                         challenge_set$Location == "Mid"))))*100,
                                                         Google_perc_amb_mid <- (Google_amb_mid/(nrow(filter(challenge_set,challenge_set$System == "Google" &
                                                                                                               challenge_set$Classification == "Ambiguity" &
                                                                                                               challenge_set$Location == "Mid"))))*100,
                                                         DeepL_perc_amb_mid <- (DeepL_amb_mid/(nrow(filter(challenge_set,challenge_set$System == "DeepL" &
                                                                                                             challenge_set$Classification == "Ambiguity" &
                                                                                                             challenge_set$Location == "Mid"))))*100))


long_df = setNames(data.frame(t(results_df[,-1])), results_df[,1])

write_csv(long_df, "Results.csv")