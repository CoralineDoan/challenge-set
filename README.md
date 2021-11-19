# Challenge Set
Results from "Comparing Three Encoder-Decoder Architectures for Neural Machine Translation: A Challenge Set Approach" (Doan, 2021)

# Abstract
Machine translation (MT) as a field of research has known significant advances in recent years, with the increased interest for neural machine translation (NMT). By combining deep learning with translation, researchers have been able to deliver systems that perform better than most, if not all, of their predecessors. While the general consensus regarding NMT is that it renders higher-quality translations that are overall more idiomatic, researchers recognize that NMT systems still struggle to deal with certain classic difficulties, and that their performance may vary depending on their architecture. In this project, we implement a challenge-set based approach to the evaluation of examples of three main NMT architectures: convolutional neural network-based systems (CNN), recurrent neural network-based (RNN) systems, and attention-based systems, trained on the same data set for English to French translation. The challenge set focuses on a selection of lexical and syntactic difficulties (e.g., ambiguities) drawn from literature on human translation, machine translation, and writing for translation, and also includes variations in sentence lengths and structures that are recognized as sources of difficulties even for NMT systems. This set allows us to evaluate performance in multiple areas of difficulty for the systems overall, as well as to evaluate any differences between architectures’ performance. Through our challenge set, we found that our CNN-based system tends to reword sentences, sometimes shifting their meaning, while our RNN-based system seems to perform better when provided with a larger context, and our attention-based system seems to struggle the longer a sentence becomes.

# Description
Here you will find our original challenge set, as established at the beginning of the writing of this thesis, as well as a modified version that takes into account changes that we believe, after thorough analysis of our results, will improve the challenge set.
