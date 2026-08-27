# chatPersonalities
This repository contains the R analysis scripts for the collaboration beween OneReach(R), Computational Psychopathology Research Group, and Acadia University. This work has been published here: https://www.nature.com/articles/s44271-026-00433-8

The full reference is:
Castiello, S., Pitliya, R. J., Lametti, D. R., & Murphy, R. A. (2026). 
Affiliation in human-AI interactions is based on shared psychological traits. 
Communications Psychology, 4(1), 93. Preprint: https://osf.io/preprints/psyarxiv/mqunw_v3.


# 0. What are the R scripts in this repository?

- For Experiment 1 and 2, open <code>main.R</code>, this script uses the cleaned data from the folders <code>experiment1/</code> and <code>experiment2/</code>.
- For Experiment 3 (preregistered: https://aspredicted.org/9yg6-y3xp.pdf), open <code>main_e3.R</code>, this script uses the cleaned data from the folder <code>experiment3/</code>.
- The sample size calculation for the preregistration can be found here: <code>sample_size_calculation_exp3.R</code>, as well as the pregistration text word <code>asPredicted_v2.docx</code>.
- The script we used to validate the affiliation score can be found here: <code>affiliation_validation.R</code>.
- In folder pilot0 you will find pilot data collected before running Experiment 1. But pilot4 is the experiment we ran to validate the affiliation score, requested by Reviewer 2. 
- The main analysis scripts also call <code>functions.R</code> to scored the psychological questionnaires and process data.
- The script <code>llm_text_analysis.R</code> is only a toy script to explore future ideas.

# 2. Reproduce Figures and Analysis
- Figure 2 and Figure 3 are produced with <code>main.R</code>
- Figure 4 is produced with <code>main_e3.R</code>




# 2. Cleaning Raw Data (Gorilla counterballance)

## Experiment 1 (anxiety)

### Notes
* Chat 1 is always Anxious.
* Chat 2 is always Normal.

The names assigned to the chatbots are what is counterbalanced. In Counterbalance A, “Pat” is Chat 1 Anxious, “Alex” is Chat 2 Normal. In Counterbalance B, “Alex” is Chat 1 Anxious, “Pat” is Chat 2 Normal. The order regarding which chats are experienced first (i.e., chat 1 first or chat 2) are counterbalanced within each counterbalance-by-name. So, there are four counterbalance-groups: 

* Chat 1 Anxious “Pat” First, Chat 2 Normal “Alex” Last
* Chat 2 Normal “Alex” First, Chat 1 Anxious “Pat” Last
* Chat 1 Anxious “Alex” First, Chat 2 Normal “Pat” Last
* Chat 2 Normal “Pat” First, Chat 1 Anxious “Alex” Last



### Counterbalance A only: 
Link: https://app.gorilla.sc/admin/project/93613

N = 36

Nodes names:
-   demograhpics: 8v2c
-   psychological questionaires: 
    -   BFI10: mcfk
    -   SCL90R: 2bh6
-   bots questionnaires:
    -   A_1PatAnx_2AlexNorm: 4a4q
    -   A_2AlexNorm_1PatAnx: ewzl
-   task names:
    -   A_1PatAnx_2AlexNorm 1PatAnx:    keg8 
    -   A_1PatAnx_2AlexNorm 2AlexNorm:  mrfa
    -   A_2AlexNorm_1PatAnx 2AlexNorm:  xy27
    -   A_2AlexNorm_1PatAnx 1PatAnx:    vs28



### Counterbalance B only: 

Link: https://app.gorilla.sc/admin/project/115811

N = 34

Nodes names:
-   demograhpics: 8v2c
-   psychological questionaires: 
    -   BFI10: mcfk
    -   SCL90R: 2bh6
-   bots questionnaires:
    -   B_1AlexAnx_2PatNorm: gjc4
    -   B_2PatNorm_1AlexAnx: gbf9
-   task names:
    -   B_1AlexAnx_2PatNorm 1AlexAnx:   p8dx   
    -   B_1AlexAnx_2PatNorm 2PatNorm:   19sb
    -   B_2PatNorm_1AlexAnx 2PatNorm:   ses1
    -   B_2PatNorm_1AlexAnx 1AlexAnx:   ocuq



### Counterbalance AB: 

Link: https://app.gorilla.sc/admin/project/122806 "oneReach_counterbalance A and B"

N = 55

Nodes names:
-   demograhpics: 2qbv
-   psychological questionaires: 
    -   BFI10: 7spi 
    -   SCL90R: 83ca
-   bots questionnaires:
    -   A_1PatAnx_2AlexNorm: 7ewr
    -   A_2AlexNorm_1PatAnx: 6eoj
    -   B_1AlexAnx_2PatNorm: wcll
    -   B_2PatNorm_1AlexAnx: eupm 
-   task names:
    -   A_1PatAnx_2AlexNorm 1PatAnx:    ncut
    -   A_1PatAnx_2AlexNorm 2AlexNorm:  vjth
    -   A_2AlexNorm_1PatAnx 2AlexNorm:  4nrv
    -   A_2AlexNorm_1PatAnx 1PatAnx:    hu1g
    -   B_1AlexAnx_2PatNorm 1AlexAnx:   4uru
    -   B_1AlexAnx_2PatNorm 2PatNorm:   tvu5
    -   B_2PatNorm_1AlexAnx 2PatNorm:   ppql
    -   B_2PatNorm_1AlexAnx 1AlexAnx:   mghz


 
## Experiment 2 (extraversion)

### Notes
* Chat 1 is always Extrovert.
* Chat 2 is always Introvert.

The names assigned to the chatbots are what is counterbalanced. In Counterbalance A, “Pat” is Chat 1 Anxious, “Alex” is Chat 2 Normal. In Counterbalance B, “Alex” is Chat 1 Anxious, “Pat” is Chat 2 Normal. The order regarding which chats are experienced first (i.e., chat 1 first or chat 2) are counterbalanced within each counterbalance-by-name. So, there are four counterbalance-groups: 

* Chat 1 Extrovert “Pat” First, Chat 2 Introvert “Alex” Last
* Chat 2 Introvert “Alex” First, Chat 1 Extrovert “Pat” Last
* Chat 1 Extrovert “Alex” First, Chat 2 Introvert “Pat” Last
* Chat 2 Introvert “Pat” First, Chat 1 Extrovert “Alex” Last



### Counterbalance AB: 

link: https://app.gorilla.sc/admin/project/122806 "oneReach_exp2_counterbalance B"

N = 120

Nodes names:
-   demograhpics: 2qbv
-   psychological questionaires: 
    -   BFI10: 7spi 
    -   SCL90R: 83ca
-   bots questionnaires (counterballance A, ask Riddhi):
    -   B_1PatExt_2AlexInt: eupm
    -   B_2AlexInt_PatExt: wcll
-   task names (counterballance A, ask Riddhi):
    -   B_1PatExt_2AlexInt chat1: xsqk 
    -   B_1PatExt_2AlexInt chat2: n6lp
    -   B_2AlexInt_1PatExt chat1: xsqk
    -   B_2AlexInt_1PatExt chat2: n6lp
