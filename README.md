# DeJesus_capstone

---
title: "Capstone 2020"
author: "Kenneth De Jesus"
date: "April 24, 2020"
output: html_document
---
  
  2020 Capstone Assignment:
  
  Statistically Design an Experiment

tl;dr

This is the last one
Turn this by the due date. 
Write it in an R markdown and submit knitted html
You can collaborate in so far as giving and receiving feedback from colleagues.
5 pts per item!
  For a rubric on how to write this, see Chapter 10 in JABSTB. Follow that multi-step format!!!
  
1) Provide a brief background and significance about a specific research problem that interests you. It could be project you’re involved with now, or a rotation project, or something you’d like to work on. The reader will need to understand enough background to make sense of the experiment you propose below. Keep it brief. In one short paragraph.

**Excessive exposure to glutamate is believed to be one of the reasons that nerve cells (motor neurons) die in patients with Amyotrophic Lateral Sclerosis (ALS). ALS is a progressive neurodegenerative disease that primarily affects motor neurons in the cerebral motor cortex, brainstem, and spinal cord, leading to loss of voluntary movement and the development of difficulty in swallowing, speaking, and breathing and shortened life expectancy. Preventing the rise of glutamate levels could be the key to protecting motor neurons and impeding the progress of the disease. Though too much glutamate can be toxic, normal levels of glutamate are necessary for the nervous system to function properly. There is a need for a treatment that could clear the excess glutamate while maintaining normal glutamate levels. This experiment presents a potential drug treatment to test its ability to reduce damage to motor neurons by decreasing levels of glutamate, which transports messages between nerve cells and motor neurons. Pharmacological properties should include the following: 1) an inhibitory effect on glutamate release, 2) inactivation of voltage-dependent sodium channels, and 3) ability to interfere with intracellular events that follow transmitter binding at excitatory amino acid receptors.**

2) Briefly state something that is unknown about this system that can be discovered through, and leads to, an experiment.  For example, "It is not known whether....."

**It is not known wether the application of drug substance with maximal concentration can decrease motor neuron excitability to inhibit a range of distinct postsynaptic mechanisms for potential ALS therapy.**

3) Make an “if” “then” prediction that is related to item #2. It should be of the general form, “if X is true, then Y should happen”.

**If drug therapy is able to decrease motor neuron excitability by multiple mechanisms, then it should be seen a decrease in synaptic glutamate release, depressing postsynaptic glutamate receptor responses, and inhibiting several postsynaptic ion currents that control resting membrane potential and action potential firing and threshold.**

4) What dependent variable will be observed to test this prediction in item #3? What predictor variable will be used to manipulate the system experimentally? Define the inherent properties of these variables (eg, are they sorted, ordered or measured).

**The effects of inward current (dependent variable) evoked by local glutamate application will be observed to prove the previous stated prediction of motor neuron excitability decrease. Predictor variable that will be used to manipulate the system will be two controls: a pleacebo and a receptor blocker to act as a negative control which will provide no effect at all. Dependent variable is measured, and predictor variables are sorted, discrete and factoral variables.**

5) Write a statistical hypothesis.  There should be a null and alternate. These should be explicitly consistent with the prediction in item #3 and the response variable in #4. In other words, make sure the statistical hypotheses that you write here serves as a test of the prediction made in item #3.

**Null hypothesis: The drug response does not inhibits motor neurons effects.** $\ null, H_{0}: \mu_{drug}\ = \mu_{control}$
**Alternate hypothesis: The drug response inhibits motor neurons effects.** $\ null, H_{0}: \mu_{drug}\ \ne \mu_{control}$

6) What is the statistical test you would use to test the hypothesis in item #5? Briefly defend what makes this appropriate for the hypothesis and the experimental variables. If there are alternatives, why is this approach chosen instead? Points will not be awarded if the justification involves something like "because everybody does it this way".

**The statistical test for this experiment is a two-sided, paired t-test for comparing group means per sample. It is desired to analyze data based on the treatment as pedrictor variables that state the outcome of motor neuron excitability independently. Two-sided analysis provides the evidence to predict a difference between the multiple treatment groups being tested. The sample size will be based upon a power of 90%, which means that the tolerance level for type2 error will be 10%. The decision threshold for type1 error will be 5%. Thus, the null hypothesis will be rejected at a p-value of less than 0.05.**

7) List the procedures and decision rules you have for executing and interpreting the experiment. These procedures range from selection of experimental units, to randomization to primary endpoint to threshold decisions. Define (and defend) what you believe will be the independent replicate.

**Membrane currents from the cell soma of rats will be recorded in the whole-cell configuration of the patch-clamp method. Independent replicates will be assured trough different animal samples to receive unique treatment testing. Excitatory postsynaptic currents (EPSCs) will be evoked by electrical stimulation (5–120 V, 0.05–0.2 ms, 0.1 Hz) of the reticular formation lateral to the border of the ipsilateral hypoglossal nucleus with an insulated bipolar platinum wire electrode. Electrical recordings are to be performed at room temperature (21–25°C) with patch electrodes pulled from borosilicate glass capillaries on a two-stage puller to a DC resistance of 5–7 MΩ and connected to the headstage of an Axopatch 1D patch-clamp amplifier or a Multiclamp 700A amplifier. Action potentials  will be evoked by depolarizing current steps from a subthreshold membrane potential, which will be kept constant in control and drug conditions by manual adjustment of baseline current injection levels. These decision will assure randomization through the process for accountable data and with valid study endpoints of 24hrs of excitability after treatment injection.**

8) Produce a graph of a simulation for the expected results. Create a dataMaker-like function in R to create and plot the data. Label and scale any axis. The graph should illustrate the magnitude of the expected response, or the level of response that you expect to see and would be minimally scientifically relevant. Be sure to illustrate any variation that is expected.
```{r message=FALSE, warning=FALSE}
library(tidyverse)
library(ez)
library(viridis)
```

```{r}
b <-  -300 #expected basal outcome value
a <-  1.25 #expected fold-to-basal effect of our positive control
f <-  1.4 #minimal scientifically relevant fold-to-basal effect of our treatment
sd <-  100 #expected standard deviation of Outcome variable
n <-  3 # number of independent replicates per group
r <- 0.9 #correlation between groups of outcome values across replicates
k <- sqrt(1-r^2) #a conversion factor
sims = 100 #number of Monte Carlo simulations to run. Keep it low to start.


RMdataMaker <- function(n, b, a, f, sd, r, k) { 
  
  
  a1 <- rnorm(n, b, sd) 
  a2 <- a1*r+k*rnorm(n, (b*-a), sd) 
  a3 <- a1*0 
    
    Outcome <- c(a1, a2, a3)
    Predictor <- c(rep(c("Control", "Drug", "Receptor Blocker"), each = n))
    ID <- rep(as.factor(c(1:n)))
    df <-data.frame(ID, Predictor, Outcome)
    }

dat <- RMdataMaker(n,b,a,f,sd,r,k)

ggplot(dat, 
       aes(Predictor, Outcome, color=ID, group=ID, cl = 0.95)
       )+ 
  geom_line()+
  geom_point(size = 4, shape=5)+
  scale_color_viridis(discrete=T)+
  theme_classic()+
  labs(x="Treatment", y="Mean Peak (pA)")
```

**Mean peak glutamate receptor current was reduced significantly by drug (P < 0.05, n = 3) and by receptor blocker (P < 0.05, n = 3).**

9) Write and perform a Monte Carlo analysis to calculate a sample size necessary to test the hypothesis. This Monte Carlo must test the primary endpoint.

```{r}
pval <- replicate(
  sims, {
 
    sample.df <- RMdataMaker(n, b, a, f, sd, r, k)
    
    sim.ezaov <- ezANOVA(
            data = sample.df, 
            wid = ID,
            dv = Outcome,
            within = Predictor,
            type = 2
            )
  
  pval <- sim.ezaov$ANOVA[1,5]
    
    }
  )

pwr.pct <- sum(pval<0.05)/sims*100
paste(pwr.pct, sep="", "% power.")
```

10) Write up it all in RMarkdown. Code chunks to illustrate specific points are welcome other than for the Monte Carlo code. Knit and submit and upload the html document by the due data. If it is readable to your best friend, it is readable to us.

EXTRA CREDIT: Github is an important resource for sharing data/analyses and collaborating.  In fact, 3 different Github repositories were used for the material in this course. We encourage you to submit your capstone as a Github page, as a gentle way to play with Github. See the directions Jessie wrote up: GitHub_pages_IBS538.pdfPreview the document

If you submit your capstone on Github, we'll need you to knit and submit a different document on Canvas. For that, just open up a blank Rmd. Only put the link to your github page in it. Knit and submit its html on canvas. The markdown code for a link is like this:

Thanks for such an exciting semester Austin, Jess, Lauren & even you, TJ! [Please click here to see my capstone project on Github.](https://url_of_my_capstone (Links to an external site.))

Those brackets and parentheses make the markdown happen.
