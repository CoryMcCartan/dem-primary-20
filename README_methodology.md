# Estimating Candidates' Structural Advantages and Disadvantages

## Miscellaneous Documentation

- Age categories: 18--34, 35--49, 50--64, 65+
- Racial categories: non-hispanic white, non-hispanic black, hispanic (any race), other (omitted)
- Educational categories: college (non-technical associates or higher) and no college (everyone else)

## Step 1: Get counts of voters by state and demographic
This comes from the [CPS microdata](https://thedataweb.rm.census.gov/ftp/cps_ftp.html)
Tabulations are into the demographic categories above, by state (including DC).
We also build a table, for each demographic group, of the percentage of each
demographic within it.  For example, "white men" are 100% men, 100% white, 25%
between 18 and 34, etc.

## Step 2: Estimate model for Democratic candidate support in the previous election
We get marginal two-party Democratic share in each demographic from  [Catalist data](https://docs.google.com/spreadsheets/d/1fFeA7rAPHIsuty77QiSWDIv0GrPM4RYCG5uPkMNSkKk/edit#gid=0). We also have the two-party Democratic share by state.

For demographics, we regress the Democratic share for a group on a logit scale
on the percentage of each demographic category within the demogrphic group (the
table from Step 1). This gives us a coefficient for each demographic category.

We use this model to predict baseline Democratic support within each
state-demographic cell.  We then perform Iterative Proportional Fitting
(modified to work on a logit scale to avoid giving more than 100% share to
the party) on the state and demographic marginals.  This yields a number of
votes, and an implied number of Democratic votes, within each state-demographic
cell, across the whole country.

## Step 3: Adjusting demographics over time
We use the Census' [Population Projection Datasets](https://www.census.gov/programs-surveys/popproj/data/datasets.html) 
 (Table 1; Table 1 Middle series for 2012 and earlier). We apply the percentage
change in each demographic category to the electorate estimated in Step 2. The
census tables don't include education, so we make an ad-hoc adjustment in line
with past trends that the college-educated share of the electorate will increase
by 3% and the non-college-educated share will decrease by 3%.

## Step 4: Calculate implied shifts in demographic groups since last election
The first part of this is computing, for each demographic group in the last
election, the difference in support for that group vs the nation, on a logit
scale.  Then the head-to-head polling is used,  by first computing the two-party
share of dem votes in each group, and then again computing the difference in
support relative to overall support, on a logit scale.  For each group, the
difference now minus the difference last election gives the necessary adjustment
per group.

## Step 5: Apply shifts 
Then we go through the voter table (output of step 3) and reallocate votes in
line with the necessary adjustments from step 4.  This usually leads to a change
in the popular vote, so we then shift the entire table proportionally to match
the desired national popular vote. Finally, results are tallied by state.
Redoing this analysis at various levels of popular vote support allows us to
answer our main question: which candidates have structural advantages and
disadvantages?

## Step 6: Add error
We repeat the above process, perturbing the polling results across groups and
candidates with random, normally-distributed noise.

