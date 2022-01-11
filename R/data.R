#' Head and Neck Cancer Multivariate Blood Data.
#'
#' Patritumab With Cetuximab and a Platinum Agent for Squamous Cell Carcinoma (Cancer) of the Head and Neck (SCCHN ) https://clinicaltrials.gov/ct2/show/NCT02633800
#' Publication: https://pubmed.ncbi.nlm.nih.gov/31648099/
#'
#'Suffix “.c1”, “.lfc” or “.delta” is used to indicate if the measurement relates to blood taken before treatment cycle 1 (baseline) or is the log fold change between cycle 2 and cycle 1 (log2(c2/c1)) or is the absolute difference between cycle 2 and 1 values (c2-c1). If there is no suffix the data is baseline.
#'The “.missing” variables indicate if a particular variable type is missing (value = 1) for that row.
#'The immune variables (9-34, except 25) are given as fractional values of their parent population. See also the gating strategy of the immune panels, below.
#'
#' @format A data frame with 56 rows and 79 variables:
#' \describe{
#'	 \item{Drug}{Original trial arm: 1 = patient was given Patritumab, 0 = patient was given placebo}
#'	 \item{Age}{Patient age in years at joining the original trial}
#'	 \item{Sex}{Birth sex: 1 = Female, 0 = Male}
#'	 \item{Site}{Site of primary cancer}
#'	 \item{Prev.Cetux}{Cetuximab given in a previous round of treatment: 1 = Yes, 0 = No}
#'	 \item{HPV.status}{HPV status at baseline}
#'	 \item{Hereg.status}{Heregulin Status at baseline}
#'	 \item{Smoking}{Patient declared smoking behaviour at baseline: 2 = Current, 1 = Ex smoker, 0 = Never smoked}
#'	 \item{CD33.Monocytes}{CD33+CD14+ Monocytes (as fraction of CD3-CD19-CD33+ parent population)}
#'	 \item{T.cell.CD3}{CD3+, T Cells (as fraction of all cells)}
#'	 \item{T.cell.CD4}{CD4+ T Cells (as fraction of CD3+)}
#'	 \item{T.cell.CD4.Cent.Mem}{CD4 Central Memory Cells (as fraction of CD4+)}
#'	 \item{T.cell.CD4.Effector}{CD4 Effector Cells (as fraction of CD4+)}
#'	 \item{T.cell.CD4.Effect.Mem}{CD4 Effector Memory Cells (as fraction of CD4+)}
#'	 \item{CD4.Naive}{CD4 Naïve Cells (as fraction of CD4+)}
#'	 \item{CD4.Tregs}{CD4 Regulatory T Cells (as fraction of CD4+)}
#'	 \item{CD4.Memory.Tregs}{Memory Tregs (as fraction of CD4 Tregs)}
#'	 \item{CD4.Mem.Act.Tregs}{Memory Activated Tregs (as fraction of CD4 Tregs)}
#'	 \item{CD4.Naive.Tregs}{Naïve Tregs (as fraction of CD4 Tregs)}
#'	 \item{T.cell.CD8}{CD8+ T Cells (as fraction of CD3+)}
#'	 \item{CD8.Cent.Mem}{CD8 Central Memory Cells (as fraction of CD8+)}
#'	 \item{CD8.Effector}{CD8 Effector Cells (as fraction of CD8+)}
#'	 \item{CD8.Effect.Mem}{CD8 Effector Memory Cells (as fraction of CD8+)}
#'	 \item{CD8.Naive}{CD8 Naïve Cells (as fraction of CD8+)}
#'	 \item{CD4.CD8.ratio}{Ratio of CD4 to CD8 T Cells}
#'	 \item{B.cell.CD19.}{CD19+, B Cells (as fraction of all cells)}
#'	 \item{B.cell.mature}{Mature B Cells (as fraction of CD19+)}
#'	 \item{B.cell.memory}{Memory B Cells (as fraction of CD19+)}
#'	 \item{B.cell.Transitional}{Transitional B Cells (as fraction of CD19+)}
#'	 \item{B.cell.plasma}{Plasma B Cells (as fraction of CD19+)}
#'	 \item{B.cell.DN}{Double Negative B Cells (as fraction of CD19+)}
#'	 \item{B.cell.naive}{Naïve B Cells (as fraction of CD19+)}
#'	 \item{B.cell.pre.switch}{Memory pre-switch B Cells (as fraction of CD19+)}
#'	 \item{B.cell.post.switch}{Memory post-switch B Cells (as fraction of CD19+)}
#'	 \item{FRET}{Degree of exosomal HER1-HER3 protein interaction determined by FRET-FLIM}
#'	 \item{normalised.miR.21.5}{Degree of exosomal microRNA, miR-21-5p, as determined by ddPCR}
#'	 \item{normalised.miR.142.3p}{Degree of exosomal microRNA, miR-142-3p, as determined by ddPCR}
#'	 \item{t.PFS}{PFS event or censored time (months)}
#'	 \item{event.PFS}{PFS event type: 1 = progression event, 0 = right censored}#' }
#' @source \url{https://doi.org/10.5522/04/16566207.v1}
"tte_data"
