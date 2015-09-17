# H3K4me3 peak breadth around TSSs in primary monocyte

We studied the H3K4me3 breath around TSSs in human primary monocyte, and its association with gene expression in both control samples and SLE patients. We started with the H3K4me3 at 27,588 TSSs of six control samples, and then related the H3H4me3 patterns identified from these samples to H3K4me3 in six SLE samples and RNA-seq results from the same samples. ChIP-seq depth of the control samples was averaged and normalized to background, so a measurement of 1.0 corresponds to an average depth two times higher than the background.

This analysis is focused on the [-5kb, 5kb] regions around 27,588 known TSS. The ChIP-seq sequencing depth has been processed and normalized to background as what's described in our previous paper. Within the 10kb regions around TSSs, the depth of samples in the same group was averaged into 50bp bins and the resultant data is two 27,588 X 201 matrix for the control and SLE groups.

We also previously obtained the RNA-seq data from the same sample cohort and the results of differential gene expression. These results can be used to evaluate the impact of peak breadth on gene expression.

The goal of this analysis is to identify distinctive H3K4me3 patterns around TSSs, based on peak breadth. We hypothesized that these patterns correspond to gene subsets having different cellular functions and transcriptional behaviors.

## Results

### Classification of TSSs by their H3K4me3 breadth in primary monocytes

The average H3K4me3, from 250bp upstream to 250bp downstream of TSSs, follows a bimodal distribution. The left-end component corresponds to random background noise from sites without H3K4me3 whereas the right-end component corresponds to sites with different levels of H3K4me3. We selected about half of all sites having significant higher H3K4me3 than background for all analyses in this study.

Agreeing with canonical H3K4me3 pattern, H3K4me3 was clearly enriched at TSS sites in general and dropped by 50% at ~450bp upstream and ~650bp downstream of TSSs (Figure 1). Closer look of individual sites suggested that H3K4me3 was not reduced at the same pace at all sites. Sites having H3K4me3 dropped immediately or slowly correspond to narrow or broad H3K4me3 peaks. To identify different types of sites, we first calculated the upstream-TSS and downstream-TSS difference of H3K4me3 for each sites. The differential H3K4me3 of both comparisons also followed bimodal distributions (Figure 2), suggesting that there are discrete groups. We were able to classify 8,398 of the 14,217 TSSs into four groups based on their H3K4me3 patterns: 1) sites with narrow H3K4me3 limited to TSS; 2) sites with extended H3K4me3 upstream of TSSs only; 3) sites with extended H3K4me3 downstream of TSSs only; and 4) sites with extended H3K4me3 at both ends (Figure 3 and 4). 

### Functional categorization of genes with these patterns was performed using DAVID
We run DAVID to look for gene sets enriched in these 4 H3K4me3 patterns, using all 14,217 H3K4me3 as background.
- 814 TSSs with narrow H3K4me3 ([results]({{ SLINK }}/results/DAVID_Ctrl_Narrow/index.html)); enriched with genes related to housekeeping functions such as mitochondrian, ribosome, and celluar membrane.
- 3,045 TSSs with extended H3K4me3 at upstream only ([results]({{ SLINK }}/results/DAVID_Ctrl_Upstream/index.html)); enriched with the target genes of a large number of TFs, such as SP1 and CREB.
- 2,789 TSSs with extended H3K4me3 at downstream only ([results]({{ SLINK }}/results/DAVID_Ctrl_Downstream/index.html)); enriched with immune response genes.
- 1,752 TSSs with extended H3K4me3 at both sides ([results]({{ SLINK }}/results/DAVID_Ctrl_Both/index.html)); also enriched with TF target genes, and genes regulating DNA binding. 

### Association between H3K4me3 breadth at TSS and  gene expression

We compared expression levels and between-sample variance of the genes associated with these four groups of TSSs, using the RNA-seq data of eight control samples (Figure 5 and 6). Comparing to sites not classified to any of the four groups, all classified TSSs, especially those with high downstream H3K4me3, are associated with higher baseline expression, suggesting H3K4m3 downstream of TSS activates gene expression. Similarly, downstream H3K4me3 was associated with higher between-sample variance of gene expression level. Since highly expressed genes had higher between-sample variance, we adjusted the variance for expression level and compared the groups again. 

From our RNA-seq data, we previously identified 1,122 and 775 genes having respectively higher and lower expression in SLE (p < 0.01). We found that there is a strong association between the H3K4me3 breadth and likelihood of differential expression (Figure 7). Genes with narrow H3K4me3 were less likely to have increased expression in SLE while those with extended H3K4me3 downstream of their TSSs were more likely to have increased expression. Opposite trend was seen between H3K4me3 and genes having decreased expression in SLE.


### The change of H3K4me3 breadth in SLE and its impact on gene transcription

We then looked at the change of H3K4me3 breadth in SLE. The _ sites classified into the four groups were reclassified using the average H3K4me3 of six patients. 94.8% % of these sites were classified into the same group, suggesting H3K4me3 breadth is mostly stable in primary monocyte. Among 907 sites switched patterns from control to SLE, none of them switched between narrow sites and any of the three broad sites. The most common switch was the 131 sites from “Both” to “Downstream only”. 

We previously reported that H3K4me3 increase at TSSs was associated with increased expression of corresponding genes in SLE.  We further included regions upstream and downstream of TSSs in this study.  H3K4me3 changes at all three regions, upstream, TSS, and downstream, were positively correlated to expression change in SLE (Figure 8). High magnitude of H3K4me3 change at TSSs caused the biggest expression change, but the region downstream of TSS had similar impact. Interestingly, expression change was very sensitive to H3K4me3 change at the downstream regions. From 1% to 10%, every one percent increase of H3K4me3 at TSS downstream lead to 1.5% increase of gene expression on average. On the other hand, TSSs themselves affected gene expression more when more dramatic increase of H3K4me3 happened. 

Since H3K4me3 at adjacent regions changes together, we analyzed the association between differential expression and H3K4me3 change at these three regions again after removing their dependence on each other (Figure 9). The positive correlation between upstream H3K4me3 and gene expression was mostly gone, suggesting that the previously observed association was due to the concordant change of H3K4me3 at TSSs. On the other hand, the association between downstream H3K4me3 and gene expression remained, suggesting that downstream change of H3K4me3 is important to gene expression change. Of the genes having significant increase of expression in SLE, 78.8% also had increased H3K4me3 at the downsream regions, while the percentages were 55.0% and 47.1% for TSSs and upstream regions, respectively (Figure 10). Indeed, the average H3K4me3 change of these genes peaked (~8%) at approximately 700bp downstream of their TSSs (Figure 11). 

## Conclusions:

1. H3K4me3 breadth at TSSs is related to gene expression, mostly with gene expression level and less with sample-sample variance. But the dominant factor here seems to be the H3K4me3 level downstream of TSSs. 
2. Relative change of H3K4me3 at downstream of TSSs is also a major factor of gene expression change. Gene expression is very sensitive to H3K4me3 change at this region. 
3. It's surprising that upstream H3K4me3 change had no global association to gene expression change after its depedence on nearby regions was removed. This might suggest that this region affects gene expression in a TSS by TSS way, considering that target genes of a large number of TFs are enriched in this group.


## Figures

- Figure 1. The average H3K4me3 around TSS reduces by 50% at ~400-500bp upstream and ~600-700bp downstream. We hypothesize that H3K4me3 level at these regions affects transcription of downstream gene. 
  ![]({{ SLINK }}/results/figures/global_average.png)

- Figure 2. The H3K4me3 levels at both upstream and downstream of TSSs fit a bimodal distribution. Consequently, TSSs can be classified into 4 groups: narrow, broad-upstream-only, broad-downstream-only, and broad-both-sides. 
  ![]({{ SLINK }}/results/figures/shoulder2head_fitted_Ctrl_Upstream.png)
  ![]({{ SLINK }}/results/figures/shoulder2head_fitted_Ctrl_Downstream.png) 

- Figure 3. The average H3K4me3 of four groups of TSSs is different at downstream and upstream regions.
  ![]({{ SLINK }}/results/figures/Pattern_Depth_Ctrl.png)

- Figure 4. Heatmaps showing H3K4m3 patterns of different TSS groups
  ![]({{ SLINK }}/results/figures/Heatmap_Ctrl_Narrow.png)
  ![]({{ SLINK }}/results/figures/Heatmap_Ctrl_Upstream.png)
  ![]({{ SLINK }}/results/figures/Heatmap_Ctrl_Downstream.png)
  ![]({{ SLINK }}/results/figures/Heatmap_Ctrl_Both.png)

- Figure 5. H3K4me3 breadth vs. gene expression level
  ![]({{ SLINK }}/results/figures/Barplot_average_fpkm_Ctrl.png)

- Figure 6. H3K4me3 breadth vs. sample-sample variance of gene expression level
  ![]({{ SLINK }}/results/figures/Barplot_average_sample-sample_variance_Ctrl.png)

- Figure 7. The likelihood for downstream genes to have significantly changed expression level when the TSS has one of the 4 patterns of H3K4me3 breadth.
  ![]({{ SLINK }}/results/figures/OddsRatio_Expr-Breadth_Control.png)

- Figure 8. Effect of H3K4me3 change at upstream, TSS, and downstream on gene expression
  ![]({{ SLINK }}/results/figures/H3K4me3-transcription_Up_Original.png)
  ![]({{ SLINK }}/results/figures/H3K4me3-transcription_Down_Original.png)

- Figure 9. Effect of H3K4me3 change at upstream, TSS, and downstream on gene expression, after removing the dependence of these 3 regions on each other.
  ![]({{ SLINK }}/results/figures/H3K4me3-transcription_Up_Adjusted.png)
  ![]({{ SLINK }}/results/figures/H3K4me3-transcription_Down_Adjusted.png)

- Figure 10. H3K4me3 changes of genes with significant increase of expression in SLE. 
  ![]({{ SLINK }}/results/figures/H3K4me3_of_Upregulated_Genes_After_Adjustment.png)

- Figure 11. Every H3K4me3 change of genes having significantly increased expression in SLE. 
  ![]({{ SLINK }}/results/figures/H3K4me3_of_Upregulated_Genes_Around_TSS.png)


