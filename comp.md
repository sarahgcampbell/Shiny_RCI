#### Computation of CTT-RCI (from Jacobson & Truax, 1991)

The CTT-based RCI is calculated by the individual’s post- pretest score difference divided by the standard error of measurement (SEM) as follows:

$$
RCI_{CTT} = \frac{X_{post}-X_{pre}}{\sqrt{SE_{pre}^2+SE_{post}^2}}=\frac{X_{post}-X_{pre}}{\sqrt{2}SEM_{pooled}}
$$

where the standard error (SE) is often assumed to be equal for pre- and post-test $SE_{pre}^2 = SE_{post}^2$, and the pre-test SE is usually adopted. Therefore, $SE_{pre}$ is computed by the $SD_{pre}$ and a reliability estimate. The reliability estimate can be that of test-retest reliability, Cronbach’s alpha, McDonald’s omega, or any other suitable ones.
$$
SEM_{pooled}^2 = {SE_{pre}^2+SE_{post}^2}
$$

$$
SE_{pre} = SD_{pre}\sqrt{1-rxx}
$$

<br>
The original RCI is based in Classical Test Theory (CTT), in which the assumptions that comes with the framework limits the accuracy of individual estimates. Therefore, Item Response Theory (IRT)-based models have been adopted to provide more accurate individual assessments. Specifically, under IRT, individual’s change scores are measured by their own measurement variability, instead of a constant measurement variability that is used across all individuals.
<br>
<br>

#### Computation of IRT-RCI (from Jabrayilov et al., 2016)
The computation of the IRT version of the RCI is only slightly modified, in that the numerator is the estimated latent trait instead of the sum score and the SE is now varied by time points and the individual.


\begin{equation}
RCI_{IRT}=\frac{\hat{\theta}_{post}-\hat{\theta}_{pre}}{\sqrt{SE_{post}^2+SE_{pre}^2}}
\end{equation}


<br>



**IRT Estimator Considerations**

In IRT, there are different ways to estimate $\hat{\theta}$.

The **maximum-likelihood (ML)** estimation is usually the default estimation method in historical softwares, but can be problematic when extreme scores are present (e.g., scoring the highest/lowest number in a depression measure). 

The **weighted maximum-likelihood (WML)** estimation was used by Jabrayilov et al. (2016), in which a weighted term is incorporated in the estimation of $\hat{\theta}$. This was done to correct ML's shortcomings, so that individuals with extreme scores would be properly estimated. 

The **expected a posteriori (EAP)** estimation was used by Chalmers and Campbell (2025), in which a prior population distribution of the latent trait (i.e., you can think of this as the characteristics of the group in which the individual belongs to) is incorporated in the estimation of $\hat{\theta}$. This was also done to reduce bias, and does a better job at estimating individual's change than WML when the individual actually changed. Though any IRT estimation perform far better than CTT. 

The **EAPsum** estimation method uses the expected value of the posterior distribution conditioned on the sum score, not the full response pattern. EAPsum precomputes the EAP estimates for all possible raw sum scores within the test's score range. Instead of using the expected posterior for each of the item response vectors to compute the posterior distribution (as in EAP), EAPsum marginalizes over all response patterns that lead to the same total score. Then, EAPsum computes the expected value of the marginal posterior, conditioned only on the sum score.

Other estimation methods, such as ML, WML, MAP, or EAP, do not have algorithms for directly estimating ability from sum scores; instead require the full item response vector as the likelihood depends on the item-level data.

<br>
<br>




**Computation Package and Functions in R**\
All calculation of the RCI estimates uses the RCI() function in the mirt R package (Chalmers, 2012).  

<br>
<br>
