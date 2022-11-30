class: center, middle, inverse, title-slide

# <br><br>clusterBMA: Combine insights from multiple clustering algorithms with Bayesian model averaging <br><br>
## Owen Forbes <br><br>
### 
### Distinguished Professor Kerrie Mengersen <br> Dr Edgar Santos-Fernandez <br>Dr Paul Wu <br><br>Queensland University of Technology

---


class: list-space

<style>

.list-space li {
padding: 0.25cm;
}

.list-nobullet li {
  list-style-type:none;
}

.center2 {
  margin: 0;
  position: absolute;
  top: 30%;
  left: 40%;
  -ms-transform: translate(-35%, -35%);
  transform: translate(-35%, -35%);
}


</style>






## Which clustering algorithm?


.pull-left[<iframe src="imgs/plot_3d_km.html" width="500" height="250" scrolling="yes" seamless="seamless" frameBorder="0"> </iframe>]

.pull-right[<iframe src="imgs/plot_3d_hc_colourfix.html" width="500" height="250" scrolling="yes" seamless="seamless" frameBorder="0"> </iframe>]

.pull-left[<iframe src="imgs/plot_3d_gmm.html" width="500" height="250" scrolling="yes" seamless="seamless" frameBorder="0"> </iframe>]


---

## Inconsistent clustering across algorithms

<iframe src="imgs/plot_3d_core_yellow.html" width="1000" height="500" scrolling="yes" seamless="seamless" frameBorder="0"> </iframe>


---

### Which clustering algorithm?

- Different algorithms will emphasise different aspects of clustering structure

--

- Choosing one 'best' model often arbitrary, unclear choice

--

  - ➡️ **Inference not calibrated for model-based uncertainty**

--

- Locking into one method loses insights offered by other methods about plausible clustering structure

--

.pull-left[
<img src="data:image/png;base64,#imgs/porque.gif" width="800" />
]

--

.pull-right[
**BMA offers a nice framework for combining clustering solutions**

  - simple
  
  - flexible

  - intuitive 
]

---


### Combining Clustering Results with Bayesian Model Averaging

--

- Limited development for Clustering
  - Finite mixture models (Russell et al., 2015)
  - Naive Bayes classifiers (Santafe & Lozano, 2006)
  - Lacks implementation across multiple clustering algorithms

--
<br>

**Advantages**

- 🆒 Weighted averaging of results incorporating model quality / goodness of fit

--

- 💯 Intuitive framework for **probabilistic** inferences combining results from **different clustering algorithms**

--

- 😮 Each input solution can have a different number of clusters `\(K\)`

--

- 😎 **Quantify model-based uncertainty and enable more robust inferences** calibrated accordingly




---

### Bayesian Model Averaging: Basics

<br>
<br>

`$$P(\Delta | Y) = \sum_{l=1}^L (\Delta | Y, M_l)P(M_l|Y)$$`



--
<br>
<br>

`$$P(M_l|Y) = \frac{P(Y|M_l)P(M_l)}{\sum_{l=1}^L P(Y|M_l)P(M_l)}$$`


---

#### BMA for Mixture Models - BIC weighting
- `\(P(Y|M_l)\)` typically involves a difficult/intractable integral and is often approximated for many applications (Fragoso et al., 2018)

--

- **Russell et al. (2015)** weight results from multiple GMMs according to BIC

`$$P(M_l \lvert Y) \approx \frac{exp(\frac{1}{2}{BIC}_l )}{\sum_{l=1}^L { exp(\frac{1}{2} BIC_{l}) }}$$`

--

- BIC definition for GMM

`$${BIC}_l = 2\log(\mathcal{L}) - \kappa_m \log(N)$$`

--

- GMM likelihood

`$$\mathcal{L}(\Theta) = \sum_{n=1}^N \sum_{k=1}^K{\pi_k\mathcal{N}(x_n|\mu_k, \Sigma_k)}$$`
--

- Multivariate Gaussian density

`$$\mathcal{N}(x|\mu, \Sigma) = \frac{\exp\left\{ -\frac{1}{2} (y-\mu)^T \Sigma^{-1}(y-\mu)\right\}}{|\Sigma|^{\frac{1}{2}} (2 \pi)^{\frac{D}{2}}}$$`
                 
                 
---
background-image: url("data:image/png;base64,#imgs/separation_compactness.png")
background-position: right
background-size: 600px

### Aside: Cluster internal validation indices

.pull-left[.content-box-blue[
- Often used as a proxy for model quality in clustering
{{content}}
]
]
--

- Choose between candidate models with different numbers of clusters `\(k\)`

{{content}}
--

- Interpreted similarly to marginal likelihood/model evidence

{{content}}
--

- Typically measure compactness and/or separation of clusters

{{content}}
--
Compared to BIC...
  - Agnostic to clustering algorithm
  - Typically do not require likelihood term

---
background-image: url("data:image/png;base64,#imgs/separation_compactness.png")
background-position: top right
background-size: 300px


### New proposed weighting / approximation for posterior model probability

BIC for GMM driven by **Multivariate Gaussian density:**

`$$\mathcal{N}(x|\mu, \Sigma) = \frac{\exp\left\{ -\frac{1}{2} (y-\mu)^T
                 \Sigma^{-1}(y-\mu)\right\}}{|\Sigma|^{\frac{1}{2}} (2 \pi)^{\frac{D}{2}}}$$`
                 
--

**Xie-Beni index**
- Ratio of compactness to separation (maximise)
`$$XB = \frac{\sum_i{\sum_{x \in C_i}d^2(x,c_i)}}{n(\min_{i, j \neq i} d^2(c_i,c_j))}$$`

--

**Calinski-Harabasz Index**
- Ratio of separation to compactness (minimise)
`$$CH = \frac{\sum_i{n_i d^2(c_i,c)/(NC-1)}}{\sum_i{\sum_{x \in C_i}d^2(x,c_i)/(n-NC)}}$$`

--



- XB and CH have **complementary strengths** (Liu et al., 2010)

---

### New proposed weighting / approximation for posterior model probability

XB and CH indices
- conceptually and mathematically similar to BIC
- Unlike BIC, can be calculated + directly compared across different clustering algorithms

--

New proposed weight:
`$$\mathcal{W}_m = \frac{\frac{1}{CH_{m}}}{\sum_{m=1}^M {\frac{1}{CH_{m}}}} + \frac{XB_m}{\sum_{m=1}^M {XB_{m}}}$$`

<br>

--

Approximate posterior model probability for weighted averaging:
`$$P(Y \lvert \mathcal{M}_m) \approx \hat{\mathcal{W}}_m = \frac{\mathcal{W}_m}{\sum_{m'=1}^M \mathcal{W}_{m'}}$$`


---


### Consistent quantity `\(\Delta\)` - Similarity matrices

Previous work (Russell et al., 2015) has used pairwise similarity matrices as `\(\Delta\)` for each model

--

- To get similarity matrix, multiply allocation matrix by its transpose:

`$$S_m = A_m A_m^T$$`

- **invariant to number and labelling of clusters across solutions**

--

.center[
<img src="data:image/png;base64,#imgs/a_to_s.png" width="800" />
]




---

### Consensus matrix

.center[
<img src="data:image/png;base64,#imgs/eeg_consensus.jpg" width="600" />
]

`\begin{equation}
  C = {\sum_{m=1}^M\hat{\mathcal{W}}_m S_m}.
\end{equation}`

---

### Consensus matrix ➡️ Matrix factorisation ➡️ Cluster allocation probabilities

- Symmetric Simplex Matrix Factorisation (SSMF; Duan, 2020) to get `\(N \times  K\)` allocation matrix `\(A_m\)` from `\(N \times N\)` consensus matrix `\(C\)`

- Generates probabilistic cluster allocations from pairwise probabilities

--

.center[
<img src="data:image/png;base64,#imgs/c_to_a.png" width="700" />
]
--

- Includes L2 regularisation step to reduce overfitting & redundant clusters



---


class: center, middle


## Case study: Clustering adolescents based on resting state EEG recordings


---

## Model results


.center[
<img src="data:image/png;base64,#imgs/clusters_scrnshot.jpg" width="1000" />
]

---

## Model results ➡️ Similarity matrices

.pull-left[**k-means** `\(\hat{\mathcal{W}}_m = 0.36\)`
<br>
<img src="data:image/png;base64,#imgs/km_similarity.png" width="300" />
]

.pull-right[**HC** `\(\hat{\mathcal{W}}_m = 0.27\)`
<br>
<img src="data:image/png;base64,#imgs/hc_similarity.png" width="300" />
]

.center[**GMM** `\(\hat{\mathcal{W}}_m = 0.37\)`
<br>
<img src="data:image/png;base64,#imgs/gmm_similarity.png" width="300" />
]



---

## Model results ➡️ Similarity matrices ➡️ Consensus matrix

<img src="data:image/png;base64,#imgs/eeg_consensus.jpg" width="800" />



---

## BMA Clusters with allocation uncertainty

.center[
<iframe src="imgs/eeg_3d_BMA_size_uncertainty_301122.html" width="750" height="400" scrolling="yes" seamless="seamless" frameBorder="0"> </iframe>
]

--

- **Uncertainty can be propagated forward for further analysis in a Bayesian framework**


---

class: center, middle


## A quick demo

---

<iframe src="imgs/demo_recording_1080.mp4" width="1000" height="650" scrolling="yes" seamless="seamless" frameBorder="0"> </iframe>


---

background-image: url("data:image/png;base64,#imgs/bernie_meme.jpg")
background-position: center
background-size: contain

---
### Next steps

- Benchmark against other ensemble clustering methods

- Compare weighting with BIC vs `\(\hat{\mathcal{W}}_m\)` for GMMs

- Consider alternative internal validation metrics to approximate `\(P(\mathcal{M}_m \lvert Y)\)`

--

- 🐦 Twitter: **@oforbes22**

--

- Preprint: **bit.ly/clusterBMA_preprint**

<img src="data:image/png;base64,#imgs/preprint_QR.png" width="100" />

.center[
<img src="data:image/png;base64,#imgs/arxiv_screenshot.jpg" width="700" />
]

--





---

## Cluster uncertainty for applied inference


.centre[
<img src="data:image/png;base64,#imgs/Fig6_trimmed.png" width="800" />
]
--

Uncertainty in cluster allocation could be used to **moderate risk prediction** based on data-driven brain phenotypes

---

### Equal Prior Weights

XB and CH indices
- conceptually and mathematically similar to BIC
- Unlike BIC, can be calculated + directly compared across different clustering algorithms

--

New proposed weight:
`$$\mathcal{W}_m = \frac{\frac{1}{CH_{m}}}{\sum_{m=1}^M {\frac{1}{CH_{m}}}} + \frac{XB_m}{\sum_{m=1}^M {XB_{m}}}$$`

<br>

--

Approximate posterior model probability for weighted averaging:
`$$P(Y \lvert \mathcal{M}_m) \approx \hat{\mathcal{W}}_m = \frac{\mathcal{W}_m}{\sum_{m'=1}^M \mathcal{W}_{m'}}$$`


--

Substituting in for model evidence and prior in BMA posterior model probability:
`$$P(\mathcal{M}_m \lvert Y) \approx \frac{\hat{\mathcal{W}}_m \left(\frac{1}{M}\right)}{\sum_{m'=1}^M \hat{\mathcal{W}}_{m'} \left(\frac{1}{M}\right)} = \hat{\mathcal{W}}_m$$`

---
class: center, middle


## Simulation study (A): Cluster separation

---

#### Simulated datasets - R package "clusterGeneration"





<img src="data:image/png;base64,#imgs/sim_data_plots.png" width="1000" />



---
#### BMA solutions


<img src="data:image/png;base64,#imgs/sim_far_bma_new.png" width="600" />


---
#### BMA solutions

<img src="data:image/png;base64,#imgs/sim_med_bma_new.png" width="600" />


---
#### BMA solutions

<img src="data:image/png;base64,#imgs/sim_close_bma_new.png" width="600" />


---



## Simulation study (B): Different numbers of clusters


---

<img src="data:image/png;base64,#imgs/diffk_simstudy_270722.png" width="600" />


---
.pull-left[
#### *k*-means

- 'Hard' clustering
- Minimises within-cluster sums of squares]


.pull-right[**k-means objective function** <br>
`$$J = \sum_{i = 1}^K (\sum_k{\lvert \lvert x_k - c_i \rvert \rvert ^2})$$`]

--

.pull-left[
#### Hierarchical Clustering (Ward's Method)

- 'Hard' clustering
- Each observation starts out in its own cluster
- Repeated pairwise fusion of clusters that minimises change in within-cluster sums of squares (Ward)]


.pull-right[**Ward's objective function** <br>
`$$D(c_1,c_2) = \delta^2(c_1,c_2) = \frac{\lvert c_1 \rvert \lvert c_2 \rvert }{\lvert c_1 \rvert + \lvert c_2 \rvert} \lvert \lvert c_1 - c_2 \rvert \rvert ^2$$`]

--

.pull-left[
#### Gaussian Mixture Model
- 'Soft' clustering
- Models data as coming from a mixture of Gaussian distributions]

.pull-right[**Mixture of multivariate Gaussians** <br>
`$$p(x_n|\mu, \Sigma, \pi,K) = \sum_{k=1}^K{\pi_k\mathcal{N}(x_n|\mu_k, \Sigma_k)}$$`]


---





     