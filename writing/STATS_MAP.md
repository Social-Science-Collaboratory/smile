# Where each statistic comes from

Verify this before I write the code. Values shown are what the objects currently hold.

Sources:
- `A` = `data/main_analysis/smile25b_happy_results_anova.Rds` (15 rows, one per term)
- `S` = `data/main_analysis/smile25b_happy_results_simple.Rds` (8 rows: context x threat x repetition)
- `sec/<outcome>` = `data/secondary_analysis/smile25b_<outcome>_results_simple.Rds` (same 8 rows)
- `D` = `data/smile25b_processed_data.csv`

---

## Results, paragraph 1

| # | Placeholder | Source | Value |
|---|---|---|---|
| 1 | *N* = X | `D`, row count | 1174 |
| 2 | age *M* = X; *SD* = X | `D$Age` | 38.77; 12.70 |
| 3 | X% Male, % Female, Z% Other | `D$Gender` | 44.0% Male, 53.4% Female, 2.6% Other |
| 4 | X% White, Black, Mixed, Asian, Other | `D$Ethnicity` | 67.4, 14.9, 8.1, 5.5, 3.3 |
| 5 | pose smiles (*M* =, *SD* =) | `D$AU12_scalar_smile` | 1.58 (1.11) |
| 6 | express naturally (*M* = X, *SD* = X) | `D$AU12_scalar_natura` | 0.32 (0.58) |
| 7 | Face Movement stat | paired *t*-test on AU12, computed in the setup chunk | *t*(1154) = 36.91, *p* < .001, BF~10~ = 3.14e+193 |
| 8 | positive (*M* = X, *SD* = X) vs negative (*M* = X, *SD* = X) | `D`, `DEQ_happy_total` by `context`, collapsing pose | positive 3.39 (1.85); negative 1.72 (1.26) |
| 9 | Emotional Context *F*(X, X) = X, *p* = X, BF10 = X | `A`, term `context` | *F*(1, 1166) = 395.24, *p* < .001, BF10 = 1.60e72 |
| 10 | Face Movement x Repetition x Context *F*(X, X) = X, *p* = X, BF10 = X | `A`, term `pose:context:repetition` | *F*(1, 1166) = 7.27, *p* = .007, BF10 = 3.82 |

## Results, paragraph 2 (positive contexts)

| # | Placeholder | Source | Value |
|---|---|---|---|
| 11 | voluntary smile, positive context: *t*() = X, *p* = X, *d* = X, 95% CI [X, Y], BF10 = X | `S`, row positive / No threat / one | *t*(1166) = −0.92, *p* = .357, *d* = −0.11, BF10 = 0.12 |
| 12 | "all *t* < X, all *d* > X, all *p* < .05, all BF10 > X" | `S`, other 3 positive rows | *t* ≤ −2.63; \|*d*\| ≥ 0.31; *p* ≤ .009; BF10 ≥ 2.30 |

## Results, paragraph 3 (negative contexts)

| # | Placeholder | Source | Value |
|---|---|---|---|
| 13 | posed voluntarily: *t*(), *p*, *d*, CI, BF10 | `S`, negative / No threat / one | *t*(1166) = 2.64, *p* = .008, *d* = 0.31, BF10 = 6.22 |
| 14 | under threat of punishment: *t*(), *p*, *d*, CI, BF10 | `S`, negative / Threat / one | *t*(1166) = 2.53, *p* = .011, *d* = 0.29, BF10 = 2.41 |
| 15 | "all *t* < X, all *p* > X, all *d* < X, all BF01 > X" | `S`, 2 negative / ten rows | \|*t*\| ≤ 1.48; *p* ≥ .139; \|*d*\| ≤ 0.17; **BF01 ≥ 0.69** (see Q4) |

## Exploratory Results

| # | Placeholder | Source | Value |
|---|---|---|---|
| 16 | burnout + well-being: "all *t* < X, all *d* < X, all *p* > xx, all BF01 > X" | `sec/Burnout` + `sec/SWL`, all 16 rows | \|*t*\| ≤ 1.67; \|*d*\| ≤ 0.19; *p* ≥ .095; BF01 ≥ 2.31 |
| 17 | fear, 10 smiles, negative: *t*() = X, *d* = X, *p* < .05 | `sec/fear`, negative / ten | **two rows** (see Q2): No threat *t* = −2.50, *d* = −0.29, *p* = .013; Threat *t* = −2.91, *d* = −0.34, *p* = .004 |
| 18 | fear inconclusive (BF10 > X) | `sec/fear`, same rows | BF10 = 1.03 and 0.72 |
| 19 | anger, negative: "all *t* < X, all d < X, all *p* < .05, all BF10 > X" | `sec/anger`, 4 negative rows | *t* ≤ 2.32; *d* ≤ 0.27; *p* ≥ .020; BF10 ≤ 0.64 — **see Q5** |
| 20 | anger, positive, once and voluntarily | `sec/anger`, positive / No threat / one | *t*(1166) = 2.00, *d* = 0.23, *p* = .045, BF10 = 8.65 |
| 21 | anger, positive, ten times under threat | `sec/anger`, positive / Threat / ten | *t*(1166) = 8.15, *d* = 0.98, *p* < .001, BF10 = 9.44e8 |

## Discussion

| # | Placeholder | Source | Value |
|---|---|---|---|
| 22 | dampening effects were X% larger than boosting effects | `S`, derived | **see Q3** — mean \|*d*\| positive 0.55 vs negative 0.20 = 169% larger |
| 23 | only X% posed smiles exceeding threshold | `D$face_compliance_scalar` | 43.0% (497 / 1155 non-missing); 42.3% if denominator is all 1174 |

---

---

# Resolved

- **Q1 — Face Movement manipulation check.** Now a paired *t*-test on AU12
  (`AU12_scalar_smile` vs `AU12_scalar_natura`), computed at the top of the setup chunk,
  with a Bayes factor from `ttestBF()`. The `*F*(X, X)` in that sentence became `*t*(X)`
  to match. The `pose` row of the happiness ANOVA is no longer cited anywhere.
- **Q2 — fear.** Switched to the aggregate convention, covering both negative/ten cells.
- **Q3 — "X% larger".** Replaced with "substantially larger"; no statistic introduced.
- **Q4 — effect sizes.** All effect sizes are now the **unstandardized** mean difference
  (`estimate`), with 95% CIs from `estimate ± qt(.975, df) * SE`. The label is set upright
  (`d`, not `*d*`) throughout, since these are not Cohen's *d*. The `effect.size` column in
  the objects is no longer used — rename the label if you want something clearer than `d`.
- **BF subscripts.** Written `BF~10~` / `BF~01~`, which render as BF₁₀ / BF₀₁.

# Still worth your attention

- **Negative/ten happiness cells (#15).** Renders "all BF~01~ > 0.69". One cell is
  BF01 = 8.50, the other 0.69 (BF10 = 1.44, weak evidence *for* an effect), so the bound
  is uninformative even though the sentence claims null results in both.
- **Anger in negative contexts (#19).** The inequalities had to be flipped to stay true:
  it now reads "all *t* < 2.33, all d < 0.23, all *p* > .020, all BF~10~ < 0.64". The
  original "all *p* < .05" contradicted "little-to-no shift". Note the negative/Threat/ten
  cell is *t* = 2.32, *p* = .020 — a real increase that the sentence doesn't cover.
- **Positive-context decreases (#12).** "all *d* > X" became "all d < -0.24", since the
  effects are negative.
- **Ethnicity percentages** put the 8 missing values and the 9 "Prefer not to say"
  responses into "Other" (4.8%), so the five categories sum to 100.
