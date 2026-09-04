# Reference issues in `smile_R2_writing_full.docx`

Flagged during the conversion to `smile_R3.qmd`. Items in §1 are **resolved** in the
.qmd; items in §2 are **still open** and need your input. No manuscript prose was
rewritten for any of these — only citation targets changed.

---

## 1. Resolved

### 1a. Coles et al. → 2022 (approved)

| Was | Now | Location |
|---|---|---|
| "a large-scale adversarial collaboration (Coles et al., **2023**)" | `[@coles2022]` | Discussion, para. 3 |
| "one of the largest experiments on emotion embodiment (Coles et al., **2021**)" | `[@coles2022]` | Limitations, para. 2 |
| "As a control condition, Coles et al. (**2021**) had participants…" | `@coles2022` | Limitations, para. 2 |

Coles et al. (2021) had no matching entry at all; Coles et al. (2023) is the JPSP
"Fact or artifact?" paper, which is still correctly cited in the Intro, Method, and
the last Limitations paragraph.

### 1b. Year mismatches (approved)

Each now renders with the reference-list year rather than the year typed in the R2 text.

| In R2 text | Renders as |
|---|---|
| Cherry, **2024** (Intro, para. 2) | Cherry, 2014 |
| Efthimiou, Baker, Elsenaar, Mehu, & Korb, **2025** | Efthimiou, Baker, Elsenaar, et al., 2024 |
| Rohrer, **2023** (Primary Analyses) | Rohrer, 2024 |

### 1c. Uncited works removed (approved)

These 11 entries were in the R2 reference list but never cited in the text. They have
been **deleted from `references.bib`** (69 entries remain, all cited):

Berkowitz (1990) · Coan, Allen, & Harmon-Jones (2001) · Coles & Frank (2023) ·
Duclos & Laird (2001) · Gellhorn (1964) · Grandey & Melloy (2017) · Laird (1974) ·
Laird & Strout (2007) · Levenson, Ekman, & Friesen (1990) · Zajonc (1985) ·
Zajonc, Murphy, & Inglehart (1989)

Two of these are near-misses worth noting: the text cites **Coles, Wyatt, & Frank
(2025)**, not Coles & Frank (2023); and **Levenson et al. (1992)**, not Levenson et
al. (1990). Both cited versions are retained.

### 1d. Gross (1998)

Cited in the Introduction ("expressive suppression … e.g., Gross, 1998") but **absent
from the R2 reference list**. It was already in `writing/r-references.bib`, so it is
cited as `@gross1998` and now renders. Confirm it is the intended source: Gross, J. J.
(1998), *Review of General Psychology, 2*(3), 271–299, "The emerging field of emotion
regulation: An integrative review".

---

### 1e. Missing citations added (verified against Crossref)

The five citations that had no bibliography entry are now resolved. Every field was checked
against the Crossref API and a second independent source.

| Key | Reference |
|---|---|
| `hager1981` | Hager & Ekman (1981), *JPSP*, 40(2), 358–362 |
| `tomkins1981` | Tomkins (1981), *JPSP*, 40(2), 355–357 |
| `matsumoto1987` | Matsumoto (1987), *JPSP*, 52(4), 769–774 |
| `ekman1993` | Ekman (1993), *American Psychologist*, 48(4), 384–392 |
| `baltrusaitis2018` | Baltrušaitis, Zadeh, Lim, & Morency (2018), *FG 2018*, 59–66 |

Two corrections made along the way:

- **Tomkins (1981) volume/pages.** The citation supplied read "37, 1519–1531", which is
  actually Tourangeau & Ellsworth's (1979) own volume and page range — a known error that
  propagates through several reference lists. Crossref confirms the reply is **40(2),
  355–357**.
- **Levenson et al. (1992) pages.** The existing entry had `972` only; corrected to
  **972–988**.
- **Tomkins author name.** `tomkins1962` read "Tomkins, Silvan" while `tomkins1981` read
  "Tomkins, Silvan S.". APA treats differing initials as different authors, so in-text
  citations were rendering as "S. S. Tomkins, 1981". Normalized to "Silvan S." in both.

### 1f. OpenFace sentence

The Limitations sentence now reads "we used OpenFace 2.2.0 instead of HumeAI for facial
expression analysis [@baltrusaitis2018]" — the empty `()` is gone. Note the manuscript
previously said "OpenFaceR"; `smile25b_run_openface.R` calls the OpenFace 2.2.0
`FeatureExtraction` binary directly via `system2()`, so there is no R wrapper package
involved.

## 2. Still open

### 2b. Author list

Taken from `writing/smile_writing.Rmd`, since `smile_R2_titlepage.docx` contains only the
correspondence note. Verify it is current.

### 2c. Hume AI

Referred to in the Method as a bare URL (`https://www.hume.ai/`) rather than a citation.
The URL is preserved verbatim; the entry (Hume AI Inc., 2025) is held in the reference list
via `nocite:` in the YAML. It has no title, so it renders sparsely — matching the R2 list,
which also has none.
