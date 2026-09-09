# Code review checklist

## Notes from Nick

- **Leave `writing/` for last.**

- **Consider having Claude build a temporary "mermaid figure" first.**

- **Annabel's Note: Consider switching to Rmd? Would make it easier to understand (e.g., sensitivity analyses  in the same document as the main analyses)**

## Files

- [x] `smile25b_process_data.R` 
- [x] `smile25b_run_openface.R`
  - Removed temporary test and restored the intended folder that was commented out
- [x] `smile25b_compile_openface.R`
- [x] `smile25b_functions.R`
- [x] `smile25b_main_analysis.R`
- [x] `smile25b_secondary_analysis.R`
- [x] `power simulation/smile_power_simulation.Rmd`
  - Set up directory
- [x] `writing/smile_R3_writing.qmd`
  - Edited a Fig.1 and Fig.2 description where "neutral expression" was used instead of "natural expression"
  - Should we include any Cronbach alphas for the emotion items?
  - Add a "See SI" to the paragraph discussing sensitivity analyses
  - BF01 was used twice, every other time we used BF10. Consider just keeping it consistent with BF10?
  - Discuss the switch to OpenFace instead of OpenAI in methodology? Not sure if there is any significant difference in the change between programs.
- [x] `writing/smile_R3_SI.qmd`
  - Clarify additional aspects of sensitivity checks, I was a bit confused by the exclude/include weak smiles since I thought it was a comparison of sensitivity and full analysis
    - made no math errors: math_errors == 0
    - were unaware of the study's true purpose: joint_awareness_check == FALSE

