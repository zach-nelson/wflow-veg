# wflow-veg

A [workflowr][] project.

[workflowr]: https://github.com/jdblischak/workflowr

This repository contains a `targets` data pipeline within a `workflowr` project. The pipeline reads in line point data from two agencies, merges after processing and appends to the master file. Further summaries are computed and statistical tests of vegetation change are conducted and displayed in tables and maps. This pipeline makes transparent how the measurability analysis is currently carried out for section I.C.1.a. of the Green Book, Determining Measurability, an annual activity of the Long Term Water Agreement. Specifically methods described in Box I.C.1.a.ii are used in the pipeline to formalize a reproducible process for the Technical Group's annual evaluation.
