# A Spatiotemporal Analysis of Teacher Practices in Supporting Student Learning and Engagement in an AI-enabled Classroom

Supplementary repository for the AIED '23 full paper "A Spatiotemporal Analysis of Teacher Practices in Supporting Student Learning and Engagement in an AI-enabled Classroom".

## Citation

Karumbaiah, S., Borchers, C., Shou, T., Falhs, A.-C., Liu, P., Nagashima, T., Rummel, N., & Aleven, V. (2023). A Spatiotemporal Analysis of Teacher Practices in Supporting Student Learning and Engagement in an AI-enabled Classroom. In Proceedings of the 24th International Conference on Artificial Intelligence in Education (AIED). Tokyo, Japan.

```
@inproceedings{karumbaiah2023spatiotemporal,
  title={A Spatiotemporal Analysis of Teacher Practices in Supporting Student Learning and Engagement in an AI-enabled Classroom},
  author={Karumbaiah, Shamya and Borchers, Conrad and Shou, Tianze and Falhs, Ann-Christin and Liu, Cindy and Nagashima, Tomohiro and Rummel, Nikol and Aleven, Vincent},
  booktitle={AIED23: 24th International Conference on Artificial Intelligence in Education},
  year={2023}
}
```

## Folder structure

`main.R` includes the main analysis script in R.

`utils.R` includes common functions referenced throughout `main.R`.

## Data files referenced in script

`events_file_aied23.txt` includes timestamped teacher and student events (e.g., teacher visits, student tutor actions), including a start and end time expressed in Unix time and the actor and subject of the event.

`pre_post_scores.txt` includes students' test scores featuring one row per student.

`crosswalk.txt` is a simple mapping including two columns to link student IDs across data sets.

`tutor_logs.txt` contains the tutor interaction data.

`detector_output.txt` includes timestamped tutor interaction events, including student, period, and day IDs, to link them to `analytics_data_aied23.csv`. The inferred degree of struggle, idleness, misuse, and gaming is given for each interaction. Please consult [this GitHub repository](https://github.com/d19fe8/CTAT-detector-plugins) for more information.

## Data availability

Please request the study data via CMU DataShop through this work's [project page](https://pslcdatashop.web.cmu.edu/Project?id=879) subject to CMU DataShop's terms of use.
