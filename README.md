# Reproducibility Guide

The full dataset of shell aliases is available at <https://doi.org/10.5281/zenodo.3778824>.

If you just want to re-run the analysis or explore the dataset, you can skip ahead to step 4 below.

1. **Collection**

    We used the [github-searcher] tool to collect all Shell language files under 29 KB containing the string `alias` from GitHub. This data collection was carried out over a period of two-and-a-half weeks from December 20th 2019 to January 8th 2020. The file `sampling.csv` contains the number of files obtained and the estimated population (as reported by GitHub) of each sampled file size stratum.

    The resulting [SQLite] database containing all collected files is available as `results_unparsed.db` in the [Zenodo] archive.
    The database schema is given in the paper.

2. **Pruning**

    The [Jupyter] notebook at `notebooks/dataset.ipynb` contains the data pruning steps. If you want to repeat these exactly, copy `results_unparsed.db` to `notebooks/results.db` and execute the notebook.

3. **Parsing**

    We used the Haskell script in the `parser` directory to parse the raw file contents to find actual alias definitions and decompose them into their constituent parts. The parser works on the database produced by [github-searcher] and extends it by adding tables for aliases, commands, and arguments. Parsing the pruned database takes about 1 hour on a 2019 iMac.

    After parsing, we removed files that did not contain any aliases from the database. (The SQL commands for this are in `notebooks/dataset.ipynb`).

4. **Analysis**

    We used [Jupyter] notebooks to carry out most of the data analysis. You can find these in the `notebooks` directory of this repository. Our environment was Python 3.7.6, with the dependencies listed in the `notebooks/requirements.txt`.

    If you just want to re-run the analysis or explore the dataset, simply copy `results.db` from the [Zenodo] archive into `notebooks/results.db` and play around with the notebooks (`notebooks/summary.ipynb` is a good starting point).

[github-searcher]: https://github.com/ipa-lab/github-searcher
[SQLite]: https://www.sqlite.org
[Zenodo]: https://doi.org/10.5281/zenodo.3778824
[Jupyter]: https://jupyter.org
