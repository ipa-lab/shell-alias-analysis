# Reproducibility Guide

The full dataset of shell aliases is available at <https://doi.org/10.5281/zenodo.3778824>.

We used [Jupyter] notebooks to carry out most of the data analysis. You can find these in the `notebooks` directory of this repository. Our environment was Python 3.7.6, with the dependencies listed in `notebooks/requirements.txt`.

If you just want to re-run the analysis or explore the dataset, you can skip ahead to step 4 below.

1. **Collection**

    We used the [github-searcher] tool to collect all Shell language files under 29 KB containing the string `alias` from GitHub. This data collection was carried out over a period of two-and-a-half weeks from December 20th 2019 to January 8th 2020. The file `sampling.csv` contains the number of files obtained and the estimated population (as reported by GitHub) of each sampled file size stratum. Additionally, `notebooks/sampling.ipynb` contains some helper code used during the data collection process.

    The resulting [SQLite] database containing all collected files is available as `results_unparsed.db` in the [Zenodo] archive.
    The database schema is given in the paper.

2. **Pruning**

    The notebook at `notebooks/dataset.ipynb` contains the data pruning steps. If you want to repeat these exactly, copy `results_unparsed.db` to `notebooks/results.db` and execute the notebook.

3. **Parsing**

    We used the Haskell script in the `parser` directory to parse the raw file contents to find actual alias definitions and decompose them into their constituent parts. The parser works on the database produced by [github-searcher] and extends it by adding tables for aliases, commands, and arguments. Parsing the pruned database takes about 1 hour on a 2019 iMac.

    After parsing, we removed files that did not contain any aliases from the database. (The SQL commands for this are in `notebooks/dataset.ipynb`).

4. **Analysis**

    All data analysis was performed on the parsed and pruned dataset, available as `results.db` in the [Zenodo] archive. You can copy it to `notebooks/results.db` to follow along. In addition to the notebooks, there are some ancillary files, such as the results of our coding process, all of which you can also find in the `notebooks` directory.

    If you want to re-run the analysis or explore the dataset, `notebooks/summary.ipynb` is a good starting point.


[github-searcher]: https://github.com/ipa-lab/github-searcher
[SQLite]: https://www.sqlite.org
[Zenodo]: https://doi.org/10.5281/zenodo.3778824
[Jupyter]: https://jupyter.org
