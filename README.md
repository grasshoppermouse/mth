
# mth

<!-- badges: start -->
<!-- badges: end -->

This repo contains the `R` and `python` code to create this wordcloud of the top 200 most frequent words in Man the Hunter (Lee and DeVore, 1968):

![](wordcloud.svg)

## How to use

Download the repo. To install the `python` dependencies, first install [uv](https://docs.astral.sh/uv/) (a python package and project manager). Then in a terminal:

```
cd /path/to/mth
uv sync
uv run python -m spacy download en_core_web_sm
```

Then run the `R` code in `wordcloud.R` (first install any missing packages), which will create the file `wordcloud.pdf`:

``` r
source("wordcloud.R")
```

## Notes

* The `python` code is called from the `R` script, using the `reticulate` package.
* Running the `R` code in RStudio will cause a crash. Use [Positron](https://positron.posit.co) or a terminal.
* The text of *Man the Hunter* was obtained from the [Internet Archive](https://archive.org/details/ManTheHunter), and will be downloaded automatically by the `R` script.
* The text contains misc errors, presumably due to errors in converting the original to text.
* Misc text cleaning included an attempt to "de-hyphenate" words hyphenated at the ends of lines, e.g., "compara-tive".
* Stop words removed using the `tidytext` package.
* Some stop words in the `tidytext` package, specifically "man", "men", "group", "groups", "area", and "areas", were *not* removed.
* Words were [lemmatized](https://en.wikipedia.org/wiki/Lemmatization) using the [spaCy](https://spacy.io) python package.
* Word embeddings were computed using the GloVe algorithm from the [`text2vec`](https://text2vec.org/glove.html) package.
* Reduction of the high dimensional word embedding space to 2D for visualization was done with [PaCMAP](https://github.com/YingfanWang/PaCMAP).
* The wordcloud was plotted using the `ggrepel` package.
* Neither the `PaCMAP` nor `ggrepel` algorithms are deterministic. A seed was set for reproducibility, but differences in hardware and operating systems can cause differences in the final appearence of the wordcloud.