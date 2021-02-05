QuAX-DaF
================

## Description

  - QuAX-DaF stands for **Qu**antitative **A**nalyse von Te**X**ten für
    **D**eutsch **a**ls **F**remdsprache (“Quantitative Analysis of
    Texts for German as a Foreign Language”).
  - QuAX-DaF is an interactive web application that analyzes words in
    texts, compares their frequency distribution to German language use,
    and generates teaching material for exercises at specific
    proficiency levels.
  - QuAX-DaF will be of use to teachers of German as a foreign language
    and developers of teaching material.

A detailed documentation in German including linguistic background and
suggestions for exercises is available
[here](https://daniel-jach.github.io/quax-daf/documentation/quax-daf-documentation.pdf).

QuAX-DaF is available for use
[here](https://danieljach.shinyapps.io/quax-daf/).

The application is written in *R* (R Core Team 2013) and uses *shiny*
(Chang et al. 2020) and *TreeTagger* (Schmid 1994).

## Updates

  - **2021-02-05**
      - Retain formatting (line breaks) of user input text in output
        material; may affect parsing results (e.g., for cross headings)
      - Handle apostrophes (unterminated single quotes) in user input
        text
      - Other minor improvements

## Author

Daniel Jach \<danieljach@protonmail.com\>

## License and Copyright

© Daniel Jach, Shanghai Normal University, China

Licensed under the [MIT License](LICENSE).

## References

<div id="refs" class="references">

<div id="ref-Chang.2020">

Chang, Winston, Joe Cheng, JJ Allaire, Yihui Xie, and Jonathan
McPherson. 2020. *Shiny: Web Application Framework for R*.
<https://CRAN.R-project.org/package=shiny>.

</div>

<div id="ref-RCT.2013">

R Core Team. 2013. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-Schmid.1994">

Schmid, Helmut. 1994. “Probabilistic Part-of-Speech Tagging Using
Decision Trees.” In *Proceedings of International Conference on New
Methods in Language Processing*. Manchester, England.
<http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/>.

</div>

</div>
