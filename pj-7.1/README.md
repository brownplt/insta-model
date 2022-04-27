pj-7.1
===

Programming Journal, Volume 7 Issue 1

Deadline: 2021-02-01 AoE

<https://programming-journal.org/timeline/>


Perspectives = Art, Science (Empirical, Theoretical), Engineering

<https://programming-journal.org/cfp/>


22 pages excluding bib

<https://programming-journal.org/submission/>


#### Diff

To make a LaTeX diff:

```
  latexdiff -c ldiff.cfg --exclude-text="section,subsection,subsubsection" programming-submission/paper.tex paper.tex > diff/diff.tex 
```

The config file (`ldiff.cfg`) ignores lstlisting environments.

To build the diff:

```
  cd diff; make full
```

