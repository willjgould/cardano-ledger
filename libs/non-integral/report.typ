#let project(title: "", body) = {
  set document(title: title)
  set page(numbering: "1", number-align: center)
  set text(font: "Linux Libertine", lang: "en")
  align(center)[#block(text(weight: 700, 1.75em, title))]
  body
}

#show: project.with(
  title: "Benchmark Results Summary",
)

== exp'/small

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000007], [seconds],
  [Std Dev], [0.000000], [seconds],
  [Sample Size], [171], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [0],
  [Low Severe], [0],
  [High Mild], [1],
  [High Severe], [0],
)

Total samples seen: 42

== exp'/one

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000012], [seconds],
  [Std Dev], [0.000000], [seconds],
  [Sample Size], [160], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [0],
  [Low Severe], [0],
  [High Mild], [0],
  [High Severe], [0],
)

Total samples seen: 43

== exp'/medium

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000011], [seconds],
  [Std Dev], [0.000001], [seconds],
  [Sample Size], [162], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [0],
  [Low Severe], [0],
  [High Mild], [0],
  [High Severe], [0],
)

Total samples seen: 44

== exp'/medium_large

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000012], [seconds],
  [Std Dev], [0.000000], [seconds],
  [Sample Size], [161], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [0],
  [Low Severe], [0],
  [High Mild], [1],
  [High Severe], [0],
)

Total samples seen: 43

== exp'/large

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000012], [seconds],
  [Std Dev], [0.000000], [seconds],
  [Sample Size], [160], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [3],
  [Low Severe], [1],
  [High Mild], [0],
  [High Severe], [0],
)

Total samples seen: 43

== ln'/small

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000029], [seconds],
  [Std Dev], [0.000000], [seconds],
  [Sample Size], [142], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [1],
  [Low Severe], [0],
  [High Mild], [1],
  [High Severe], [0],
)

Total samples seen: 42

== ln'/one

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000013], [seconds],
  [Std Dev], [0.000000], [seconds],
  [Sample Size], [159], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [2],
  [Low Severe], [0],
  [High Mild], [0],
  [High Severe], [0],
)

Total samples seen: 42

== ln'/medium

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000014], [seconds],
  [Std Dev], [0.000000], [seconds],
  [Sample Size], [157], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [0],
  [Low Severe], [0],
  [High Mild], [0],
  [High Severe], [0],
)

Total samples seen: 43

== ln'/medium_large

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000027], [seconds],
  [Std Dev], [0.000001], [seconds],
  [Sample Size], [144], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [0],
  [Low Severe], [0],
  [High Mild], [0],
  [High Severe], [0],
)

Total samples seen: 43

== ln'/large

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000028], [seconds],
  [Std Dev], [0.000001], [seconds],
  [Sample Size], [143], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [0],
  [Low Severe], [0],
  [High Mild], [0],
  [High Severe], [0],
)

Total samples seen: 43

== exponentiation/small_int

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000028], [seconds],
  [Std Dev], [0.000000], [seconds],
  [Sample Size], [143], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [0],
  [Low Severe], [0],
  [High Mild], [0],
  [High Severe], [0],
)

Total samples seen: 42

== exponentiation/small_frac

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000026], [seconds],
  [Std Dev], [0.000000], [seconds],
  [Sample Size], [145], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [5],
  [Low Severe], [0],
  [High Mild], [2],
  [High Severe], [1],
)

Total samples seen: 43

== exponentiation/medium

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000041], [seconds],
  [Std Dev], [0.000001], [seconds],
  [Sample Size], [135], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [0],
  [Low Severe], [0],
  [High Mild], [2],
  [High Severe], [0],
)

Total samples seen: 42

== exponentiation/large

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000044], [seconds],
  [Std Dev], [0.000001], [seconds],
  [Sample Size], [134], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [0],
  [Low Severe], [0],
  [High Mild], [0],
  [High Severe], [0],
)

Total samples seen: 43

== findE/small

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000000], [seconds],
  [Std Dev], [0.000000], [seconds],
  [Sample Size], [262], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [0],
  [Low Severe], [0],
  [High Mild], [4],
  [High Severe], [0],
)

Total samples seen: 43

== findE/small_frac

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000000], [seconds],
  [Std Dev], [0.000000], [seconds],
  [Sample Size], [262], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [1],
  [Low Severe], [0],
  [High Mild], [0],
  [High Severe], [0],
)

Total samples seen: 42

== findE/medium

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000001], [seconds],
  [Std Dev], [0.000000], [seconds],
  [Sample Size], [221], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [0],
  [Low Severe], [0],
  [High Mild], [0],
  [High Severe], [1],
)

Total samples seen: 42

== findE/large

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000001], [seconds],
  [Std Dev], [0.000000], [seconds],
  [Sample Size], [213], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [0],
  [Low Severe], [0],
  [High Mild], [0],
  [High Severe], [0],
)

Total samples seen: 42

== taylorExpCmp/small

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000000], [seconds],
  [Std Dev], [0.000000], [seconds],
  [Sample Size], [254], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [0],
  [Low Severe], [0],
  [High Mild], [1],
  [High Severe], [0],
)

Total samples seen: 42

== taylorExpCmp/medium

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000000], [seconds],
  [Std Dev], [0.000000], [seconds],
  [Sample Size], [241], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [2],
  [Low Severe], [6],
  [High Mild], [3],
  [High Severe], [0],
)

Total samples seen: 41

== taylorExpCmp/large

=== Analysis

#table(
  columns: (auto, auto, auto),
  [Metric], [Value], [Unit],
  [Mean], [0.000001], [seconds],
  [Std Dev], [0.000000], [seconds],
  [Sample Size], [212], [measurements],
)

=== Outliers

#table(
  columns: (auto, auto),
  [Category], [Count],
  [Low Mild], [0],
  [Low Severe], [0],
  [High Mild], [0],
  [High Severe], [0],
)

Total samples seen: 43

