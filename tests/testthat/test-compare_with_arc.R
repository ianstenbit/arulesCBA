library("testthat")
library("arulesCBA")
library("arc")
data("iris")

context("Compare with arc")

cba_classifier <- CBA(Species ~ ., iris, supp = 0.05, conf=0.9, lhs.support=FALSE)

arc_classifier <- arc::cba(iris, classAtt = "Species",
  rulelearning_options =
    list(minconf = 0.9, minsupp= 0.05, maxtime=1000, target_rule_count=50000, minlen=1, maxlen = 10,
      trim=TRUE, find_conf_supp_thresholds=FALSE))

inspect(rules(cba_classifier))
inspect(arc_classifier@rules)


expect_equal(length(rules(cba_classifier)), length(arc_classifier@rules))
expect_equal(lhs(rules(cba_classifier)), lhs(arc_classifier@rules))

