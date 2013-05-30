(ns treegen.core
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [fipp.printer :refer (pprint-document defprinter)])
  (:gen-class))

(defn pretty-tree [tree]
  (if (= (count tree) 3)
    (let [[node sub1 sub2] tree]
      [:group (name node)
       [:nest 2
        :break (pretty-tree sub1)
        :break (pretty-tree sub2)]])
    (let [[node term] tree]
      [:span (name node) " → " (name term)])))

(defprinter print-tree pretty-tree
  {:width 70})

(defn build-nodeo [max-len nonterm-rules term-rules]
  (letfn [(nonterm-ruleo [from to1 to2]
            (l/membero [from to1 to2]
                       (for [[k [v1 v2]] nonterm-rules] [k v1 v2])))

          (term-ruleo [from to]
            (l/membero [from to] (into [] term-rules)))

          (node-nontermo [node terms trace n]
            (l/fresh [sub1 sub2 terms1 terms2 trace1 trace2 n1 n2]
                       (nonterm-ruleo node sub1 sub2)
                       (fd/in n1 n2 (fd/interval 1 max-len))
                       (fd/+ n1 n2 n)
                       (l/appendo terms1 terms2 terms)
                       (l/== trace [node trace1 trace2])
                       (nodeo sub1 terms1 trace1 n1)
                       (nodeo sub2 terms2 trace2 n2)))

          (node-termo [node terms trace n]
            (l/fresh [term]
                       (term-ruleo node term)
                       (fd/== n 1)
                       (l/== trace [node term])
                       (l/== terms [term])))

          (nodeo [node terms trace n]
            (l/conde
             [(node-nontermo node terms trace n)]
             [(node-termo node terms trace n)]))]
    nodeo))

(defn build-trees [nonterm-rules term-rules start-node terms]
  (let [nodeo (build-nodeo 100 nonterm-rules term-rules)]
    (l/run* [tree] (nodeo start-node terms tree (count terms)))))

(comment
  (->> (build-trees {:a [:b :c]
                     :b [:b :b]
                     :c [:b :c]}
                    {:b :b-term
                     :c :c-term}
                    :a
                    [:b-term :b-term :b-term :c-term])
       (map print-tree)
       (dorun)))

(defn -main
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "This program is intended to be used from nrepl at this point"))
