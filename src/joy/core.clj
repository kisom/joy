(ns joy.core
  (:gen-class))

(defn moo
  "MOO COW SAYS MOO"
  []
  (println "moo!"))

(defn -main [& args]
  (moo))