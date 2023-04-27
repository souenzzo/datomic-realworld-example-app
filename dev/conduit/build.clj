(ns conduit.build
  (:require [clojure.tools.build.api :as b]))

(def class-dir "target/classes")

(def *basis
  (delay (b/create-basis {:project "deps.edn"})))

(def *rev-count
  (delay (b/git-count-revs nil)))

(defn -main
  [& _]
  (b/delete {:path "target"})
  (b/write-pom {:class-dir class-dir
                :lib       'conduit/conduit
                :version   (str "0.0." @*rev-count)
                :basis     @*basis
                :src-dirs  ["src"]})
  (b/compile-clj {:basis     @*basis
                  :src-dirs  ["src"]
                  :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file "target/conduit.jar"
           :basis     @*basis
           :main      'conduit.main}))
