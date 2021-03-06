(defproject foppl "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"] [anglican "1.1.0"] [org.clojure/data.json "0.2.1"] [org.clojure/core.match "1.0.0"] [distributions "0.1.2"] [incanter/incanter-core "1.9.3"] [incanter/incanter-charts "1.9.3"]]
  :main ^:skip-aot foppl.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}}
  :plugins [[cider/cider-nrepl "0.26.0"]])
