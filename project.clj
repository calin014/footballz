(defproject footballz "0.1.0-SNAPSHOT"
  :description "Footballz, a multiplayer game."
  :url "http://github.com/calin014/footballz"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.521"]
                 [org.clojure/core.async "0.3.442" :exclusions [org.clojure/tools.reader]]
                 [org.clojure/core.memoize "0.5.8"]
                 [reagent "0.6.1"]
                 [com.taoensso/sente "1.11.0"]
                 [environ "1.0.2"]
                 [http-kit "2.2.0"]
                 [compojure "1.5.2"]
                 [ring "1.5.1"]
                 [ring/ring-defaults "0.2.3"]
                 [ring-cors "0.1.7"]]

  :plugins [[lein-figwheel "0.5.10"]
            [lein-cljsbuild "1.1.5" :exclusions [[org.clojure/clojure]]]
            [lein-environ "1.0.2"]]

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :main footballz.server.main

  :uberjar-name "footballz-standalone.jar"

  :profiles
  {:dev {:env {:dev? "true"}
         :cljsbuild {:builds
                     [{:id "dev"
                       :source-paths ["src" "dev"]
                       :figwheel {}
                       :compiler {:main footballz.client.main
                                  :asset-path "js/compiled/out"
                                  :output-to "resources/public/js/compiled/footballz.js"
                                  :output-dir "resources/public/js/compiled/out"
                                  :source-map-timestamp true}}]}}
   :uberjar {:hooks [leiningen.cljsbuild]
             :aot :all
             :cljsbuild {:builds
                         [{:id "min"
                           :source-paths ["src" "prod"]
                           :compiler {:main footballz.client.main
                                      :output-to "resources/public/js/compiled/footballz.js"
                                      :optimizations :advanced
                                      :pretty-print false}}]}}}

  :figwheel {:css-dirs ["resources/public/css"]})
