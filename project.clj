;;   Copyright (c) 7theta. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://www.eclipse.org/legal/epl-v10.html)
;;   which can be found in the LICENSE file at the root of this
;;   distribution.
;;
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any others, from this software.

(defproject com.7theta/distantia "0.2.2"
  :description "diff and patch implementations for clojure data structures"
  :url "https://github.com/7theta/distantia"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:dev {:global-vars {*warn-on-reflection* true}
                   :plugins [[lein-cljsbuild "1.1.7"]
                             [lein-doo "0.1.7"]]
                   :dependencies [[org.clojure/clojure "1.10.1"]
                                  [org.clojure/clojurescript "1.10.520"]
                                  [org.clojure/tools.namespace "0.3.1"]
                                  [org.clojure/test.check "0.10.0"]
                                  [com.gfredericks/test.chuck "0.2.10"]]
                   :source-paths ["dev"]}}
  :clean-targets ^{:protect false} ["out" "target"]
  :cljsbuild {:builds [{:id "test"
                        :source-paths ["src" "test"]
                        :compiler {:output-to "out/testable.js"
                                   :main distantia.test-runner
                                   :optimizations :advanced}}]}
  :scm {:name "git"
        :url "https://github.com/7theta/distantia"})
