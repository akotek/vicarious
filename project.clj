(defproject vicarious "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/test.check "1.0.0"]
                 [org.clojure/test.generative "1.0.0"]
                 [net.mikera/core.matrix "0.62.0"]
                 [net.mikera/vectorz-clj "0.48.0"]
                 [incanter "1.9.3"]
                 [metasoarous/oz "1.5.0"]
                 [org.jsoup/jsoup "1.7.3"]]
  :repl-options {:init-ns vicarious.core})
