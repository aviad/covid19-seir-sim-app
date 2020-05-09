(defproject simple "lein-git-inject/version"

  :dependencies [[org.clojure/clojure       "1.10.1"]
                 [org.clojure/clojurescript "1.10.597"
                  :exclusions [com.google.javascript/closure-compiler-unshaded
                               org.clojure/google-closure-library
                               org.clojure/google-closure-library-third-party]]
                 [thheller/shadow-cljs      "2.8.83"]
                 [reagent                   "0.10.0"]
                 [re-frame                  "RELEASE"]

                 [thi.ng/geom "1.0.0-RC4"]
                 [aysylu/loom "1.0.2"]
                 ;; [ubergraph "0.8.2"]
                 [incanter "1.9.3"]
                 [com.taoensso/tufte "2.1.0"]
                 ]
  :dev-dependencies [[org.clojars.stumitchell/clairvoyant "0.2.1"]
                     [day8/re-frame-tracer "0.1.1-SNAPSHOT"]

                     ]
  :plugins      [[day8/lein-git-inject "0.0.11"]
                 [lein-shadow          "0.1.7"]]

  :middleware   [leiningen.git-inject/middleware]

  :clean-targets ^{:protect false} [:target-path
                                    "shadow-cljs.edn"
                                    "package.json"
                                    "package-lock.json"
                                    "resources/public/js"]

  :shadow-cljs {:nrepl  {:port 8777}
                :builds {:client {:target     :browser
                                  :output-dir "resources/public/js"
                                  :modules    {:client {:init-fn covid19.core/run}}
                                  :devtools   {:http-root "resources/public"
                                               :http-port 8280}}
                         :mobile {:target     :react-native
                                  :output-dir "resources/public/js"
                                  :modules    {:mobile {:init-fn covid19.core/run}}
                                  :devtools   {:http-root "resources/public"
                                               :http-port 8280}}}}

  :aliases {"dev-auto" ["shadow" "watch" "client"]})
