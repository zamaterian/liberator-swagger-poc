(ns common.swagger-test
  (:require [clojure.test :refer :all]
            [common.swagger :refer :all]
            [compojure.core :as cc :refer [defroutes ANY]]
            [liberator.core :refer [resource defresource]]
            [schema.core :as s]
            [cheshire.core :as json]
            [compojure.api.sweet :refer :all]
            [ring.mock.request :as mock]))



; ############################################################
;  common liberator resource base.
;  for authenicated resources
; ############################################################

(def test-auth-base-api
  [:service-available? workaround-missing-mediatype
   :as-response as-response-swagger
   :available-media-types (conj available-media-types "application/json")
   :handle-exception exception-handler
   :malformed? parse-and-coerce-body
   :handle-malformed (fn [ctx] (:common.swagger/error-data ctx))
   :available-charsets  ["utf-8"]])


; ############################################################
; our microservices routes and schemaes
; ############################################################

(s/defschema TEST2 {:test-id Number})
(s/defschema JUJU {:juju-id String})


(rest-api route-juju "/juju" []
          test-auth-base-api
          [:allowed-methods  [:get]
           :exists? (fn [_] {::entity {:juju-id "juju"}})
           :handle-ok ::entity]
          [:header-params [x-callid :- String, {x-user :- String ""} authorization :- String]
           :return JUJU
           :responses {400 {:schema error-schema}
                       404 nil}])

(rest-api route-exception "/exception" []
          test-auth-base-api
          [:allowed-methods  [:get]
           :exists? (fn [_] {::entity {:juju-id "juju"}})
           :handle-ok (fn [_] (throw (Exception. "dummy exception")))]
          [:header-params [x-callid :- String, {x-user :- String ""} authorization :- String]
           :return JUJU
           :responses {400 {:schema error-schema}
                       404 nil}])

(rest-api route-ejendom "/ejendom/:id" [id]
          test-auth-base-api
          [:allowed-methods  [:get :put]
           :exists? (fn [ctx] {::entity  [ id ]})
           :handle-ok ::entity
           :handle-created :common.swagger/data]
          [:path-params [id :- Long]
           :header-params [x-callid :- String, {x-user :- String ""} authorization :- String]
           :summary  "Service beskrivelse"
           :body [body TEST2]
           :x-roles ["update-role-1" "hent-role-1"]
           :return TEST2
           :responses {201 {:schema TEST2}
                       400 {:schema error-schema}
                       404 nil}
           :description "Update and retrive an ejendom"
           :tags ["ejendom"]])

(defroutes* app
  system-application
  route-juju
  route-exception
  route-ejendom
  )



; ############################################################
; the microservice handler
; ############################################################
(defapi handler
  {:formats [] }
  (middlewares  []
               (swagger-ui)
               (swagger-docs
                 {:info swagger-info-map
                  :tags [{:name "system", :description "system services"}]})
               app))

; #####################################################################################
; #####################################################################################

(defn ddbug [x]
  (prn "dbug" x)x)

(defn transform-response [response]
  (try
    (-> response
        (assoc :headers (dissoc (:headers response) "Vary" "Content-Length"))
        (update-in [:body] (comp #(json/parse-string % true) (if (instance? java.io.BufferedInputStream (:body response))
                                                               slurp
                                                               #(or % ""))))
        )
    (catch Exception e (clojure.stacktrace/print-cause-trace  e) (prn "XXXX") (throw e))))

;"Tests that swagger meta info is not lost for each route"
(deftest get-swagger-son
  (is (= (sort (map name  (-> (mock/request :get "/swagger.json")
                              (mock/header "Accept" "application/json")
                              (mock/header "Content-Type" "application/json")
                              handler
                              transform-response
                              :body
                              (dissoc :produces :consumes :definitions :tags)
                              :paths
                              keys
                              )))
         (sort (list
                 "system/echo"
                 "system/discover"
                 "system/ping"
                 "exception"
                 "juju"
                 "ejendom/{id}")))))

(deftest  get-system-echo
  (is (= (-> (mock/request :get "/system/echo")
             (mock/header "Accept" "application/json")
             (mock/header "Content-Type" "application/json")
             handler
             transform-response
             (select-keys [:status :body]))
         {:status 200
          :body {:context "/system" :path-info "/echo" :uri "/system/echo"
                 :remote-addr "localhost", :params {}, :route-params {},
                 :headers {:content-type "application/json", :accept "application/json", :host "localhost"},
                 :server-port 80, :form-params {}, :query-params {},
                 :server-name "localhost", :query-string nil, :scheme "http", :request-method "get"}})))

(deftest  get-system-discover
  (is (= (-> (mock/request :get "/system/discover")
             (mock/header "Accept" "application/json")
             (mock/header "Content-Type" "application/json")
             handler
             transform-response
             (select-keys [:status :body]))
         {:status 200
          :body {:version "1.0" :service "service-name"}})))

(deftest  get-system-ping
  (is (= (-> (mock/request :get "/system/ping")
             (mock/header "Accept" "application/json")
             (mock/header "Content-Type" "application/json")
             handler
             (select-keys [:status :body]))
         {:status 200
          :body "OK"})))

(deftest  get-route-with-wrong-http-verb
  (is (= (-> (mock/request :post "/exception")
             (mock/header "x-callid" "dummy-callid")
             (mock/header "Accept" "application/json")
             (mock/header "Content-Type" "application/json")
             (mock/header "authorization" "dummy-header")
             handler
             (select-keys [:status :body]))
         {:status 405
          :body "Method not allowed."})))

(deftest  get-route-with-an-exception
  (is (= (-> (mock/request :get "/exception")
             (mock/header "x-callid" "dummy-callid")
             (mock/header "Accept" "application/json")
             (mock/header "Content-Type" "application/json")
             (mock/header "authorization" "dummy-header")
             handler
             transform-response
             (select-keys [:status :body]))
         {:status 500
          :body {:errors "dummy exception"}})))

(deftest  get-route-without-any-params
  (is (= (-> (mock/request :get "/juju")
             (mock/header "x-callid" "dummy-callid")
             (mock/header "Accept" "application/json")
             (mock/header "Content-Type" "application/json")
             (mock/header "authorization" "dummy-header")
             handler
             transform-response
             (select-keys [:status :body]))
         {:status 200
          :body {:juju-id "juju"}})))

(deftest  get-route-with-id-long
  (is (= (-> (mock/request :get "/ejendom/10")
             (mock/header "x-callid" "dummy-callid")
             (mock/header "Accept" "application/json")
             (mock/header "Content-Type" "application/json")
             (mock/header "authorization" "dummy-header")
             handler
             transform-response
             (select-keys [:status :body]))
         {:status 500
          :body {:errors "(not (map? [10]))"}})))

(deftest get-route-with-wrong-id-string
  (is (= (-> (mock/request :get "/ejendom/ti")
             (mock/header "x-callid" "dummy-callid")
             (mock/header "Accept" "application/json")
             (mock/header "Content-Type" "application/json")
             (mock/header "authorization" "dummy-header")
             handler
             transform-response
             (select-keys [:status :body]))
         {:status 400
          :body {:errors {:id "(not (instance? java.lang.Long \"ti\"))"}}})))

(deftest get-route-with-no-auth-header
  (is (= (-> (mock/request :get "/ejendom/12")
             (mock/header "x-callid" "dummy-callid")
             (mock/header "Accept" "application/json")
             (mock/header "Content-Type" "application/json")
             handler
             transform-response
             (select-keys [:status :body]))
         {:status 400
          :body {:errors  {:authorization "missing-required-key"}}})))

(deftest put-route-with-correct-request-body
  (is (= (-> (mock/request :put "/ejendom/11")
             (mock/header "x-callid" "dummy-callid")
             (mock/header "Accept" "application/json")
             (mock/header "authorization" "dummy-header")
             (mock/header "Content-Type" "application/json")
             (mock/body (json/encode {:test-id 19000}))
             handler
             transform-response
             (select-keys [:status :body]))
         {:status 201
          :body {:test-id 19000}})))

(deftest put-route-with-wrong-request-body
  (is (= (-> (mock/request :put "/ejendom/11")
             (mock/header "x-callid" "dummy-callid")
             (mock/header "Accept" "application/json")
             (mock/header "authorization" "dummy-header")
             (mock/header "Content-Type" "application/json")
             (mock/body (json/encode {:test-id 19000 :ejendom-id 10 :nested {:a 3}}))
             handler
             transform-response
             (select-keys [:status :body]))
         {:status 400
          :body {:errors {:ejendom-id "disallowed-key", :nested "disallowed-key"}}})))
