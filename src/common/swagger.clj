(ns common.swagger
  (:require [liberator.representation :as rep]
            [liberator.core :refer [resource defresource]]
            [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [cheshire.parse :as parse]
            [io.clojure.liberator-transit]
            [clojure.data.codec.base64 :as b64]
            [clojure.string :as string]
            [compojure.core :as cc :refer [defroutes ANY]]
            [common.http.content-types :as ct]
            [compojure.api.sweet :as sweet]
            [schema.core :as s]
            [schema.utils :as su]
            [plumbing.core :refer [for-map]]
            [clojure.walk :refer :all])
  (:import [java.net URL]
           (schema.utils ValidationError))
  )


(def ^:const available-media-types
  ["application/transit+json"
   "application/transit+msgpack"
   "application/edn"])

(def ^:const valid-content-types (keys ct/parser-map))

(defn- body-as-inputstream [body]
  (when body
    (condp instance? body
      java.lang.String (io/input-stream (.getBytes body))
      body)))

(defn check-content-type
  "set of http methods as keyword #{:post :put ..}
   liberator context
   valid-content-types as a list of rexp"
  [methods ctx content-types]
  (if (methods (get-in ctx [:request :request-method]))
    (or
      (when (get-in ctx [:request :headers "content-type"]) (some #(when (re-find % (get-in ctx [:request :headers "content-type"])) %) valid-content-types))
      [false {:message "Unsupported Content-Type"}])
    true))

(defn parse-body
  [context key]
  (when (#{:put :post :patch} (get-in context [:request :request-method]))
    (try
      (if-let [body (body-as-inputstream (get-in context [:request :body]))]
        (let [content-type (get-in context [:request :headers "content-type"])
              parser (ct/parser-for ct/parser-map content-type)
              data (parser body)]
          ; add audit log if request body must be logged
          [false {key data}])
        {:message "No body"})
      (catch Exception e
        (log/warn e "error parsing body")
        {:message (format "IOException: " (.getMessage e))}))))


(defn- format-error
  "Returns error in the same format as compojure-api coercers"
  [formatter]
  (fn [{:keys [error]}] (formatter {:errors error})))


(defn exception-handler
  "Returns error in the same format as compojure-api coercers"
  [ctx]
  (log/warn (:exception ctx) "")
  {:errors (.getMessage (:exception ctx))})

; ############################################################
; restructure swagger info based on http verb type
; used in the macro api
; ############################################################
(defmulti restructure-swagger
  (fn [method swagger-map liberator-map] method))

(defmethod restructure-swagger :get [_ swagger-map liberator-map]
  (let [swagger (dissoc swagger-map :body :consumes)
        swagger (assoc swagger :produces (:available-media-types liberator-map))
        swagger (update-in swagger [:responses] #(dissoc % :202 :204 :201))
        ]
    swagger))

(defmethod restructure-swagger :post [method swagger-map liberator-map]
  (let [swagger (dissoc swagger-map :return)
        swagger (assoc swagger :produces (:available-media-types liberator-map) :consumes (:available-media-types liberator-map))
        swagger (update-in swagger [:responses] #(dissoc % :200 :202))]
    swagger))

(defmethod restructure-swagger :put [method swagger-map liberator-map]
  (let [swagger (dissoc swagger-map :return)
        swagger (assoc swagger :produces (:available-media-types liberator-map) :consumes (:available-media-types liberator-map))
        swagger (update-in swagger [:responses] #(dissoc % :200 :202))
        ]
    swagger))

(defmethod restructure-swagger :patch [method swagger-map liberator-map]
  (let [swagger (dissoc swagger-map :return)
        swagger (assoc swagger :produces (:available-media-types liberator-map) :consumes (:available-media-types liberator-map))
        swagger (update-in swagger [:responses] #(dissoc % :200 :202))
        ]
    swagger))

(defmethod restructure-swagger :delete [method swagger-map liberator-map]
  (let [swagger (dissoc swagger-map :return :consumes :produces :body)
        swagger (update-in swagger [:responses] #(dissoc % :201))]
    swagger))

(defmethod restructure-swagger :any [method swagger-map liberator-map]
  (let [swagger (dissoc swagger-map :return :consumes :produces :body)
        swagger (update-in swagger [:responses] #(dissoc % :201))]
    swagger))

(defmethod restructure-swagger :options [method swagger-map liberator-map]
  (let [swagger (dissoc swagger-map :return :consumes :produces :body)
        swagger (update-in swagger [:responses] #(dissoc % :201))]
    swagger))

(defmethod restructure-swagger :head [method swagger-map liberator-map]
  (let [swagger (dissoc swagger-map :return :consumes :produces :body)
        swagger (update-in swagger [:responses] #(dissoc % :201))]
    swagger))

(defn- resolve-symbol-value [sym]
  (try
    (if (symbol? sym)
      (var-get (resolve sym))
      sym)
    (catch Throwable t
      (throw (Exception. (str "Compile error, could not resolve: " sym"'s value for usage in rest-api" t))))))

(defmacro rest-api
  "compiles a defroute* with the http-method as specified in allowed-methods
   plus an any route ex. with [:get :put] :
   (defroute* name (GET \"/someroute\" []
   (a-liberator-resource))
   (PUT \"/someroute\" []
   (a-liberator-resource))
   (ANY \"/someroute\" []
   (a-liberator-resource)))

   name is the name created in the namespace
   path and params is the compojure route info
   liberator-base is a list of defaults liberator values or an empty list*
   liberator-resource a list of liberator values or an empty list*
   swagger is a liste of swagger values or an empty list*

   * (can be a symbol)
   "

  [name path params liberator-base liberator-resource swagger]
  (let [lib-body# (concat (resolve-symbol-value liberator-base)
                          (resolve-symbol-value liberator-resource))
        res# (apply hash-map lib-body#)
        swagger-map# (apply hash-map (resolve-symbol-value swagger))
        lib-res# (apply list 'liberator.core/defresource (gensym "api-service-") path params lib-body#)
        routes# (gensym "lib-route-")
        verbs {:get 'compojure.api.sweet/GET* :put 'compojure.api.sweet/PUT* :post 'compojure.api.sweet/POST* :delete 'compojure.api.sweet/DELETE*
               :any 'compojure.api.sweet/ANY* :head 'compojure.api.sweet/HEAD* :patch 'compojure.api.sweet/PATCH* :options 'compojure.api.sweet/OPTIONS*}
        ; add a default any routes as the last route in a resource
        ; because of method not allowed 405 liberator
        methods# (conj (:allowed-methods res#) :any)
        route-fn (fn [method]
                   (conj (apply vector
                                path
                                params
                                (mapcat identity (restructure-swagger method  swagger-map# res#)))
                         lib-res#))
        ]

    `(compojure.api.sweet/defroutes* ~name ~@(map #(apply list (verbs %) (route-fn %)) methods#))))



; ############################################################
; Extend compojure-api to push functionality down into
; the liberatator descisions tree
; ############################################################
(defn workaround-missing-mediatype [ctx]
  ; because of liberator #94
  {:representation {:media-type ; fixme join retuner en empty sring...
                    (or  (clojure.string/join \/ (seq (liberator.conneg/best-allowed-content-type
                                                        (get-in ctx [:request :headers "accept"])
                                                        (conj available-media-types "application/json"))))
                        "application/json")}})

; todo patched version, remember to make upsteam pr
(defn stringify-error [error]
  (postwalk
    (fn [x]
      (cond
        (instance? ValidationError x) (str (su/validation-error-explain x))
        (map? x) (for-map [[k v] x]
                          k (cond
                              (instance? ValidationError v) (str (su/validation-error-explain v))
                              (symbol? v) (str v)
                              :else v))
        :default x))
    error))

(def schema-error-formatter (format-error stringify-error))

(defn- response-body-coercer [status body responses]
  (if (and status responses)
    (if-let [schema (:schema ((or  responses {}) status))]
      (let [coerce-result (ring.swagger.schema/coerce schema body)]
        (if (ring.swagger.schema/error? coerce-result)
          (do (log/warn (str "Response coerce failed. " body))
              [500 (schema-error-formatter coerce-result)])
          [status body]))
      [status body])
    [status body]))

(defn as-response-swagger
  ([body ctx]
   (as-response-swagger body ctx identity))
  ([body ctx f]
   (let [status (:status ctx)
         swagger-responses (get-in ctx [:request ::swagger-coerce :responses])
         [status body] (response-body-coercer status body swagger-responses)
         ctx (assoc ctx :status status)
         ctx (f ctx)]
     (assoc (liberator.representation/as-response body ctx) :status status))))


(defn pre-validation "middleware for getting hold of swaggers responses at compiletime"
  [key value handler]
  (fn [req]
    (let [req (update-in req [::swagger-coerce key] merge value)
          res (handler req)]
      res)))

(defn parse-and-coerce-body [ctx]
  (let [data (parse-body ctx ::data)]
    (if-not (map? data) ; nothing parsed from body
      (let [coerce-result (ring.swagger.schema/coerce (get-in ctx [:request ::swagger-coerce :body] s/Any) (::data (second data)))]
        (if-not (ring.swagger.schema/error? coerce-result)
          data ;no error in schema or not schema to verify against
          [true {::error-data (schema-error-formatter coerce-result)}]))
      data)))

(def strict-schema (comp compojure.api.meta/strict compojure.api.meta/fnk-schema))

; input validation (request)
(defmethod compojure.api.meta/restructure-param :body [_ [value schema] {:keys [body] :as acc}]
  (-> acc
      (assoc-in [:parameters :parameters :body] schema)
      (assoc :body `((pre-validation :body ~schema ~@body)))))


(defmethod compojure.api.meta/restructure-param :x-roles[k v acc]
  (update-in acc [:parameters] assoc k v))


(defmethod compojure.api.meta/restructure-param :body-params [_ body-params acc]
  (let [schema (strict-schema body-params)]
    (-> acc
        (assoc-in [:parameters :parameters :body] schema))))

; output validation (response)
(defmethod compojure.api.meta/restructure-param :responses
  [_ responses {:keys [body] :as acc}]
  (-> acc
      (update-in [:parameters :responses] merge responses)
      (assoc :body `((pre-validation :responses ~responses ~@body)))))

(defmethod compojure.api.meta/restructure-param :return [_ schema {:keys [body] :as acc}]
  (let [response (#'compojure.api.meta/convert-return schema)]
    (-> acc
        (update-in [:parameters :responses] merge response)
        (assoc :body `((pre-validation :responses ~response ~@body))))))

; ############################################################
; Common rest liberator bases.
; ############################################################

(def auth-base-api
  [;:handle-unauthorized unauthorized-handler
   ;:authorized? authorized?
   ;:allowed?  allowed?
   :service-available? workaround-missing-mediatype
   :as-response as-response-swagger
   :available-media-types (conj available-media-types "application/json")
   :handle-exception exception-handler
   :malformed? parse-and-coerce-body
   :handle-malformed (fn [ctx] (::error-data ctx))
   :available-charsets  ["utf-8"]])

(def no-auth-base-api
  [:service-available? workaround-missing-mediatype
   :as-response as-response-swagger
   :available-media-types (conj available-media-types "application/json")
   :handle-exception exception-handler
   :malformed? parse-and-coerce-body
   :handle-malformed (fn [ctx] (::error-data ctx))
   :available-charsets  ["utf-8"]])

(def swagger-info-map {:version "1.0.0"
                       :title "Ejendomsvurdering"
                       :description "Skatteministeriet ejendomsvurdering"
                       :termsOfService "http://helloreverb.com/terms/"
                       :contact {:url "https://iceskm.dk"}
                       :license {:name "Eclipse Public License"
                                 :url "http://www.eclipse.org/legal/epl-v10.html"}})


; ############################################################
; Common rest resources across microservices.
; ############################################################

(s/defschema error-schema {:errors {s/Keyword s/Any}})
(s/defschema service-discovery {:service (s/either String s/Any) :version String})
(rest-api ping "/ping" []
          no-auth-base-api
          [:allowed-methods  [:get]
           :exists? (fn [ctx] {::entity  "OK"})
           :handle-ok ::entity]
          [:return String
           :description "return a string with OK"
           :tags ["system"]])

(rest-api discover "/discover" []
          no-auth-base-api
          [:allowed-methods  [:get]
           :exists? (fn [_]  {::entity {:service "service-name" :version "1.0"}})
           :handle-ok ::entity]
          [:return service-discovery
           :description "return a string with OK"
           :tags ["system"]])

(rest-api echo "/echo" []
          no-auth-base-api
          [:allowed-methods  [:get]
           :exists? (fn [ctx]
                      {::entity (-> (:request ctx)
                                    (dissoc :body
                                            :compojure.api.middleware/options
                                            :ring.swagger.middleware/data
                                            ::swagger-coerce))})
           :handle-ok ::entity]
          [:return {s/Keyword s/Any}
           :description "echo the request map back"
           :tags ["system"]])

(sweet/defroutes* system-application
  (sweet/context* "/system" []
                  ping
                  echo
                  discover))
