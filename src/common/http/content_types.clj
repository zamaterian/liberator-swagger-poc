(ns common.http.content-types
  "A common library for emitting and parsing to and from the correct content types."
  (:require [clojure.edn :as edn]
            [cheshire.core :as json]
            [cognitect.transit :as transit])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream InputStream]))

;; Parsers (implemented as protocols to provide easy type dispatch)

(defprotocol JSON (parse-json [this]))
(extend-protocol JSON
  String
  (parse-json [str]
    (json/parse-string str true))
  java.io.InputStream
  (parse-json [is]
    (json/parse-stream (clojure.java.io/reader is) true)))

(defprotocol Transit (parse-transit [this type])) ;protocols dispatch on the first argument
(extend-protocol Transit
  String
  (parse-transit [str type]
    (parse-transit (ByteArrayInputStream. (.getBytes "{\"asdf\": 1}")) type))
  java.io.InputStream
  (parse-transit [is type]
    (transit/read (transit/reader is type))))

(defprotocol EDN (parse-edn [this]))
(extend-protocol EDN
  String
  (parse-edn [str]
    (edn/read-string str))
  InputStream
  (parse-edn [is]
   (edn/read {:eof nil} (java.io.PushbackReader. (java.io.InputStreamReader. is)))))

(def parser-map
  "Return a map of MIME-type to parsers. Included types are edn, json and
  form-encoding. parser-options are key-val pairs"
  {#"^application/edn"                      parse-edn
   #"^application/vnd.skm\+edn"             parse-edn
   #"^application/json"                     parse-json
   #"^application/transit\+json"            #(parse-transit % :json)
   #"^application/transit\+msgpack"         #(parse-transit % :msgpack)
   #"^application/vnd.skm\+transit-json"    #(parse-transit % :json)
   #"^application/vnd.skm\+transit-msgpack" #(parse-transit % :msgpack)})

(defn parser-for
  "Find a parser for the given content-type, never returns nil"
  [parser-map content-type]
  (or (when content-type
        (parser-map (some #(when (re-find % content-type) %) (keys parser-map))))
      identity))

;; Emitters

(defn emit-json [data]
  (json/write data))

(defn emit-transit
  "Allowed types: :json :json-verbose :msgpack"
  [data type]
  (let [buffer (ByteArrayOutputStream.)]
    (transit/write (transit/writer buffer type) data)
    buffer))

(defn emit-edn [data]
  (pr-str data))

(def emitter-map {:json ["application/json" emit-json]
                  :transit-json ["application/transit+json" #(emit-transit % :json)]
                  :transit-json-verbose ["application/transit+json;verbose" #(emit-transit % :json)]
                  :transit-msgpack ["application/transit+msgpack" #(emit-transit % :msgpack)]
                  :edn ["application/edn" emit-edn]})


