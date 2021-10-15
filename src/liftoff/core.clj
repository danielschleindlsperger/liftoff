(ns liftoff.core
  (:require [clojure.java.io :as io]
            [jsonista.core :as jsonista]
            [reitit.ring :as ring]
            [juxt.jinx.alpha.schema :as jinx.schema]
            [juxt.jinx.alpha.validate :as jinx.validate]))

;; For now we only support JSON string schemas
(defn parse-openapi-schema
  [raw-schema & schema-type]
  ;; TODO: add map with operationId->operationConfig
  {:liftoff/openapi (jsonista/read-value raw-schema),
   :liftoff/handler-config
     {"updatePet" {:validations
                     {:parameters
                        {:path nil,
                         :query nil,
                         :body
                           {"application/json"
                              {"schema" {"$ref" "#/components/schemas/Pet"}},
                            "application/xml"
                              {"schema" {"$ref" "#/components/schemas/Pet"}},
                            "application/x-www-form-urlencoded"
                              {"schema" {"$ref" "#/components/schemas/Pet"}}}},
                      ;; TODO: not that important right now
                      :responses {}}},
      ;; TODO: handle "required" flag for parameters
      "getPetById" {:validations {:parameters {:path {"petId" {"type" "integer",
                                                               "format"
                                                                 "int64"}},
                                               :query nil,
                                               :body nil},
                                  :responses {}}}}})

(defn add-handlers
  [liftoff-schema handlers]
  (assoc liftoff-schema :liftoff/handlers handlers))

(defn wrap-operation-id
  [handler operation-id]
  (fn [req] (handler (assoc req :liftoff/operation-id operation-id))))

(defn wrap-validate-openapi
  [handler liftoff-schema]
  (fn [req]
    (let [operation-id (get-in req [:liftoff/operation-id])
          param-validations (get-in liftoff-schema
                                    [:liftoff/handler-config operation-id
                                     :validations :parameters])]
      (prn operation-id)
      (prn param-validations)
      (when (:path param-validations)
        (let [valid? (jinx.validate/validate
                       (:path param-validations)
                       (get-in req [:path-params])
                       {:base-document (get liftoff-schema :liftoff/openapi)})]
          (prn valid?)))
      ;; validate
      (let [resp (handler req)]
        ;; validate response?
        resp))))

(defn ->ring
  "Generate a ready-to-use Ring compatible handler function from a liftoff schema."
  [liftoff-schema]
  ;; TODO: validate we have all parts we need in the schema
  (let [routes
          (map (fn [[path methods]]
                 (let [path-config
                         (into
                           {}
                           (map
                             (fn [[method method-config]]
                               (let [operation-id (get method-config
                                                       "operationId")
                                     handler (get-in liftoff-schema
                                                     [:liftoff/handlers
                                                      (keyword operation-id)])]
                                 ;; TODO: validate that we have a handler
                                 ;; defined for the given operationId (and also
                                 ;; that the operationId is defined in the
                                 ;; schema)
                                 [(keyword method)
                                  {:operation-id operation-id,
                                   :handler handler,
                                   :middleware [[wrap-operation-id operation-id]
                                                [wrap-validate-openapi
                                                 liftoff-schema]]}]))
                             methods))]
                   [path path-config]))
            (get-in liftoff-schema [:liftoff/openapi "paths"]))]
    ;; Routes with parameters can have conflicts with explicit routes, such as
    ;; /pets/findByTags and /pets/{petId}.
    ;; For a simpler implementation we just ignore the conflicts for now instead
    ;; of building up a nested route tree which might solve the issue.
    (ring/ring-handler (ring/router routes {:conflicts nil}))))

(comment
  (def raw-schema (slurp (io/resource "examples/pet-store.json")))
  (def handlers
    {:getPetById (fn [req] {:status 200, :body "getPetById"}),
     :addPet (fn [req] {:status 201, :body "addPet"}),
     :updatePet (fn [req] {:status 200, :body "updatePet"}),
     :updatePetWithForm (fn [req] {:status 200, :body "updatePetWithForm"}),
     :deletePet (fn [req] {:status 200, :body "deletePet"}),
     :loginUser (fn [req] {:status 200, :body "loginUser"}),
     :logoutUser (fn [req] {:status 200, :body "logoutUser"})})
  (def app
    (-> raw-schema
        (parse-openapi-schema :json)
        (add-handlers handlers)
        (->ring)))
  (def openapi (parse-openapi-schema raw-schema :json))
  (def base-document (jinx.schema/schema (get-in openapi [:liftoff/openapi])))
  (def test-schema (jinx.schema/schema {"$ref" "#/components/schemas/Pet"}))
  (time (jinx.validate/validate test-schema
                                {"name" "bar", "photoUrls" []}
                                {:base-document base-document}))
  (time (app {:request-method :get, :uri "/pet/asdf3"}))
  (time (app {:request-method :delete, :uri "/pet/123"})))
