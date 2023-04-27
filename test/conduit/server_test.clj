(ns conduit.server-test
  (:require [cheshire.core :as json]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            [conduit.server :as server]
            [datomic.api :as d]
            [io.pedestal.http :as http]
            [io.pedestal.test :refer [response-for]])
  (:import (java.time Clock)))

(defn create-service
  [{::keys [static-clock?]}]
  (let [conn (-> "datomic:mem://conduit"
               (doto d/delete-database
                     d/create-database)
               d/connect)
        {:keys [db-after]} @(d/transact conn server/tx-schema)]
    (-> {::server/conn       conn
         ::server/jwt-secret "00000000-0000-0000-0000-000000000000"
         ::server/schema-db  db-after}
      (merge (when static-clock?
               {::server/clock (proxy [Clock] []
                                 (instant [] (.toInstant #inst"1970")))}))
      server/create-service
      http/create-servlet)))

(defn fetch-json
  [{::http/keys [service-fn]
    ::keys      [std-headers]} request-method uri & {:keys [headers body]}]
  (let [headers (merge (when body
                         {"Content-Type" "application/json"})
                  std-headers
                  headers)
        response (apply response-for service-fn request-method uri
                   (concat (when body
                             [:body (json/generate-string body)])
                     (when headers
                       [:headers headers])))
        json-response? (some-> response :headers (get "Content-Type")
                         (string/starts-with? "application/json"))]
    (merge response
      (when json-response?
        {:body (some-> response :body (json/parse-string true))}))))

(defn authenticate
  [service-map username]
  (let [response
        (fetch-json service-map :post "/users"
          :body {:user {:email    (str username "@localhost")
                        :username username
                        :password username}})
        token (-> response :body :user :token)]
    (update service-map
      ::std-headers merge {"Authorization" (str "Token " token)})))

(def static-clock-jwt-token
  "eyJhbGciOiJIUzI1NiJ9.eyJlbWFpbCI6ImNvbmR1aXRAbG9jYWxob3N0IiwidXNlcm5hbWUiOiJjb25kdWl0IiwiaWF0IjowLCJleHAiOjEyMDB9.PbdklV45QUqk6ZkECg59rmaSp0CYgGMT-zn4EMG0h_Y")

(comment
  (-> {:email    "conduit@localhost"
       :username "conduit"
       :iat      0
       :exp      1200}
    (buddy.sign.jwt/sign (str (java.util.UUID. 0 0)))))

(deftest endpoints-authentication-flow
  (let [service-map (create-service {::static-clock? true})]
    (is (= [201 {:bio      nil
                 :email    "conduit@localhost"
                 :image    "data:"
                 :token    static-clock-jwt-token
                 :username "conduit"}]
          (-> (fetch-json service-map :post "/users"
                :body {:user {:email    "conduit@localhost"
                              :username "conduit"
                              :password "123"}})
            ((juxt :status (comp :user :body)))
            #_(doto clojure.pprint/pprint))))
    (is (= [200 {:bio      nil
                 :email    "conduit@localhost"
                 :image    "data:"
                 :token    static-clock-jwt-token
                 :username "conduit"}]
          (-> (fetch-json service-map :post "/users/login"
                :body {:user {:email    "conduit@localhost"
                              :password "123"}})
            ((juxt :status (comp :user :body)))
            #_(doto clojure.pprint/pprint))))
    (is (= [200 {:bio      nil
                 :email    "conduit@localhost"
                 :image    "data:"
                 :token    static-clock-jwt-token
                 :username "conduit"}]
          (-> (fetch-json service-map :get "/user"
                :headers {"Authorization" (str "Token " static-clock-jwt-token)})
            ((juxt :status (comp :user :body)))
            #_(doto clojure.pprint/pprint))))
    (is (= [200 {:bio      "Hello World"
                 :email    "conduit@localhost"
                 :image    "data:"
                 :token    static-clock-jwt-token
                 :username "conduit"}]
          (-> (fetch-json service-map :put "/user"
                :headers {"Authorization" (str "Token " static-clock-jwt-token)}
                :body {:user {:bio "Hello World"}})
            ((juxt :status (comp :user :body)))
            #_(doto clojure.pprint/pprint))))
    (is (= [200 {:articles      []
                 :articlesCount 0}]
          (-> (fetch-json service-map :get "/articles"
                :headers {"Authorization" (str "Token " static-clock-jwt-token)})
            ((juxt :status :body))
            #_(doto clojure.pprint/pprint))))))

(deftest incremental-slug-generation
  (let [service (create-service {})
        conduit-browser (authenticate service "conduit")]
    (is (= "hello-world"
          (-> conduit-browser
            (fetch-json :post "/articles"
              :body {:article {:body        "hello"
                               :description "world"
                               :title       "hello world"
                               :tagList     ["a" "b" "c"]}})
            :body :article :slug)))
    (is (= "hello-world-1"
          (-> conduit-browser
            (fetch-json :post "/articles"
              :body {:article {:body        "hello"
                               :description "world"
                               :title       "Hello world"
                               :tagList     ["a" "b" "c"]}})
            :body :article :slug)))))


(deftest endpoints-basic-flow
  (let [service (create-service {})
        conduit-browser (authenticate service "conduit")
        response (fetch-json conduit-browser :post "/articles"
                   :body {:article {:body        "hello"
                                    :description "world"
                                    :title       "hello world"
                                    :tagList     ["a" "b" "c"]}})]
    (is (= 201 (:status response)))
    (is (= {:bio       nil
            :following false
            :image     "data:"
            :username  "conduit"}
          (-> response :body :article :author
            #_(doto clojure.pprint/pprint))))
    (is (= {:body           "hello"
            :description    "world"
            :favorited      false
            :slug           "hello-world"
            :favoritesCount 0
            :tagList        ["a" "b" "c"]
            :title          "hello world"}
          (-> response :body :article
            (dissoc :author :createdAt :updatedAt)
            #_(doto clojure.pprint/pprint))))
    (is (= 1
          (-> conduit-browser
            (fetch-json :get "/articles")
            :body
            :articlesCount)))
    (is (= {:body           "hello"
            :description    "world"
            :slug           "hello-world"
            :favorited      false
            :favoritesCount 0
            :tagList        ["a" "b" "c"]
            :title          "hello world"}
          (-> conduit-browser
            (fetch-json :get "/articles")
            :body
            :articles
            first
            (dissoc :author :createdAt :updatedAt))))))

(deftest following-flow
  (let [service (create-service {})
        conduit-browser (authenticate service "conduit")
        _alice-browser (authenticate service "alice")]
    (is (= {:bio       nil
            :image     "data:"
            :username  "alice"
            :following true}
          (-> conduit-browser
            (fetch-json :post "/profiles/alice/follow")
            :body
            :profile
            #_(doto clojure.pprint/pprint))))))


(deftest tags-update-to-many
  (let [service (create-service {})
        conduit-browser (authenticate service "conduit")
        {:keys [tagList slug]} (-> conduit-browser
                                 (fetch-json :post "/articles"
                                   :body {:article {:body        "a"
                                                    :title       "b"
                                                    :description "c"
                                                    :tagList     ["d" "e"]}})
                                 :body
                                 :article)]
    (is (= tagList ["d" "e"]))
    (is (= ["e" "f"]
          (-> conduit-browser
            (fetch-json :put (str "/articles/" slug)
              :body {:article {:tagList ["e" "f"]}})
            :body
            :article
            :tagList
            #_(doto clojure.pprint/pprint))))
    (is (= ["e" "f"]
          (-> conduit-browser
            (fetch-json :get "/tags")
            :body
            :tags
            #_(doto clojure.pprint/pprint))))))


(deftest article-update-body
  (let [service (create-service {})
        conduit-browser (authenticate service "conduit")
        {:keys [body slug]} (-> conduit-browser
                              (fetch-json :post "/articles"
                                :body {:article {:body        "a"
                                                 :title       "b"
                                                 :description "c"
                                                 :tagList     ["d" "e"]}})
                              :body
                              :article)]
    (is (= body "a"))
    (is (= "Hello"
          (-> conduit-browser
            (fetch-json :put (str "/articles/" slug)
              :body {:article {:body "Hello"}})
            :body
            :article
            :body
            #_(doto clojure.pprint/pprint))))))

(deftest create-a-comment
  (let [service (create-service {})
        conduit-browser (authenticate service "conduit")
        {:keys [slug]} (-> conduit-browser
                         (fetch-json :post "/articles"
                           :body {:article {:body        "a"
                                            :title       "b"
                                            :description "c"
                                            :tagList     ["d" "e"]}})
                         :body
                         :article)]
    (is (= "Hello"
          (-> conduit-browser
            (fetch-json :post (str "/articles/" slug "/comments")
              :body {:comment {:body "Hello"}})
            :body
            :comment
            :body
            #_(doto clojure.pprint/pprint))))
    (is (= 1
          (-> conduit-browser
            (fetch-json :get (str "/articles/" slug "/comments")
              :body {:comment {:body "Hello"}})
            :body
            :comments
            count
            #_(doto clojure.pprint/pprint))))))


(deftest article-pagination
  (let [service (create-service {})
        conduit-browser (authenticate service "conduit")
        size 1337
        article-generator (gen/hash-map
                            :body (s/gen string?)
                            :title (s/gen string?)
                            :description (s/gen string?)
                            :tagList (s/gen (s/coll-of string?)))]
    (dotimes [idx size]
      (-> conduit-browser
        (fetch-json :post "/articles"
          :body {:article (-> article-generator
                            gen/generate
                            (update :description str " " idx))})
        :status
        (== 201)
        (assert "fail on creating article")))
    (is (= size
          (-> conduit-browser
            (fetch-json :get "/articles")
            :body
            :articlesCount
            #_(doto clojure.pprint/pprint))))
    (is (= 20
          (-> conduit-browser
            (fetch-json :get "/articles")
            :body
            :articles
            count
            #_(doto clojure.pprint/pprint)))
      "default limit size")
    (is (= (range 20)
          (-> conduit-browser
            (fetch-json :get "/articles")
            :body
            :articles
            (->> (map :description)
              (map #(parse-long (last (string/split % #" ")))))
            #_(doto clojure.pprint/pprint)))
      "the first 20")
    (is (= (range 1317 1337)
          (-> conduit-browser
            (fetch-json :get "/articles?offset=1317")
            :body
            :articles
            (->> (map :description)
              (map #(parse-long (last (string/split % #" ")))))
            #_(doto clojure.pprint/pprint)))
      "the last 20")))

(deftest delete-an-article
  (let [service (create-service {})
        conduit-browser (authenticate service "conduit")
        alice-browser (authenticate service "alice")
        {:keys [slug]} (-> conduit-browser
                         (fetch-json :post "/articles"
                           :body {:article {:body        "a"
                                            :title       "b"
                                            :description "c"
                                            :tagList     ["d" "e"]}})
                         :body
                         :article)]
    (is (= 401
          (-> alice-browser
            (fetch-json :delete (str "/articles/" slug))
            :status
            #_(doto clojure.pprint/pprint))))
    (is (= 200
          (-> conduit-browser
            (fetch-json :delete (str "/articles/" slug))
            :status
            #_(doto clojure.pprint/pprint))))))

(comment
  (require '[clojure.java.process :as p])
  @(p/start {:out :inherit, :err :stdout}
     "npm" "run" "newman" "--"
     "run"
     "--global-var" "APIURL=http://localhost:8080"
     "--global-var" "EMAIL=conduit@localhost"
     "--global-var" "PASSWORD=123"
     "--global-var" "USERNAME=conduit"
     "https://raw.githubusercontent.com/gothinkster/realworld/main/api/Conduit.postman_collection.json"))
