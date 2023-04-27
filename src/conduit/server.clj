(ns conduit.server
  (:require [buddy.sign.jwt :as jwt]
            [cheshire.core :as json]
            [clojure.set :as set]
            [clojure.string :as string]
            [datomic.api :as d]
            [io.pedestal.http :as http]
            [io.pedestal.http.body-params :as body-params]
            [io.pedestal.interceptor :as interceptor]
            [io.pedestal.log :as log])
  (:import (java.security MessageDigest)
           (java.text Normalizer Normalizer$Form)
           (java.time Clock Duration Instant)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;
;; db stuff ;;
;;;;;;;;;;;;;;

(defn fetch-profile
  [db viewer eid]
  (merge {:bio nil}
    (d/pull db '[(:conduit.user/bio :as :bio)
                 (:conduit.user/image :as :image :default "data:")
                 (:conduit.user/username :as :username)] eid)
    {:following (if-not viewer
                  false
                  (not (empty? (d/q '[:find ?viewer ?profile
                                      :in $ ?viewer ?profile
                                      :where
                                      [?viewer :conduit.user/follows ?profile]]
                                 db viewer eid))))}))

(defn fetch-article
  [db viewer eid]
  (let [article (d/pull db '[(:conduit.article/body :as :body)
                             (:conduit.article/description :as :description)
                             (:conduit.article/slug :as :slug)
                             (:conduit.article/tag :as :tagList :default [])
                             (:conduit.article/title :as :title)
                             {:conduit.user/_published-article [:db/id]}]
                  eid)
        at (first (d/q '[:find (max ?inst) (min ?inst)
                         :keys :updatedAt :createdAt
                         :in $ ?article
                         :where
                         [?ident :db/ident]
                         [?article ?ident _ ?tx]
                         [?tx :db/txInstant ?inst]]
                    db eid))]
    (-> article
      (dissoc :conduit.user/_published-article)
      (update :tagList sort)
      (merge at
        {:author         (fetch-profile db viewer (-> article :conduit.user/_published-article :db/id))
         :favorited      (if-not viewer
                           false
                           (not (empty? (d/q '[:find ?viewer ?article
                                               :in $ ?viewer ?article
                                               :where
                                               [?viewer :conduit.user/favorite-article ?article]]
                                          db viewer eid))))
         :favoritesCount (or (ffirst (d/q '[:find (count ?user)
                                            :in $ ?article
                                            :where
                                            [?user :conduit.user/favorite-article ?article]]
                                       db eid))
                           0)}))))

(defn fetch-multiple-articles-response
  [db articles {::keys [offset limit viewer]
                :or    {offset 0
                        limit  20}}]
  {:articles      (into []
                    (comp (drop offset)
                      (take limit)
                      (map (partial fetch-article db viewer)))
                    articles)
   :articlesCount (count articles)})

(defn fetch-comment
  [db viewer eid]
  (let [comment (d/pull db '[(:conduit.comment/body :as :body)
                             (:conduit.comment/id :as :id)
                             {:conduit.comment/author [:db/id]}]
                  eid)
        at (first (d/q '[:find (max ?inst) (min ?inst)
                         :keys :updatedAt :createdAt
                         :in $ ?article
                         :where
                         [?ident :db/ident]
                         [?article ?ident _ ?tx]
                         [?tx :db/txInstant ?inst]]
                    db eid))]
    (-> comment
      (dissoc :conduit.comment/author)
      (merge at
        {:author (fetch-profile db viewer (-> comment :conduit.comment/author :db/id))}))))


(defn user-response
  [{::keys [^Duration jwt-duration jwt-secret ^Instant now db]} eid]
  (let [user (d/pull db '[(:conduit.user/bio :as :bio)
                          (:conduit.user/email :as :email)
                          (:conduit.user/image :as :image :default "data:")
                          (:conduit.user/username :as :username)] eid)]
    {:user (merge {:bio nil}
             user
             {:token (jwt/sign (-> user
                                 (select-keys [:email :username])
                                 (assoc :iat (.getEpochSecond now)
                                        :exp (.getEpochSecond (.plus now jwt-duration))))
                       jwt-secret)})}))

;;;;;;;;;;;;;;
;; Handlers ;;
;;;;;;;;;;;;;;

(defn slugify
  [s]
  (-> s
    str
    (Normalizer/normalize Normalizer$Form/NFD)
    string/lower-case
    (string/replace #"[^a-z0-9-]" (fn [s] (if (string/blank? s)
                                            "-"
                                            "")))))

(defn create-article #_#_"/articles" :post
  [{::keys [conn jwt-params]
    :keys  [json-params]}]
  (let [viewer (some-> jwt-params
                 :email
                 (some->> (vector :conduit.user/email)))
        base-slug (-> json-params :article :title slugify)
        article-tempid (d/tempid :db.part/user)
        tx-data [[:db/add viewer :conduit.user/published-article article-tempid]
                 {:db/id                       article-tempid
                  :conduit.article/body        (-> json-params :article :body)
                  :conduit.article/description (-> json-params :article :description)
                  :conduit.article/title       (-> json-params :article :title)
                  :conduit.article/tag         (into (sorted-set)
                                                 (map slugify)
                                                 (-> json-params :article :tagList))}]
        do-tx (fn do-tx [n]
                (try
                  @(d/transact conn (conj tx-data
                                      [:db/add article-tempid
                                       :conduit.article/slug (str base-slug
                                                               (some->> n (str "-")))]))
                  (catch Exception ex
                    (let [{:keys [cognitect.anomalies/category db/error]} (ex-data (ex-cause ex))]
                      (if (and (= category :cognitect.anomalies/conflict)
                            (= error :db.error/unique-conflict))
                        (do-tx (inc (or n 0)))
                        (throw ex))))))
        {:keys [db-after tempids]} (do-tx nil)
        id (d/resolve-tempid db-after tempids article-tempid)]
    {:body   {:article (fetch-article db-after viewer id)}
     :status 201}))

(defn create-article-comment #_#_"/articles/{slug}/comments" :post
  [{::keys [jwt-params conn]
    :keys  [path-params json-params]}]
  (let [tempid-comment (d/tempid :db.part/user)
        author (some->> jwt-params :email
                 (vector :conduit.user/email))
        tx-data (concat [[:db/add [:conduit.article/slug (:slug path-params)]
                          :conduit.article/comments tempid-comment]
                         {:db/id                tempid-comment
                          :conduit.comment/id   (d/squuid)
                          :conduit.comment/body (-> json-params :comment :body)}]
                  (when author
                    [[:db/add tempid-comment :conduit.comment/author author]]))
        {:keys [db-after tempids]} @(d/transact conn tx-data)
        id (d/resolve-tempid db-after tempids tempid-comment)]
    {:body   {:comment (fetch-comment db-after author id)}
     :status 200}))

(defn create-article-favorite #_#_"/articles/{slug}/favorite" :post
  [{::keys [jwt-params conn]
    :keys  [path-params]}]
  (let [tx-data [[:db/add [:conduit.user/email (:email jwt-params)]
                  :conduit.user/favorite-article
                  [:conduit.article/slug (:slug path-params)]]]
        {:keys [db-after]} @(d/transact conn tx-data)]
    {:body   {:article (fetch-article db-after [:conduit.user/email (:email jwt-params)]
                         [:conduit.article/slug (:slug path-params)])}
     :status 200}))

(defn create-user #_#_"/users" :post
  [{::keys [conn]
    :keys  [json-params]
    :as    request}]
  (let [{:keys [user]} json-params
        tx-data [{:conduit.user/email    (:email user)
                  :conduit.user/username (:username user)
                  :conduit.user/password (:password user)}]
        {:keys [db-after]} @(d/transact conn tx-data)]
    {:body   (-> request
               (assoc ::db db-after)
               (user-response [:conduit.user/email (:email user)]))
     :status 201}))

(defn delete-article #_#_"/articles/{slug}" :delete
  [{::keys [conn jwt-params db]
    :keys  [path-params]}]
  (let [article (d/entity db [:conduit.article/slug (:slug path-params)])
        owner-email (-> article
                      :conduit.user/_published-article
                      :conduit.user/email)]
    (cond
      (= owner-email
        (:email jwt-params))
      (do
        @(d/transact conn [[:db/retractEntity (:db/id article)]])
        {:status 200})

      :else
      {:status 401})))

(defn delete-article-comment #_#_"/articles/{slug}/comments/{id}" :delete
  [{::keys [conn db jwt-params]
    :keys  [path-params]}]
  (let [comment (d/entity db [:conduit.comment/id (parse-uuid (:id path-params))])
        owner-email (-> comment
                      :conduit.comment/author
                      :conduit.user/email)]
    (cond
      (= owner-email
        (:email jwt-params))
      (do
        @(d/transact conn [[:db/retractEntity (:db/id comment)]])
        {:status 200})
      :else
      {:status 401})))

(defn delete-article-favorite #_#_"/articles/{slug}/favorite" :delete
  [{::keys [jwt-params conn]
    :keys  [path-params]}]
  (let [tx-data [[:db/retract [:conduit.user/email (:email jwt-params)]
                  :conduit.user/favorite-article
                  [:conduit.article/slug (:slug path-params)]]]
        {:keys [db-after]} @(d/transact conn tx-data)]
    {:body   {:article (fetch-article db-after [:conduit.user/email (:email jwt-params)]
                         [:conduit.article/slug (:slug path-params)])}
     :status 200}))

(defn follow-user-by-username #_#_"/profiles/{username}/follow" :post
  [{::keys [conn jwt-params]
    :keys  [path-params]}]
  (let [tx-data [[:db/add [:conduit.user/email (:email jwt-params)]
                  :conduit.user/follows
                  [:conduit.user/username (:username path-params)]]]
        {:keys [db-after]} @(d/transact conn tx-data)]
    {:body   {:profile (fetch-profile db-after [:conduit.user/email (:email jwt-params)]
                         [:conduit.user/username (:username path-params)])}
     :status 200}))

(defn get-articles-feed #_#_"/articles/feed" :get
  [{::keys [db jwt-params]
    :keys  [query-params]}]
  (let [offset (some-> query-params :offset parse-long)
        limit (some-> query-params :limit parse-long)
        viewer [:conduit.user/email (:email jwt-params)]
        articles (sort (map first (d/q '[:find ?article
                                         :in $ ?viewer
                                         :where
                                         [?viewer :conduit.user/follows ?author]
                                         [?author :conduit.user/published-article ?article]]
                                    db viewer)))]
    {:body   (fetch-multiple-articles-response db articles (merge {::viewer viewer}
                                                             (when (number? offset)
                                                               {::offset offset})
                                                             (when (number? limit)
                                                               {::limit limit})))
     :status 200}))

(defn get-article #_#_"/articles/{slug}" :get
  [{::keys [db jwt-params]
    :keys  [path-params]
    :as    request}]
  (if (contains? #{"feed"} (:slug path-params))
    (get-articles-feed request)
    (let [viewer (some-> jwt-params
                   :email
                   (some->> (vector :conduit.user/email)))]
      {:body   {:article (fetch-article db viewer [:conduit.article/slug (:slug path-params)])}
       :status 200})))

(defn get-article-comments #_#_"/articles/{slug}/comments" :get
  [{::keys [jwt-params db]
    :keys  [path-params]}]
  (let [viewer (some-> jwt-params
                 :email
                 (some->> (vector :conduit.user/email)))]
    {:body   {:comments (map (partial fetch-comment db viewer)
                          (map first (d/q '[:find ?comment
                                            :in $ ?slug
                                            :where
                                            [?article :conduit.article/slug ?slug]
                                            [?article :conduit.article/comments ?comment]]
                                       db (:slug path-params))))}
     :status 200}))

(defn get-articles #_#_"/articles" :get
  [{::keys [jwt-params db]
    :keys  [query-params]
    :as    req}]
  (let [tag (some-> query-params :tag)
        author (some-> query-params :author)
        favorited (some-> query-params :favorited)
        offset (some-> query-params :offset parse-long)
        limit (some-> query-params :limit parse-long)
        viewer (some-> jwt-params
                 :email
                 (some->> (vector :conduit.user/email)))
        articles (-> {:query (concat
                               '[:find ?article
                                 :in $]
                               (when tag '[?tag])
                               (when author '[?author])
                               (when favorited '[?favorited])
                               '[:where
                                 [?article :conduit.article/slug]]
                               (when tag
                                 '[[?article :conduit.article/tag ?tag]])
                               (when author
                                 '[[?user :conduit.user/published-article ?article]
                                   [?user :conduit.user/username ?author]])
                               (when favorited
                                 '[[?user :conduit.user/favorite-article ?article]
                                   [?user :conduit.user/username ?favorited]]))
                      :args  (concat [db]
                               (when tag [(slugify tag)])
                               (when author [author])
                               (when favorited [favorited]))}
                   d/query
                   (->> (map first))
                   sort)]
    {:body   (fetch-multiple-articles-response db articles
               (merge (when viewer
                        {::viewer viewer})
                 (when (number? offset)
                   {::offset offset})
                 (when (number? limit)
                   {::limit limit})))
     :status 200}))

(defn get-current-user #_#_"/user" :get
  [{::keys [jwt-params]
    :as    request}]
  {:body   (user-response request [:conduit.user/email (:email jwt-params)])
   :status 200})

(defn get-profile-by-username #_#_"/profiles/{username}" :get
  [{::keys [jwt-params db]
    :keys  [path-params]}]
  (let [viewer (some-> jwt-params
                 :email
                 (some->> (vector :conduit.user/email)))]
    {:body   {:profile (fetch-profile db viewer [:conduit.user/username (:username path-params)])}
     :status 200}))

(defn get-tags #_#_"/tags" :get
  [{::keys [db]}]
  {:body   {:tags (sort (map first (d/q '[:find ?tag
                                          :where
                                          [_ :conduit.article/tag ?tag]]
                                     db)))}
   :status 200})

(defn login #_#_"/users/login" :post
  [{::keys [db]
    :keys  [json-params]
    :as    request}]
  (let [db-password (some-> json-params :user :email
                      (->> (vector :conduit.user/email)
                        (d/entity db))
                      :conduit.user/password)
        req-password (some-> json-params :user :password)
        match? (when (and (string? db-password)
                       (string? req-password))
                 (MessageDigest/isEqual
                   (.getBytes ^String db-password)
                   (.getBytes ^String req-password)))]
    (cond
      (not match?) {:status 401}
      :else {:body   (user-response request [:conduit.user/email (:email (:user json-params))])
             :status 200})))

(defn unfollow-user-by-username #_#_"/profiles/{username}/follow" :delete
  [{::keys [conn jwt-params]
    :keys  [path-params]}]
  (let [tx-data [[:db/retract [:conduit.user/email (:email jwt-params)]
                  :conduit.user/follows
                  [:conduit.user/username (:username path-params)]]]
        {:keys [db-after]} @(d/transact conn tx-data)]
    {:body   {:profile (fetch-profile db-after [:conduit.user/email (:email jwt-params)]
                         [:conduit.user/username (:username path-params)])}
     :status 200}))

(defn update-article #_#_"/articles/{slug}" :put
  [{::keys [jwt-params conn db]
    :keys  [path-params json-params]}]
  (let [viewer (some-> jwt-params
                 :email
                 (some->> (vector :conduit.user/email)
                   (d/entid db)))
        eid [:conduit.article/slug (:slug path-params)]
        tx-data (concat [;; this cas ensure that these attributes will never change.
                         [:db/cas eid :conduit.article/slug (:slug path-params) (:slug path-params)]]
                  (keep (fn [[k v]]
                          (let [ident (keyword "conduit.article" (name k))]
                            (when (contains? #{:conduit.article/body :conduit.article/description :conduit.article/title}
                                    ident)
                              [:db/add eid ident v])))
                    (dissoc (:article json-params)
                      :tagList))
                  (when (contains? (:article json-params) :tagList)
                    (let [next-tags (into (sorted-set)
                                      (map slugify)
                                      (:tagList (:article json-params)))
                          current-tags (-> (d/pull db [:conduit.article/tag] eid)
                                         :conduit.article/tag
                                         set)
                          tags-to-remove (set (remove next-tags current-tags))
                          tags-to-create (set (remove current-tags next-tags))]
                      (for [tag (concat tags-to-remove tags-to-create)]
                        [(if (contains? tags-to-create tag)
                           :db/add
                           :db/retract)
                         eid :conduit.article/tag tag]))))
        {:keys [db-after]} @(d/transact conn tx-data)]
    {:body   {:article (fetch-article db-after viewer eid)}
     :status 200}))

(defn update-current-user #_#_"/user" :put
  [{::keys [conn jwt-params]
    :keys  [json-params]
    :as    request}]
  (let [{:keys [db-after]} @(d/transact conn [(assoc (set/rename-keys (:user json-params)
                                                       {:bio      :conduit.user/bio
                                                        :email    :conduit.user/email
                                                        :username :conduit.user/username
                                                        :password :conduit.user/password})
                                                :conduit.user/email (:email jwt-params))])]
    {:body   (-> request
               (assoc ::db db-after)
               (user-response [:conduit.user/email (:email jwt-params)]))
     :status 200}))

;; HTTP Stuff

(def json-opts
  {:date-format "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"})

(def json-io
  (assoc (body-params/body-params)
    :name ::json-io
    :leave (fn [{:keys [response] :as ctx}]
             (if (or (-> response :body nil?)
                   (-> response :headers (get "Content-Type")))
               ctx
               (assoc ctx :response
                          (-> response
                            (assoc :body (json/generate-string (:body response) json-opts))
                            (assoc-in [:headers "Content-Type"] "application/json")))))))

(defn with-env-on-request
  [static-env dynamic-env]
  (interceptor/interceptor
    {:name  ::with-env
     :enter (fn [ctx] (update ctx :request (partial merge static-env (dynamic-env ctx))))}))

(def generic-error
  (interceptor/interceptor
    {:error (fn [ctx ex]
              (log/info :exception ex)
              (assoc ctx :response {:headers {"Content-Type" "application/json"}
                                    :body    (-> {:errors {:body (loop [body []
                                                                        ex ex]
                                                                   (if-let [cause (ex-cause ex)]
                                                                     (recur (conj body (ex-message ex))
                                                                       cause)
                                                                     body))}}
                                               (json/generate-string json-opts))
                                    :status  422}))}))

(def jwt-auth
  (interceptor/interceptor
    {:enter (fn [{:keys [request] :as ctx}]
              (if-let [jwt-params (some-> request :headers
                                    (get "authorization")
                                    (string/split #" " 2)
                                    second
                                    (jwt/unsign (::jwt-secret request)
                                      {:now (.getEpochSecond ^Instant (::now request))}))]
                (assoc-in ctx [:request ::jwt-params] jwt-params)
                ctx))}))

(defn create-service
  [{::keys [clock jwt-secret conn]
    :or    {clock (Clock/systemUTC)}
    :as    env}]
  (-> {::http/routes #{["/articles" :get get-articles
                        :route-name ::get-articles]
                       ["/articles" :post create-article
                        :route-name ::create-article]
                       ;; route conflict. gambiarra inside get-article fn
                       #_["/articles/feed" :get [get-articles-feed]
                          :route-name ::get-articles-feed]
                       ["/articles/:slug" :get get-article
                        :route-name ::get-article]
                       ["/articles/:slug" :put update-article
                        :route-name ::update-article]
                       ["/articles/:slug" :delete delete-article
                        :route-name ::delete-article]
                       ["/articles/:slug/comments" :get get-article-comments
                        :route-name ::get-article-comments]
                       ["/articles/:slug/comments" :post create-article-comment
                        :route-name ::create-article-comment]
                       ["/articles/:slug/comments/:id" :delete delete-article-comment
                        :route-name ::delete-article-comment]
                       ["/articles/:slug/favorite" :post create-article-favorite
                        :route-name ::create-article-favorite]
                       ["/articles/:slug/favorite" :delete delete-article-favorite
                        :route-name ::delete-article-favorite]
                       ["/profiles/:username" :get get-profile-by-username
                        :route-name ::get-profile-by-username]
                       ["/profiles/:username/follow" :post follow-user-by-username
                        :route-name ::follow-user-by-username]
                       ["/profiles/:username/follow" :delete unfollow-user-by-username
                        :route-name ::unfollow-user-by-username]
                       ["/tags" :get get-tags
                        :route-name ::get-tags]
                       ["/user" :get get-current-user
                        :route-name ::get-current-user]
                       ["/user" :put update-current-user
                        :route-name ::update-current-user]
                       ["/users" :post create-user
                        :route-name ::create-user]
                       ["/users/login" :post login
                        :route-name ::login]}}
    http/default-interceptors
    (update ::http/interceptors (fn [interceptors]
                                  (into [generic-error
                                         (with-env-on-request env
                                           (fn [_context]
                                             {::now          (Instant/now clock)
                                              ::db           (d/db conn)
                                              ::jwt-duration (Duration/parse "PT20M")
                                              ::jwt-secret   jwt-secret}))
                                         jwt-auth]
                                    cat
                                    [(butlast interceptors)
                                     [json-io]
                                     [(last interceptors)]])))))

(def tx-schema
  [{:db/cardinality :db.cardinality/one
    :db/ident       :conduit.article/body
    :db/valueType   :db.type/string}
   {:db/cardinality :db.cardinality/many
    :db/ident       :conduit.article/comments
    :db/isComponent true
    :db/valueType   :db.type/ref}
   {:db/cardinality :db.cardinality/one
    :db/ident       :conduit.article/description
    :db/valueType   :db.type/string}
   {:db/cardinality :db.cardinality/one
    :db/ident       :conduit.article/slug
    :db/unique      :db.unique/value
    :db/valueType   :db.type/string}
   {:db/cardinality :db.cardinality/many
    :db/ident       :conduit.article/tag
    :db/valueType   :db.type/string}
   {:db/cardinality :db.cardinality/one
    :db/ident       :conduit.article/title
    :db/valueType   :db.type/string}
   {:db/cardinality :db.cardinality/one
    :db/ident       :conduit.comment/author
    :db/valueType   :db.type/ref}
   {:db/cardinality :db.cardinality/one
    :db/ident       :conduit.comment/body
    :db/valueType   :db.type/string}
   {:db/cardinality :db.cardinality/one
    :db/ident       :conduit.comment/id
    :db/unique      :db.unique/identity
    :db/valueType   :db.type/uuid}
   {:db/cardinality :db.cardinality/one
    :db/ident       :conduit.user/bio
    :db/unique      :db.unique/identity
    :db/valueType   :db.type/string}
   {:db/cardinality :db.cardinality/one
    :db/ident       :conduit.user/email
    :db/unique      :db.unique/identity
    :db/valueType   :db.type/string}
   {:db/cardinality :db.cardinality/many
    :db/ident       :conduit.user/favorite-article
    :db/valueType   :db.type/ref}
   {:db/cardinality :db.cardinality/many
    :db/ident       :conduit.user/follows
    :db/valueType   :db.type/ref}
   {:db/cardinality :db.cardinality/one
    :db/ident       :conduit.user/image
    :db/valueType   :db.type/string}
   {:db/cardinality :db.cardinality/one
    :db/ident       :conduit.user/password
    :db/valueType   :db.type/string}
   {:db/cardinality :db.cardinality/many
    :db/ident       :conduit.user/published-article
    :db/isComponent true
    :db/valueType   :db.type/ref}
   {:db/cardinality :db.cardinality/one
    :db/ident       :conduit.user/username
    :db/unique      :db.unique/identity
    :db/valueType   :db.type/string}])
