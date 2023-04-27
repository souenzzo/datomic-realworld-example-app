(ns conduit.main
  (:gen-class)
  (:require [conduit.server :as server]
            [datomic.api :as d]
            [io.pedestal.http :as http]))

(defonce *state
  (atom nil))

(defn -main
  [& _]
  (swap! *state
    (fn [st]
      (some-> st http/stop)
      (let [conn (-> (System/getProperty "conduit.db-uri"
                       "datomic:mem://conduit")
                   (doto d/create-database)
                   d/connect)
            {:keys [db-after]} @(d/transact conn server/tx-schema)]
        (-> {::server/schema-db  db-after
             ::server/jwt-secret (System/getProperty "conduit.jwt-secret"
                                   (str (random-uuid)))
             ::server/conn       conn}
          server/create-service
          (assoc ::http/type :jetty
                 ::http/port 8080
                 ::http/join? false
                 ::http/host "0.0.0.0")
          http/create-server
          http/start)))))

(comment
  (-main))
