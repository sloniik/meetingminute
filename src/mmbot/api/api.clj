(ns mmbot.api.api
    (:require [clj-http.client :as http]
      [cheshire.core :refer :all :as json]
      [mmbot.util.core :as util]
      [clojure.tools.logging :as log]
      [clj-time.core :as t]
      [clj-time.coerce :as c]
      [mmbot.db.core :as db]))

;; hard code Token code
(def ^:private TOKEN "209052244:AAGPzU2N3E6MFnqEiU30ByLruXefaGaoB-A")
(def ^:private URL "https://api.telegram.org/bot")

;; number of the last message id
(def last-message (atom 0))

;; найти последнее обработанное сообщение
(reset! last-message (db/get-last-message-number))

;; LOCAL FUNCTIONS
;;======================================================================
(defn- update-params
       "return sting with /getUpdates parametrs"
       [o l t]
       (str
         (if o (str "offset=" o) "")
         (if l
           (str (if o "&" "") (str "limit=" l))
           "")
         (if t
           (str (if l "&" "") (str "timeout=" t))
           "")))
;;TEST
(update-params 1 nil 1)

(defn- query-for-2-params
       "creates query for two parametrs.
       each param is a map"
       [param1 param2]
       (let [p1-text  (:name param1)
             p2-text  (:name param2)
             p1-val   (:value param1)
             p2-val   (:value param2)
             q-p1     (util/par-query-string p1-val p1-text)
             q-p2     (util/par-query-string p2-val p2-text)
             res-text (str q-p1
                           (if (= q-p1 util/CODE-EMPTY)
                             util/CODE-EMPTY
                             (if (= q-p2 util/CODE-EMPTY) util/CODE-EMPTY "&"))
                           q-p2)]
            {:name res-text
             :value (if(= res-text util/CODE-EMPTY) util/CODE-EMPTY util/CODE-UNION)}))

;; TEST
(query-for-2-params
  (query-for-2-params
    (query-for-2-params
      {:value "a16sdfl" :name "chat_id"}
      {:value "124" :name "text"})
    {:value "" :name "login"})
  {:value "bla-bla" :name "mega-text"})

(defn- get-mess-type
      "return message type {text, doc, photo..."
      [m]
      (log/info "MESSAGE:" m)
      (if (:text m)
        (do
          (log/info "M-TYPE is TEXT")
          :text))
      (if (:photo m)
        (do
          (log/info "M-TYPE is PHOTO")
          :photo))
      (if (:document m)
        (do
          (log/info "M-TYPE is DOCUMENT")
          :document)))
;;TEST
(def msg-t (get-mess-type
             {:message_id 28,
              :from {:id 104594715, :first_name "Sergey", :last_name "Artyuhov", :username "sloniik"},
              :chat {:id 104594715, :first_name "Sergey", :last_name "Artyuhov", :username "sloniik", :type "private"},
              :date 1461412944,
              :photo [{:file_id "AgADAgAD3KcxGxv9OwZqXN50fWHGAdPJRw0ABFRT16rH-8ufPh0AAgI",
                       :file_size 939,
                       :width 84,
                       :height 90}]}))
;;======================================================================


(defn -get-me
      "return current bot"
      [bot url token]
      (let [res (http/get (str url token "/getMe"))
            responce (util/parse-responce res)]
           (log/info "getMe result:" res)
           (if (:ok responce)
             (do
               (log/info "getMe returns OK")
               (:result responce))
             (log/error "Telegram API didn't answer to getMe"))))

(defn -updates
      "get all updates in the bot"
      [bot url token]
      (let [res (http/get (str url token "/getUpdates"))
            responce (util/parse-responce res)]
           (log/info "getUpdates results:" res)
           (if (:ok responce)
             (do
               (log/info "getUpdates returns OK")
               (:result responce))
             (log/error "Telegram API didn't answer to getUpdate"))))

(defn -updates-with-params
      "get all updates in the bot"
      [bot url token params]
      (let [offset    (:offset params)
            limit     (:limit params)
            timeout   (:timeout params)
            par-str (update-params offset limit timeout)
            res (http/get (str url token "/getUpdates?" par-str))
            responce (util/parse-responce res)]
           (log/info "OFFS: " offset)
           (log/info "URL:" (str url token "/getUpdates?" par-str))
           (log/info "getUpdates results:" res)
           (if (:ok responce)
             (do
               (log/info "getUpdates returns OK")
               (:result responce))
             (log/error "Telegram API didn't answer to getUpdate"))))

(defn -msg-send
      "send message (:text) to chat (:chat-id)"
      [bot url token params]
      (let [chat-id    (:chat-id params)
            msg-text     (:text params)
            par-str (:name
                      (query-for-2-params
                      {:value chat-id :name "chat_id"}
                      {:value msg-text :name "text"}))]
           (println "PARAMS: " params)
           (println "URL:" (str url token "/sendMessage?" par-str))
           (let
             [res (http/post (str url token "/sendMessage?" par-str))
              responce (util/parse-responce res)]
             (log/info "sendMessage results:" res)
           (if (:ok responce)
             (do
               (log/info "getUpdates returns OK")
               (:result responce))
             (log/error "Telegram API didn't answer to getUpdate")))))

(-msg-send mm-bot URL TOKEN {:chat-id 104594715 :text "'Hello there, user'"})


;;======================================================================
(defprotocol TBotAPI
  (get-me [this] "Функция возвращает описание текущего бота")
  (updates [this params] "Получить json со всеми сообщениями ")
  (msg-send [this data] "Послать данные"))

(defrecord Bot [url token]
           TBotAPI
           (get-me
             [bot]
             (-get-me bot url token))
           (updates
             [bot params]
             (if params
               (-updates-with-params bot url token params)
               (-updates bot url token)))
           (msg-send
             [bot params]
             (-msg-send bot url token params)))


;;======================================================================
;;=== Execute ===
;;======================================================================

(def mm-bot (Bot. URL TOKEN))
(def me (get-me mm-bot))

(def upd-param {:offset @last-message
                :limit 20
                :timeout 1000})

(swap! last-message inc)
;(def upd-param nil)

;; test
(def upd (updates mm-bot upd-param))

(def message (-> upd first :message))
;;расчет даты
(c/from-long
  (-> upd last :message :date (*  1000)))


(defn answer-messages
      "answer to all the messages"
      [all-upd]
      (loop [m (util/get-message all-upd)
             r (rest all-upd)]
            (if (nil? m)
              (log/info "ALL-MESSAGE ARE ANSWERED!")
              (do
                (log/info "REST: " r)
                (get-mess-type m)
                (let [char-id (-> m :chat :id)
                      msg-text (:text m)])
                (recur (util/get-message r)
                       (rest r))))))

(answer-messages upd)
