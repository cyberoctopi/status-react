(ns status-im.chat.events
  (:require [re-frame.core :as re-frame]
            [taoensso.timbre :as log]
            [status-im.utils.handlers :as handlers]
            [status-im.utils.gfycat.core :as gfycat]
            [status-im.chat.models :as model]
            [status-im.chat.models.unviewed-messages :as unviewed-messages-model]
            [status-im.chat.sign-up :as sign-up]
            [status-im.chat.constants :as chat-const]
            [status-im.data-store.handler-data :as handler-data]
            [status-im.data-store.messages :as msg-store]
            [status-im.data-store.contacts :as contacts-store]
            [status-im.data-store.chats :as chats-store]
            [status-im.ui.screens.navigation :as navigation]
            [status-im.protocol.core :as protocol]
            [status-im.constants :as const]
            [status-im.ui.components.list-selection :as list-selection]
            [status-im.chat.events.input :as input-events]
            status-im.chat.events.commands
            status-im.chat.events.animation
            status-im.chat.events.receive-message
            status-im.chat.events.sign-up
            status-im.chat.events.console))

;;;; Coeffects

(re-frame/reg-cofx
  :stored-unviewed-messages
  (fn [cofx _]
    (assoc cofx :stored-unviewed-messages (msg-store/get-unviewed))))

(re-frame/reg-cofx
  :get-stored-message
  (fn [cofx _]
    (assoc cofx :get-stored-message msg-store/get-by-id)))

(re-frame/reg-cofx
  :get-stored-messages
  (fn [cofx _]
    (assoc cofx :get-stored-messages msg-store/get-by-chat-id)))

(re-frame/reg-cofx
  :get-last-stored-message
  (fn [cofx _]
    (assoc cofx :get-last-stored-message msg-store/get-last-message)))

(re-frame/reg-cofx
  :get-message-previews
  (fn [cofx _]
    (assoc cofx :message-previews (msg-store/get-previews))))

(re-frame/reg-cofx
  :all-stored-chats
  (fn [cofx _]
    (assoc cofx :all-stored-chats (chats-store/get-all))))

(re-frame/reg-cofx
  :get-stored-chat
  (fn [cofx _]
    (assoc cofx :get-stored-chat chats-store/get-by-id)))

(re-frame/reg-cofx
  :gfy-generator
  (fn [cofx _]
    (assoc cofx :gfy-generator gfycat/generate-gfy)))

;;;; Effects

(re-frame/reg-fx
  :update-message
  (fn [message]
    (msg-store/update-message message)))

(re-frame/reg-fx
  :save-message
  (fn [{:keys [chat-id] :as message}]
    (msg-store/save chat-id message)))

(re-frame/reg-fx
  :save-chat
  (fn [chat]
    (chats-store/save chat)))

(re-frame/reg-fx
  :save-all-contacts
  (fn [contacts]
    (contacts-store/save-all contacts)))

(re-frame/reg-fx
  :protocol-send-seen
  (fn [params]
    (protocol/send-seen! params)))

(re-frame/reg-fx
  :browse
  (fn [[command link]]
    (list-selection/browse command link)))

;;;; Handlers

(handlers/register-handler-db
  :set-layout-height
  [re-frame/trim-v]
  (fn [db [height]]
    (assoc db :layout-height height)))

(handlers/register-handler-db
  :set-chat-ui-props
  [re-frame/trim-v]
  (fn [db [kvs]]
    (model/set-chat-ui-props db kvs)))

(handlers/register-handler-db
  :toggle-chat-ui-props
  [re-frame/trim-v]
  (fn [db [ui-element]]
    (model/toggle-chat-ui-prop db ui-element)))

(handlers/register-handler-db
  :show-message-details
  [re-frame/trim-v]
  (fn [db [details]]
    (model/set-chat-ui-props db {:show-bottom-info? true
                                 :show-emoji?       false
                                 :bottom-info       details})))

(handlers/register-handler-fx
  :load-more-messages
  [(re-frame/inject-cofx :get-stored-messages)]
  (fn [{{:keys [current-chat-id loading-allowed] :as db} :db
        get-stored-messages :get-stored-messages} _]
    (let [all-loaded? (get-in db [:chats current-chat-id :all-loaded?])]
      (if (and loading-allowed (not all-loaded?))
        (let [messages-path [:chats current-chat-id :messages]
              messages      (get-in db messages-path)
              chat-messages (filter #(= current-chat-id (:chat-id %)) messages)
              new-messages  (get-stored-messages current-chat-id (count chat-messages))
              all-loaded?   (> const/default-number-of-messages (count new-messages))]
          {:db (-> db
                   (assoc :loading-allowed false)
                   (update-in messages-path concat new-messages)
                   (assoc-in [:chats current-chat-id :all-loaded?] all-loaded?))
           ;; we permit loading more messages again after 400ms
           :dispatch-later [{:ms 400 :dispatch [:set :loading-allowed true]}]})
        {:db db}))))

(handlers/register-handler-db
  :set-message-shown
  [re-frame/trim-v]
  (fn [db [{:keys [chat-id message-id]}]]
    (update-in db
               [:chats chat-id :messages]
               (fn [messages]
                 (map (fn [message]
                        (if (= message-id (:message-id message))
                          (assoc message :new? false)
                          message))
                      messages)))))

(defn init-console-chat
  [{:keys [chats] :accounts/keys [current-account-id] :as db}]
  (if (chats const/console-chat-id)
    {:db db}
    (cond-> {:db (-> db
                     (assoc :new-chat sign-up/console-chat
                            :current-chat-id const/console-chat-id)
                     (update :chats assoc const/console-chat-id sign-up/console-chat))
             :dispatch-n [[:add-contacts [sign-up/console-contact]]]
             :save-chat sign-up/console-chat
             :save-all-contacts [sign-up/console-contact]}

      (not current-account-id)
      (update :dispatch-n concat sign-up/intro-events))))

(handlers/register-handler-fx
  :init-console-chat
  (fn [{:keys [db]} _]
    (init-console-chat db)))

(handlers/register-handler-fx
  :initialize-chats
  [(re-frame/inject-cofx :all-stored-chats)
   (re-frame/inject-cofx :stored-unviewed-messages)
   (re-frame/inject-cofx :get-last-stored-message)
   (re-frame/inject-cofx :get-message-previews)]
  (fn [{:keys [db all-stored-chats stored-unviewed-messages get-last-stored-message message-previews]} _]
    (let [{:accounts/keys [account-creation?] :contacts/keys [contacts]} db
          new-db (unviewed-messages-model/load-unviewed-messages db stored-unviewed-messages)
          event  [:load-default-contacts!]]
      (if account-creation?
        {:db new-db
         :dispatch event}
        (let [chats (->> all-stored-chats
                         (map (fn [{:keys [chat-id] :as chat}]
                                [chat-id (assoc chat :last-message (get-last-stored-message chat-id))]))
                         (into {}))
              fx {:db (-> new-db
                          (assoc-in [:message-data :preview] message-previews)
                          (assoc :handler-data (handler-data/get-all))
                          (assoc :chats chats))
                  :dispatch-n [event]}]
          (if (not (get contacts const/console-chat-id))
            (update fx :dispatch-n conj [:add-contacts [sign-up/console-contact]])
            fx))))))

(handlers/register-handler-fx
  :reload-chats
  [(re-frame/inject-cofx :all-stored-chats) (re-frame/inject-cofx :get-last-stored-message)]
  (fn [{:keys [db all-stored-chats get-last-stored-message]} _]
    (let [updated-chats (->> all-stored-chats
                             (map (fn [{:keys [chat-id] :as chat}]
                                    (let [prev-chat    (get (:chats db) chat-id)
                                          updated-chat (assoc chat :last-message (get-last-stored-message chat-id))]
                                      [chat-id (merge prev-chat updated-chat)])))
                             (into {}))]
      (-> (assoc db :chats updated-chats)
          init-console-chat))))

(handlers/register-handler-fx
  :send-seen!
  [re-frame/trim-v]
  (fn [{:keys [db]} [{:keys [message-id chat-id from]}]]
    (let [{:keys [web3 current-public-key chats]
           :contacts/keys [contacts]} db
          {:keys [group-chat public?]} (get chats chat-id)]
      (cond-> {:db (unviewed-messages-model/remove-unviewed-messages db chat-id)
               :update-message {:message-id     message-id
                                :message-status :seen}}
        (and (not (get-in contacts [chat-id] :dapp?))
             (not public?))
        (assoc :protocol-send-seen
               {:web3    web3
                :message (cond-> {:from       current-public-key
                                  :to         from
                                  :message-id message-id}
                           group-chat (assoc :group-id chat-id))})))))

(handlers/register-handler-fx
  :show-mnemonic
  [(re-frame/inject-cofx :get-stored-message) re-frame/trim-v]
  (fn [{:keys [get-stored-message]} [mnemonic signing-phrase]]
    (let [crazy-math-message? (get-stored-message chat-const/crazy-math-message-id)]
      {:dispatch-n (sign-up/passphrase-messages-events mnemonic
                                                       signing-phrase
                                                       crazy-math-message?)})))

(handlers/register-handler-fx
  :account-generation-message
  [(re-frame/inject-cofx :get-stored-message)]
  (fn [{:keys [get-stored-message]} _]
    (when-not (get-stored-message chat-const/passphrase-message-id)
      {:dispatch sign-up/account-generation-event})))

(handlers/register-handler-fx
  :move-to-internal-failure-message
  [(re-frame/inject-cofx :get-stored-message)]
  (fn [{:keys [get-stored-message]} _]
    (when-not (get-stored-message chat-const/move-to-internal-failure-message-id)
      {:dispatch sign-up/move-to-internal-failure-event})))

(handlers/register-handler-fx
  :browse-link-from-message
  (fn [{{:contacts/keys [contacts]} :db} [_ link]]
    {:browse [(get-in contacts chat-const/browse-command-ref) link]}))

(handlers/register-handler-fx
  :init-chat
  [(re-frame/inject-cofx :get-stored-messages)]
  (fn [{:keys [db get-stored-messages]} _]
    (let [current-chat-id (:current-chat-id db)]
      {:db (assoc-in db [:chats current-chat-id :messages] (get-stored-messages current-chat-id))})))

(defn preload-chat-data
  "Takes coeffects map and chat-id, returns effects necessary when navigating to chat"
  [{:keys [db get-stored-messages]} chat-id]
  (let [messages (get-in db [:chats chat-id :messages])
        chat-loaded-event (get-in db [:chats chat-id :chat-loaded-event])
        jail-loaded? (get-in db [:contacts/contacts chat-id :jail-loaded?])]
    (cond-> {:db (-> db
                     (assoc :current-chat-id chat-id)
                     (assoc-in [:chats chat-id :was-opened?] true)
                     (model/set-chat-ui-props {:validation-messages nil})
                     (update-in [:chats chat-id] dissoc :chat-loaded-event))
             :dispatch-n [[:load-requests! chat-id]]}

      (empty? messages)
      (assoc-in [:db :chats chat-id :messages] (get-stored-messages chat-id))

      chat-loaded-event
      (update :dispatch-n conj chat-loaded-event))))

(handlers/register-handler-db
  :add-chat-loaded-event
  [re-frame/trim-v]
  (fn [db [chat-id event]]
    (assoc-in db [:chats chat-id :chat-loaded-event] event)))

;; TODO(janherich): remove this unnecessary event in the future (only model function `add-chat` will stay)
(handlers/register-handler-fx
  :add-chat
  [(re-frame/inject-cofx :gfy-generator) re-frame/trim-v]
  (fn [cofx [chat-id]]
    (model/add-chat cofx chat-id)))

(defn navigate-to-chat
  "Takes coeffects map and chat-id, returns effects necessary for navigation and preloading data"
  ([cofx chat-id]
   (navigate-to-chat cofx chat-id false))
  ([cofx chat-id navigation-replace?]
   (let [nav-fn (if navigation-replace?
                  #(navigation/navigate-to % :chat)
                  #(navigation/replace-view % :chat))]
     (-> (preload-chat-data cofx chat-id)
         (update :db nav-fn)))))

(handlers/register-handler-fx
  :navigate-to-chat
  [(re-frame/inject-cofx :get-stored-messages) re-frame/trim-v]
  (fn [cofx [chat-id {:keys [navigation-replace?]}]]
    (navigate-to-chat cofx chat-id navigation-replace?)))

(handlers/register-handler-fx
  :start-chat
  [(re-frame/inject-cofx :gfy-generator)
   (re-frame/inject-cofx :get-stored-messages)
   re-frame/trim-v]
  (fn [{:keys [db] :as cofx} [contact-id {:keys [navigation-replace?]}]]
    (when (not= (:current-public-key db) contact-id) ; don't allow to open chat with yourself
      (if (get (:chats db) contact-id)
        (navigate-to-chat cofx contact-id navigation-replace?) ; existing chat, just preload and displey
        (let [add-chat-fx (model/add-chat cofx contact-id)] ; new chat, create before preload & display
          (merge add-chat-fx
                 (navigate-to-chat (assoc cofx :db (:db add-chat-fx))
                                   contact-id
                                   navigation-replace?)))))))

;; TODO(janherich): remove this unnecessary event in the future (only model function `update-chat` will stay)
(handlers/register-handler-fx
  :update-chat!
  [(re-frame/inject-cofx :get-stored-chat) re-frame/trim-v]
  (fn [cofx [chat]]
    (model/update-chat cofx chat)))

;; TODO(janherich): remove this unnecessary event in the future (only model function `upsert-chat` will stay)
(handlers/register-handler-fx
  :upsert-chat!
  [(re-frame/inject-cofx :get-stored-chat) re-frame/trim-v]
  (fn [cofx [chat]]
    (model/upsert-chat cofx chat)))

(handlers/register-handler-fx
  :check-and-open-dapp!
  (fn [{{:keys [current-chat-id]
         :contacts/keys [contacts] :as db} :db} _]
    (when-let [dapp-url (get-in contacts [current-chat-id :dapp-url])]
      (-> db
          (input-events/select-chat-input-command
           (assoc (get-in contacts chat-const/browse-command-ref) :prefill [dapp-url]) nil true)
          (assoc :dispatch [:send-current-message])))))
