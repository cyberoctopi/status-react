(ns status-im.ui.screens.wallet.request.events
  (:require [status-im.utils.handlers :as handlers]
            [status-im.ui.screens.wallet.db :as wallet-db]
            [status-im.chat.events.input :as input-events]
            [status-im.chat.constants :as chat-const]
            [re-frame.core :as re-frame]))

(handlers/register-handler-fx
  ::wallet-send-chat-request
  [re-frame/trim-v]
  (fn [{{:contacts/keys [contacts] :as db} :db} [amount]] 
    (-> db
        (input-events/select-chat-input-command
         (assoc (get-in contacts chat-const/request-command-ref) :prefill [amount]) nil true)
        (assoc :dispatch [:send-current-message]))))

(handlers/register-handler-fx
  :wallet-send-request
  [re-frame/trim-v]
  (fn [{{:wallet/keys [request-transaction]} :db} [{:keys [whisper-identity]}]]
    {:dispatch-n [[:navigate-back]
                  [:navigate-to-clean :chat-list]
                  [:add-chat-loaded-event whisper-identity [::wallet-send-chat-request (:amount request-transaction)]]
                  [:start-chat whisper-identity]]}))

(handlers/register-handler-fx
  :wallet-validate-request-amount
  (fn [{{:wallet/keys [request-transaction] :as db} :db} _]
    (let [amount (:amount request-transaction)
          error (wallet-db/get-amount-validation-error amount)]
      {:db (assoc-in db [:wallet/request-transaction :amount-error] error)})))
