(ns status-im.chat.models.commands
  (:require [status-im.chat.constants :as chat-consts]
            [status-im.bots.constants :as bots-constants]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

(defn- resolve-references
  [contacts name->ref]
  (reduce-kv (fn [acc name ref]
               (assoc acc name (get-in contacts ref)))
             {}
             name->ref))

(defn commands-responses
  "Returns map of commands/responses eligible for current chat."
  [type access-scope->commands-responses {:keys [address]} {:keys [contacts group-chat]} all-contacts]
  (let [bots-only?         (every? (fn [{:keys [identity]}]
                                     (get-in all-contacts [identity :dapp?]))
                                   contacts)
        basic-access-scope (cond-> #{}
                             group-chat (conj :group-chats)
                             (not group-chat) (conj :personal-chats)
                             address (conj :registered)
                             (not address) (conj :anonymous)
                             (not bots-only?) (conj :not-for-bots))
        global-access-scope (conj basic-access-scope :global)
        member-access-scopes (into #{} (map (comp (partial conj basic-access-scope) :identity))
                                   contacts)]
    (reduce (fn [acc access-scope]
              (merge acc (resolve-references all-contacts
                                             (get-in access-scope->commands-responses [access-scope type]))))
            {}
            (cons global-access-scope member-access-scopes))))

(defn requested-responses
  "Returns map of requested command responses eligible for current chat."
  [access-scope->commands-responses account chat contacts requests]
  (let [requested-responses (map (comp name :type) requests)
        responses-map (commands-responses :response access-scope->commands-responses account chat contacts)]
    (select-keys responses-map requested-responses)))


