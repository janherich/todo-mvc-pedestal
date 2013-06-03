(ns todo-mvc.rendering
  (:require [domina :as dom]
            [domina.events :as dom-events]
            [domina.css :as css]
            [goog.events :as goog-events]
            [goog.History :as history]
            [io.pedestal.app.render.push :as render]
            [io.pedestal.app.render.events :as events-helper]
            [io.pedestal.app.render.push.templates :as templates]
            [io.pedestal.app.protocols :as p]
            [io.pedestal.app.messages :as msg]
            [io.pedestal.app.render.push.handlers.automatic :as auto])
  (:require-macros [todo-mvc.html-templates :as html-templates]))

;; Load templates.

(def templates (html-templates/todo-mvc-templates))

;; Define constants

(def *enter-key* 13)

(def *event-type-navigate* goog.history.EventType.NAVIGATE)

(def *h* (goog.History.))

(defn render-todo-page [renderer [_ path] transmitter]
  (let [parent (render/get-parent-id renderer path)
        html (templates/add-template renderer path (:todo-mvc templates))]
    (dom/append! (dom/by-id parent) (html {}))))

(defn destroy-todo-page [renderer [_ path] transmitter]
  (let [parent (render/get-parent-id renderer path)]
    (dom/destroy-children! (dom/by-id parent))))

(defn decode-hash [transform-name messages transmitter e]
  (when-let [filter-value (get {"/" :all "/active" :remaining "/completed" :completed} (.-token e))]
    (doseq [message (msg/fill transform-name messages {:filter filter-value})]
      (p/put-message transmitter message))))

(defn todo-mvc-transform-enable [renderer [_ path transform-name messages] transmitter]
  (condp = transform-name
  :add-todo
  (let [add-todo-input (dom/by-id "new-todo")]
    (dom-events/listen! add-todo-input 
		                    :keypress 
					              (fn [e]
					                (when (= *enter-key* (:keyCode e))
		                        (dom-events/prevent-default e)
					                  (let [text (.-value add-todo-input)]
		                          (set! (.-value add-todo-input) "")
                              (doseq [message (msg/fill transform-name messages {:text text})]
                                (p/put-message transmitter message)))))))
  :filter-todos (do 
                  (goog-events/listen *h* *event-type-navigate* (partial decode-hash transform-name messages transmitter))
                  (.setEnabled *h* true))))

(defn todo-mvc-transform-disable [renderer [_ path transform-name messages] transmitter]
  (condp = transform-name
  :add-todo (dom-events/unlisten! (dom/by-id "new-todo"))
  :filter-todos (do
                  (goog-events/removeAll *h* *event-type-navigate*)
                  (.setEnabled *h* false))))

(defn create-todos-section-node [renderer [_ path] transmitter]
  (let [id (render/new-id! renderer path)
        html (templates/add-template renderer path (:todos-section templates))]
    (templates/prepend-t renderer [:todo-mvc] {:append-todos-section (html {:id id})})))

(defn update-todos-section-node [renderer [_ path _ new-value] transmitter]
  (let [new-value (if new-value 
                    {:check-state "" :toggle-class "toggle-all toggle-all-checked"} 
                    {:check-state nil :toggle-class "toggle-all"})]
    (templates/update-t renderer path new-value)))

(defn select-toggle-all [id]
  (-> (css/sel (str "#" id)) (css/sel ".toggle-all")))

(defn todos-section-transform-enable [renderer [_ path transform-name messages] transmitter]
  (let [id (render/get-id renderer path)]
	  (condp = transform-name
	  :toggle-todos
    (events-helper/send-on :change (select-toggle-all id) transmitter transform-name messages))))

(defn todos-section-transform-disable [renderer [_ path transform-name messages] transmitter]
  (let [id (render/get-id renderer path)]
	  (condp = transform-name
	  :toggle-todos (dom-events/unlisten! (select-toggle-all id)))))

(defn create-todos-node [renderer [_ path] transmitter]
  (let [id (render/new-id! renderer path)
        html (templates/add-template renderer path (:todos templates))]
    (templates/prepend-t renderer [:todo-mvc :todos-section] {:append-todos (html {:id id})})))

(defn create-todo-node [renderer [_ path _ index] transmitter]
  (let [id (render/new-id! renderer path)
        html (templates/add-template renderer path (:todo templates))]
    (templates/insert-t renderer [:todo-mvc :todos-section :todos] {:append-todo (html {:id id})} index)))

(defn update-todo-node [renderer [_ path _ new-value] transmitter]
  (let [new-value (if (:completed new-value) 
                    (assoc new-value :completed "completed" :check-state "" :toggle-class "toggle toggle-checked")
                    (assoc new-value :completed nil :check-state nil :toggle-class "toggle"))]
    (templates/update-t renderer path new-value)))

(defn select-todo-label [id]
  (-> (css/sel (str "#" id)) (css/sel "label")))

(defn select-todo-update [id]
  (-> (css/sel (str "#" id)) (css/sel ".edit")))

(defn select-todo-complete [id]
  (-> (css/sel (str "#" id)) (css/sel ".toggle")))

(defn select-todo-delete [id]
  (-> (css/sel (str "#" id)) (css/sel ".destroy")))

(defn todo-form-transform-enable [renderer [_ path transform-name messages] transmitter]
  (let [id (render/get-id renderer path)]
    (condp = transform-name
	  :update-todo
	  (let [todo (dom/by-id id)
          label (select-todo-label id)
          update-field (select-todo-update id)]
      (dom-events/listen! label :dblclick (partial
                                            (fn [todo e]
                                              (dom-events/prevent-default e)
                                              (dom/add-class! todo "editing")) todo))
      (dom-events/listen! update-field :keypress (partial
                                                   (fn [todo e]
                                                     (when (= *enter-key* (:keyCode e))
                                                       (dom-events/prevent-default e)
                                                       (dom/remove-class! todo "editing")
                                                       (let [value (.-value (dom-events/target e))]
                                                         (doseq [message (msg/fill transform-name messages {:text value})]
                                                           (p/put-message transmitter message))))) todo)))
	  :toggle-todo
    (events-helper/send-on :change (select-todo-complete id) transmitter transform-name messages)
	  :delete-todo
    (events-helper/send-on-click (select-todo-delete id) transmitter transform-name messages))))

(defn todo-form-transform-disable [renderer [_ path transform-name messages] transmitter]
  (let [id (render/get-id renderer path)]
	  (condp = transform-name
	  :update-todo (let [label (select-todo-label id)
                       update-field (select-todo-update id)]
                   (dom-events/unlisten! label)
                   (dom-events/unlisten! update-field))
   	:toggle-todo (dom-events/unlisten! (select-todo-complete id))
	  :delete-todo (dom-events/unlisten! (select-todo-delete id)))))

(defn create-footer-node [renderer [_ path] transmitter]
  (let [id (render/new-id! renderer path)
        html (templates/add-template renderer path (:footer templates))]
    (templates/prepend-t renderer [:todo-mvc] {:append-footer (html {:id id})})))

(defn create-remaining-node [renderer [_ path] transmitter]
  (let [id (render/new-id! renderer path)
        html (templates/add-template renderer path (:remaining templates))]
    (templates/prepend-t renderer [:todo-mvc :footer] {:append-remaining (html {:id id})})))

(defn update-remaining-node [renderer [_ path _ new-value] transmitter]
  (templates/update-t renderer path {:remaining (str new-value)}))

(defn create-filter-node [renderer [_ path] transmitter]
  (let [id (render/new-id! renderer path)
        html (templates/add-template renderer path (:filter templates))]
    (templates/prepend-t renderer [:todo-mvc :footer] {:append-filter (html {:id id})})))

(defn update-filter-node [renderer [_ path _ new-value] transmitter]
  (let [attr-map {:all (if (= :all new-value) "selected" nil)
                  :remaining (if (= :remaining new-value) "selected" nil)
                  :completed (if (= :completed new-value) "selected" nil)}]
    (templates/update-t renderer path attr-map)))

(defn create-completed-node [renderer [_ path] transmitter]
  (let [id (render/new-id! renderer path)
        html (templates/add-template renderer path (:completed templates))]
    (templates/prepend-t renderer [:todo-mvc :footer] {:append-completed (html {:id id})})))

(defn update-completed-node [renderer [_ path _ new-value] transmitter]
  (templates/update-t renderer path {:completed (str new-value)}))

(defn completed-transform-enable [renderer [_ path transform-name messages] transmitter]
  (let [id (render/get-id renderer path)]
	  (condp = transform-name
	  :delete-completed (events-helper/send-on-click (dom/by-id id) transmitter transform-name messages))))

(defn completed-transform-disable [renderer [_ path transform-name messages] transmitter]
  (let [id (render/get-id renderer path)]
	  (condp = transform-name
	  :delete-completed (dom-events/unlisten! (dom/by-id id)))))

(defn render-config []
  [[:node-create [:todo-mvc] render-todo-page]
   [:node-destroy [:todo-mvc] destroy-todo-page]
   [:transform-enable [:todo-mvc] todo-mvc-transform-enable]
   [:transform-disable [:todo-mvc] todo-mvc-transform-disable]
   [:node-create [:todo-mvc :todos-section] create-todos-section-node]
   [:node-destroy [:todo-mvc :todos-section] auto/default-exit]
   [:value [:todo-mvc :todos-section] update-todos-section-node]
   [:transform-enable [:todo-mvc :todos-section] todos-section-transform-enable]
   [:transform-disable [:todo-mvc :todos-section] todos-section-transform-disable]   
   [:node-create [:todo-mvc :todos-section :todos] create-todos-node]
   [:node-destroy [:todo-mvc :todos-section :todos] auto/default-exit]
   [:node-create [:todo-mvc :todos-section :todos :*] create-todo-node]
   [:node-destroy [:todo-mvc :todos-section :todos :*] auto/default-exit]
   [:value [:todo-mvc :todos-section :todos :*] update-todo-node]
   [:transform-enable [:todo-mvc :todos-section :todos :*] todo-form-transform-enable]
   [:transform-disable [:todo-mvc :todos-section :todos :*] todo-form-transform-disable]
   [:node-create [:todo-mvc :footer] create-footer-node]
   [:node-destroy [:todo-mvc :footer] auto/default-exit]
   [:node-create [:todo-mvc :footer :remaining] create-remaining-node]
   [:node-destroy [:todo-mvc :footer :remaining] auto/default-exit]
   [:value [:todo-mvc :footer :remaining] update-remaining-node]
   [:node-create [:todo-mvc :footer :filter] create-filter-node]
   [:node-destroy [:todo-mvc :footer :filter] auto/default-exit]
   [:value [:todo-mvc :footer :filter] update-filter-node]   
   [:node-create [:todo-mvc :footer :completed] create-completed-node]
   [:node-destroy [:todo-mvc :footer :completed] auto/default-exit]
   [:value [:todo-mvc :footer :completed] update-completed-node]
   [:transform-enable [:todo-mvc :footer :completed] completed-transform-enable]
   [:transform-disable [:todo-mvc :footer :completed] completed-transform-disable]])
