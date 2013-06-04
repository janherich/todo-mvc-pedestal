(ns ^:shared todo-mvc.behavior
    (:require [clojure.set :as set]
              [clojure.string :as string]
              [io.pedestal.app :as app]
              [io.pedestal.app.messages :as msg]
              [io.pedestal.app.util.platform :as platform]
              [todo-mvc.util :as util]))

;; Transforms

(defn delete-update-count [state completed]
  (if completed
    (update-in state [:completed] dec)
    (update-in state [:remaining] dec)))

(defn show-todo? [filter-state todo-completed]
  (if todo-completed
    (if (= :remaining filter-state) false true)
    (if (= :completed filter-state) false true)))

(defn complete-todos [todos completed filter-state]
  (util/update-values todos (fn [todo]
                              (-> todo
                                (assoc :completed completed)
                                (assoc :show (show-todo? filter-state completed))))))

(defn filter-todos [todos filter-state]
  (util/update-values todos (fn [{completed :completed :as todo}] 
                              (assoc todo :show (show-todo? filter-state completed)))))

(defn delete-completed [todos]
  (into {} (filter (fn [[k v]] (not (:completed v))) todos)))

(def allowed-filter-values #{:all :remaining :completed})

(defn delete-todo [state id completed]
  (-> state
    (update-in [:todos] dissoc id)
    (delete-update-count completed)))

(defn todos-transform [state message]
  (condp = (msg/type message)
    msg/init (:value message)
    :add-todo (let [text (string/trim (:text message))]
                (if (string/blank? text)
                  state
                  (let [id (util/random-id)
		                    todo {:text text
                              :added (platform/date)
		                          :completed false
		                          :show (show-todo? (:filter state) false)}]
		                (-> state
		                  (assoc-in [:todos id] todo)
		                  (update-in [:remaining] inc)))))
    :update-todo (let [id (:id message)
                       todo (get-in state [:todos id])
                       text (string/trim (:text message))] 
                   (if todo
                     (if (string/blank? text)
                       (delete-todo state id (:completed todo))
                       (assoc-in state [:todos id] (assoc todo :text text)))
                     state))
    :toggle-todo (let [id (:id message)
                       todo (get-in state [:todos id])]
                   (if todo
                     (let [completed (not (:completed todo))
                           todo (assoc todo :completed completed)
                           todo (assoc todo :show (show-todo? (:filter state) completed))]
                       (-> state
                         (assoc-in [:todos id] todo)
                         (update-in [:remaining] #(if completed (dec %) (inc %)))
                         (update-in [:completed] #(if completed (inc %) (dec %)))))
                     state))
    :toggle-todos (let [complete (> (:remaining state) 0)
                        filter (:filter state)
                        todos-count (count (:todos state))]
                    (if todos-count
                      (-> state 
	                      (update-in [:todos] complete-todos complete (:filter state))
	                      (assoc :remaining (if complete 0 todos-count))
	                      (assoc :completed (if complete todos-count 0)))
                      state))
    :delete-todo (let [id (:id message)
                       todo (get-in state [:todos id])]
                   (if todo
                     (delete-todo state id (:completed todo))
                     state))
    :delete-completed (-> state
                        (update-in [:todos] delete-completed)
                        (assoc :completed 0))
    :filter-todos (let [filter (allowed-filter-values (:filter message))] 
                    (if (and filter (not= filter (:filter state)))
                      (-> state
                        (update-in [:todos] filter-todos filter)
                        (assoc :filter filter))
                      state))
    state))

;; Effects

(defn send-todos-to-service [message old-model new-model]
  [message])

;; Combines

(defn assoc-key-dissoc-show [coll]
  (mapv (fn [[k v]] (-> (assoc v :id k)
                      (dissoc :show))) coll))

(defn add-sort-index [{todos :todos :as model}]
  (let [displayed-todos (assoc-key-dissoc-show (filter (fn [[k v]] (:show v)) todos))
        displayed-todos (zipmap (iterate inc 0) (sort-by (juxt :completed :added) displayed-todos))
        displayed-todos (into {} (map (fn [[k v]] [(:id v) k]) displayed-todos))
        todos (into {} (map (fn [[k v]] [k (if-let [s-idx (get displayed-todos k)] (assoc v :sort-index s-idx) v)]) todos))]
    (assoc model :todos todos)))

(defn todos-rendered-diff [{todos-first :todos} {todos-second :todos}]
  (assoc-key-dissoc-show (filter (fn [[k v]] (and (:show v) (not (:show (get todos-first k))))) todos-second)))

(defn new-todos-rendered [state input-name old-value new-value]
  (todos-rendered-diff old-value (add-sort-index new-value)))

(defn deleted-todos-rendered [state input-name old-value new-value]
  (todos-rendered-diff new-value old-value))

(defn updated-todos-rendered [state input-name {old-todos :todos} {new-todos :todos}]
  (assoc-key-dissoc-show (filter (fn [[k new-todo]]
							             (let [old-todo (get old-todos k)]
							               (and (:show old-todo) (:show new-todo) (not= old-todo new-todo)))) new-todos)))

(defn first-todos-added? [old-completed new-completed old-remaining new-remaining]
  (and (<= (+ old-completed old-remaining) 0) (> (+ new-completed new-remaining) 0)))

(defn last-todos-deleted? [old-completed new-completed old-remaining new-remaining]
  (and (> (+ old-completed old-remaining) 0) (<= (+ new-completed new-remaining) 0)))

(defn remaining-todos-changed? [old-completed new-completed old-remaining new-remaining]
  (and (> (+ old-completed old-remaining) 0) (> (+ new-completed new-remaining) 0) (not= old-remaining new-remaining)))

(defn remaining-todos [state input-name {old-completed :completed old-remaining :remaining} 
                                        {new-completed :completed new-remaining :remaining}]
  (when (and old-completed old-remaining new-completed new-remaining)
	  (cond 
	    (first-todos-added? old-completed new-completed old-remaining new-remaining) 
     {:node :create :remaining new-remaining :check-all-state (= 0 new-remaining)}
	    (remaining-todos-changed? old-completed new-completed old-remaining new-remaining) 
     {:node :update :remaining new-remaining :check-all-state (= 0 new-remaining)}
	    (last-todos-deleted? old-completed new-completed old-remaining new-remaining) 
     {:node :delete})))

(defn filter-state [state input-name {old-completed :completed old-remaining :remaining old-filter :filter} 
                                     {new-completed :completed new-remaining :remaining new-filter :filter}]
  (when (and old-completed old-remaining old-filter new-completed new-remaining new-filter)
	  (cond 
	    (first-todos-added? old-completed new-completed old-remaining new-remaining) 
     {:node :create :filter new-filter}
	    (and (> (+ new-completed new-remaining) 0) (not= old-filter new-filter)) 
     {:node :update :filter new-filter}
	    (last-todos-deleted? old-completed new-completed old-remaining new-remaining) 
     {:node :delete})))

(defn completed-todos [state input-name {old-completed :completed} {new-completed :completed}]
  (when (and old-completed new-completed)
	  (cond 
	    (and (<= old-completed 0) (> new-completed 0)) 
     {:node :create :completed new-completed}
	    (and (> old-completed 0) (> new-completed 0) (not= old-completed new-completed)) 
     {:node :update :completed new-completed}
	    (and (> old-completed 0) (<= new-completed 0)) 
     {:node :delete})))

;; Emits

(def ^:private initial-app-model
  [{:todo-mvc
     {:transforms
       {:add-todo [{msg/topic :todos (msg/param :text) {}}]
        :filter-todos [{msg/topic :todos (msg/param :filter) {}}]}}}])

(defn- new-todos-deltas [value]
  (vec (mapcat (fn [{:keys [id sort-index] :as todo}]
                 [[:node-create [:todo-mvc :todos-section :todos id] :map sort-index]
                  [:value [:todo-mvc :todos-section :todos id] todo]
                  [:transform-enable [:todo-mvc :todos-section :todos id] :update-todo [{msg/topic :todos (msg/param :text) {} 
                                                                                         :id id}]]
                  [:transform-enable [:todo-mvc :todos-section :todos id] :toggle-todo [{msg/topic :todos :id id}]]
                  [:transform-enable [:todo-mvc :todos-section :todos id] :delete-todo [{msg/topic :todos :id id}]]])
               (sort-by :sort-index value))))

(defn- delete-todos-deltas [value]
  (vec (mapcat (fn [{:keys [id] :as todo}]
                 [[:transform-disable [:todo-mvc :todos-section :todos id] :update-todo]
                  [:transform-disable [:todo-mvc :todos-section :todos id] :toggle-todo]
                  [:transform-disable [:todo-mvc :todos-section :todos id] :delete-todo]
                  [:node-destroy [:todo-mvc :todos-section :todos id]]])
               value)))

(defn- update-todos-deltas [value]
  (mapv (fn [{:keys [id] :as todo}]
          [:value [:todo-mvc :todos-section :todos id] todo]) value))

(defn- remaining-deltas [{op :node remaining :remaining check-all-state :check-all-state}]
  (case op
    :create [[:node-create [:todo-mvc :todos-section] :map]
             [:value [:todo-mvc :todos-section] check-all-state]
             [:transform-enable [:todo-mvc :todos-section] :toggle-todos [{msg/topic :todos}]]
             [:node-create [:todo-mvc :todos-section :todos] :map]
             [:node-create [:todo-mvc :footer] :map]
             [:node-create [:todo-mvc :footer :remaining] :map]
             [:value [:todo-mvc :footer :remaining] remaining]]
    :update [[:value [:todo-mvc :todos-section] check-all-state]
             [:value [:todo-mvc :footer :remaining] remaining]]
    :delete [[:node-destroy [:todo-mvc :footer :remaining]]
             [:node-destroy [:todo-mvc :footer]]
             [:node-destroy [:todo-mvc :todos-section :todos]]
             [:transform-disable [:todo-mvc :todos-section] :toggle-todos]
             [:node-destroy [:todo-mvc :todos-section]]]
    []))

(defn- filter-deltas [{op :node filter :filter}]
  (case op
    :create [[:node-create [:todo-mvc :footer :filter] :map]
             [:value [:todo-mvc :footer :filter] filter]]
    :update [[:value [:todo-mvc :footer :filter] filter]]
    :delete [[:node-destroy [:todo-mvc :footer :filter]]]
    []))

(defn- completed-deltas [{op :node completed :completed}]
  (case op
    :create [[:node-create [:todo-mvc :footer :completed] :map]
             [:value [:todo-mvc :footer :completed] completed]
             [:transform-enable [:todo-mvc :footer :completed] :delete-completed [{msg/topic :todos}]]]
    :update [[:value [:todo-mvc :footer :completed] completed]]
    :delete [[:transform-disable [:todo-mvc :footer :completed] :delete-completed]
             [:node-destroy [:todo-mvc :footer :completed]]]
    []))

(defn todo-mvc-emit
  ([inputs] initial-app-model)
  ([inputs changed-inputs]
     (reduce (fn [a input-name]
               (let [new-value (:new (get inputs input-name))]
                 (concat a (case input-name
                             :new-todos (new-todos-deltas new-value)
                             :deleted-todos (delete-todos-deltas new-value)
                             :updated-todos (update-todos-deltas new-value)
                             :completed (completed-deltas new-value)
                             :remaining (remaining-deltas new-value)
                             :filter (filter-deltas new-value)
                             []))))
             [] 
             changed-inputs)))

;; Dataflow

(def todos-app
  {:transform {:todos {:init {:todos {} :remaining 0 :completed 0 :filter :all} :fn todos-transform}}
   :effect {:todos send-todos-to-service}
   :combine {:new-todos {:fn new-todos-rendered :input #{:todos}}
             :updated-todos {:fn updated-todos-rendered :input #{:todos}}
             :deleted-todos {:fn deleted-todos-rendered :input #{:todos}}
             :completed {:fn completed-todos :input #{:todos}}
             :remaining {:fn remaining-todos :input #{:todos}}
             :filter {:fn filter-state :input #{:todos}}}
   :emit {:emit {:fn todo-mvc-emit :input #{:new-todos :deleted-todos :updated-todos :completed :remaining :filter}}}})
