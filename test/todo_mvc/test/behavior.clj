(ns todo-mvc.test.behavior
  (:require [io.pedestal.app :as app]
            [io.pedestal.app.protocols :as p]
            [io.pedestal.app.tree :as tree]
            [io.pedestal.app.messages :as msg]
            [io.pedestal.app.render :as render]
            [io.pedestal.app.util.test :as test])
  (:use clojure.test
        todo-mvc.behavior
        [io.pedestal.app.query :only [q]]))

;; Test a transform function

(deftest test-todos-transform
  (is (= (todos-transform {} {msg/type msg/init :value {:todos {} :remaining 0 :completed 0 :filter :all}})
         {:todos {} :remaining 0 :completed 0 :filter :all})))

;; Test combines functions

(deftest test-todos-combines
  (is (= (new-todos-rendered nil nil {:todos {}} {:todos {"1" {:show true} "2" {:show true}}})
         [{:id "1" :show true} {:id "2" :show true}])))

;; Build an application, send a message to a transform and check the transform
;; state

(deftest test-app-state
  (let [app (app/build todos-app)]
    (app/begin app)
    (is (vector?
          (test/run-sync! app [{msg/topic :todos msg/type msg/init :value {:todos {} :remaining 0 :completed 0 :filter :all}}])))
    (is (= (-> app :state deref :models :todos) {:todos {} :remaining 0 :completed 0 :filter :all}))
    (is (vector?
          (test/run-sync! app [{msg/topic :todos msg/type :add-todo :text "t1"}])))
    (is (= (-> app :state deref :models :todos :remaining) 1))))

;; Use io.pedestal.app.query to query the current application model

(deftest test-query-ui
  (let [app (app/build todos-app)
        app-model (render/consume-app-model app (constantly nil))]
    (app/begin app)
    (is (test/run-sync! app [{msg/topic :todos msg/type msg/init :value {:todos {} :remaining 0 :completed 0 :filter :all}}]))
    (is (= (q '[:find ?v
                :where
                [?n :t/path [:io.pedestal.app/todos]]
                [?n :t/value ?v]]
              @app-model)
           [[{:todos {} :remaining 0 :completed 0 :filter :all}]]))))
