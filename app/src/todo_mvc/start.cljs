(ns todo-mvc.start
  (:require [io.pedestal.app.protocols :as p]
            [io.pedestal.app :as app]
            [io.pedestal.app.render.push :as push-render]
            [io.pedestal.app.render :as render]
            [todo-mvc.behavior :as behavior]
            [todo-mvc.rendering :as rendering]))

(defn create-app [render-config]
  (let [app (app/build behavior/todos-app)
        render-fn (push-render/renderer "content" render-config)
        app-model (render/consume-app-model app render-fn)]
    ;; If services existed, configure the application to send all
    ;; effects there.
    ;; (app/consume-effect app services-fn)
    ;;
    ;; Start the application
    (app/begin app)
{:app app :app-model app-model}))

(defn ^:export main []
  (create-app (rendering/render-config)))
