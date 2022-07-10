(ns ripley.alpine
  "Alpine.js helpers for Ripley."
  (:require [ripley.html :as h]
            [cheshire.core :as cheshire]
            [clojure.string :as str]
            [ripley.live.protocols :as p]
            [ripley.impl.dynamic :as dynamic]
            [ripley.impl.output :as output]
            [ripley.live.source :as source]
            [ripley.template :as template]
            [clojure.set :as set]))

(defn alpine-js-script
  "Script to add Alpine.js to the page."
  []
  (h/html
   [:script {:src "//unpkg.com/alpinejs" :defer true}])
  ;; Add new patch extension "AM" (alpine merge) we use to update the $data
  (h/html
   [:script
    "function ripley_patch_AM(elt,payload) {
  let e = Alpine.$data(elt);
  Object.keys(payload).forEach(k=>e[k]=payload[k]);
};"]))

(defn x-for
  "Render a live collection using alpine x-for template.

  Options:

  :container-element  keyword specifying the container element, defaults to :span
  :source      live source for the items, must be serializable to JSON
  :index-name  index name to use for Alpine directives (defaults to \"index\").
  :render      template for rendering the items

  Template is called with a special proxy map that will return accessor
  expressions so they can be used in `x-*` directives directly.

  Any items accessed from the proxy must be JSON serializable.
  "
  [{:keys [container-element source render index-name]
    :or {index-name "index"
         container-element :span}}]
  (let [ctx dynamic/*live-context*
        container-element-name (h/element-name container-element)
        container-element-classes (h/element-class-names container-element)
        ;; Register collection without component (we send patches manually)
        collection-id (p/register! ctx nil :_ignore {})

        x-for-attr (str "(it," index-name ") in items")
        initial-collection (or (p/current-value source) [])

        template-data (template/->template-data-proxy "it[%d]")
        template-html (output/render-to-string
                       render template-data)
        fields-fn (template/template-data-proxy-fields template-data)]

    (p/listen!
     source
     (fn x-for-listener
       [new-collection]
       ;; TODO: optimize this to do removes, and additions efficiently
       (p/send! ctx [[collection-id "AM" (mapv fields-fn new-collection)]])))

    (h/out! "<" container-element-name
            " data-rl=\"" collection-id "\""
            (when (seq container-element-classes)
              (str " class=\"" (str/join " " container-element-classes)))
            " x-data=\"")
    (h/dyn! (cheshire/encode (mapv fields-fn initial-collection)))
    (h/out! "\"><template x-for=\"" x-for-attr "\">"
            template-html
            "</template></" container-element-name ">")))

(defn changed-keys [a b]
  (into {}
        (keep (fn [[new-key new-val :as mapping]]
                (let [old-val (get a new-key ::no-value)]
                  (when (not= old-val new-val)
                    mapping))))
        b))

(defn x-data
  "Render container element with a live source as Alpine.js data.

  The source data must be a map. The map is diffed and any changed
  fields are sent to the client and merged.
  No keys will be removed from. The child component will not be
  rerendered by Ripley.

  If data option is provided, it will also be added to the x-data
  attribute on the client side. It will not be updated but is
  meant for client side properties.

  The child component is called with a special data proxy map
  that keeps track of what values the component uses.

  Only changes in used keys will be sent to the client.
  "
  [{:keys [container-element source data]
    :or {container-element :span}}
   component]
  (let [ctx dynamic/*live-context*
        id (p/register! ctx nil :_ignore {})
        source (source/source source)


        tdp (template/->template-data-proxy "_%d")
        html (output/render-to-string component tdp)
        fields (::template/fields @(.-state tdp))
        k->n (into {}
                   (map (fn [[k idx]]
                          [k (format "_%d" idx)]))
                   fields)
        remap #(set/rename-keys % k->n)

        initial-value (remap (p/current-value source))
        old-value (atom initial-value)
        element-name (h/element-name container-element)]
    (p/listen!
     source
     (fn x-data-listener
       [new-value]
       (let [new-value (remap new-value)
             payload (changed-keys @old-value new-value)]
         (def *p payload)
         (reset! old-value new-value)
         (when-not (empty? payload)
           (p/send! ctx [[id "AM" payload]])))))
    (h/out! "<" element-name
            " data-rl=\"" id "\""
            " x-data=\"")
    (h/dyn! (cheshire/encode (merge data initial-value)))
    (h/out! "\"")
    (let [cls (h/element-class-names container-element)]
      (when (seq cls)
        (h/out! " class=\"" (str/join " " cls) "\"")))
    (h/out! ">"
            html
            "</" element-name ">")))

(defn callback [callback-fn & parameter-names]
  (str "_rs("
       (p/register-callback! dynamic/*live-context* callback-fn)
       ",[" (str/join "," parameter-names) "])"))
