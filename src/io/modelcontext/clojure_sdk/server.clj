(ns io.modelcontext.clojure-sdk.server
  (:require [clojure.core.async :as async]
            [io.modelcontext.clojure-sdk.mcp.errors :as mcp.errors]
            [io.modelcontext.clojure-sdk.specs :as specs]
            [jsonrpc4clj.coercer :as coercer]
            [jsonrpc4clj.server :as jsonrpc.server]
            [me.vedang.logger.interface :as log]))

;;; Helpers
;; Logging and Spec Checking
(defmacro conform-or-log
  "Provides log function for conformation, while preserving line numbers."
  [spec value]
  (let [fmeta (assoc (meta &form)
                :file *file*
                :ns-str (str *ns*))]
    `(coercer/conform-or-log
       (fn [& args#]
         (cond (= 2 (count args#)) (log/error :msg (first args#)
                                              :explain (second args#)
                                              :meta ~fmeta)
               (= 4 (count args#)) (log/error :ex (first args#)
                                              :msg (second args#)
                                              :spec ~spec
                                              :value ~value
                                              :meta ~fmeta)
               :else (throw (ex-info "Unknown Conform Error" :args args#))))
       ~spec
       ~value)))

;;; Helper functions for handling various requests

(defn store-client-info!
  [context client-info client-capabilities]
  (let [client-id (random-uuid)]
    (swap! (:connected-clients context) assoc
      client-id
      {:client-info client-info, :capabilities client-capabilities})
    client-id))

(defn- supported-protocol-version
  "Return the version of MCP protocol as part of connection initialization."
  [version]
  ;; [ref: version_negotiation]
  (if ((set specs/supported-protocol-versions) version)
    version
    (first specs/supported-protocol-versions)))

(defn- handle-initialize
  [context params]
  (let [client-info (:clientInfo params)
        client-capabilities (:capabilities params)
        server-info (:server-info context)
        server-capabilities @(:capabilities context)
        client-id (store-client-info! context client-info client-capabilities)]
    (log/trace :fn :handle-initialize
               :msg "[Initialize] Client connected!"
               :client-info client-info
               :client-id client-id)
    {:protocolVersion (supported-protocol-version (:protocolVersion params)),
     :capabilities server-capabilities,
     :serverInfo server-info}))

(defn- handle-ping [_context _params] (log/trace :fn :handle-ping) "pong")

(defn- handle-list-tools
  [context _params]
  (log/trace :fn :handle-list-tools)
  {:tools (mapv :tool (vals @(:tools context)))})

(defn coerce-tool-response
  "Coerces a tool response into the expected format.
   If the response is not sequential, wraps it in a vector.
   If the tool has an outputSchema, adds structuredContent.
   Preserves _meta field from response for bidirectional async communication.

   IMPORTANT: Uses string key \"_meta\" not keyword :_meta because jsonrpc4clj's
   camelCase converter (csk/->camelCaseString) strips underscore prefixes from
   keywords, turning :_meta into \"meta\". String keys bypass this conversion."
  [tool response]
  (let [;; Handle case where response is a map with :_meta (keyword from handler)
        meta-data (when (map? response) (:_meta response))
        ;; Extract content - if response is a map with content keys, use them
        content (cond
                  (and (map? response) (:content response)) (:content response)
                  (sequential? response) (vec response)
                  :else [response])
        content (if (sequential? content) (vec content) [content])
        base-map {:content content}]
    ;; @TODO: [ref: structured-content-should-match-output-schema-exactly]
    ;; NOTE: Must use string key "_meta" - keyword :_meta becomes "meta"
    ;; due to jsonrpc4clj camelCase conversion stripping underscore prefix
    (cond-> base-map
      (:outputSchema tool) (assoc :structuredContent content)
      meta-data (assoc "_meta" meta-data))))

(defn- handle-call-tool
  [context params]
  (log/trace :fn :handle-call-tool
             :tool (:name params)
             :args (:arguments params))
  (let [tools @(:tools context)
        tool-name (:name params)
        arguments (:arguments params)]
    (if-let [{:keys [tool handler]} (get tools tool-name)]
      (try (coerce-tool-response tool (handler arguments))
           (catch Exception e
             {:content [{:type "text", :text (str "Error: " (.getMessage e))}],
              :isError true}))
      (do
        (log/debug :fn :handle-call-tool :tool tool-name :error :tool-not-found)
        {:error (mcp.errors/body :tool-not-found {:tool-name tool-name})}))))

(defn- handle-list-resources
  [context _params]
  (log/trace :fn :handle-list-resources)
  {:resources (mapv :resource (vals @(:resources context)))})

(defn- handle-read-resource
  [context params]
  (log/trace :fn :handle-read-resource :resource (:uri params))
  (let [resources @(:resources context)
        uri (:uri params)]
    (if-let [{:keys [handler]} (get resources uri)]
      {:contents [(handler uri)]}
      (do (log/debug :fn :handle-read-resource
                     :resource uri
                     :error :resource-not-found)
          {:error (mcp.errors/body :resource-not-found {:uri uri})}))))

(defn- handle-list-prompts
  [context _params]
  (log/trace :fn :handle-list-prompts)
  {:prompts (mapv :prompt (vals @(:prompts context)))})

(defn- handle-get-prompt
  [context params]
  (log/trace :fn :handle-get-prompt
             :prompt (:name params)
             :args (:arguments params))
  (let [prompts @(:prompts context)
        prompt-name (:name params)
        arguments (:arguments params)]
    (if-let [{:keys [handler]} (get prompts prompt-name)]
      (handler arguments)
      (do (log/debug :fn :handle-get-prompt
                     :prompt prompt-name
                     :error :prompt-not-found)
          {:error (mcp.errors/body :prompt-not-found
                                   {:prompt-name prompt-name})}))))

;;; Requests and Notifications

;; [ref: initialize_request]
(defmethod jsonrpc.server/receive-request "initialize"
  [_ context params]
  (log/trace :fn :receive-request :method "initialize" :params params)
  ;; [tag: log_bad_input_params]
  ;;
  ;; If the input is non-conformant, we should log it. But we shouldn't
  ;; take any other action. The principle we want to follow is Postel's
  ;; law: https://en.wikipedia.org/wiki/Robustness_principle
  (conform-or-log ::specs/initialize-request params)
  (->> params
       (handle-initialize context)
       (conform-or-log ::specs/initialize-response)))

;; [ref: initialized_notification]
(defmethod jsonrpc.server/receive-notification "notifications/initialized"
  [_ _ params]
  (conform-or-log ::specs/initialized-notification params))

;; [ref: ping_request]
(defmethod jsonrpc.server/receive-request "ping"
  [_ context params]
  (log/trace :fn :receive-request :method "ping" :params params)
  ;; [ref: log_bad_input_params]
  (conform-or-log ::specs/ping-request params)
  (->> params
       (handle-ping context)))

;; [ref: list_tools_request]
(defmethod jsonrpc.server/receive-request "tools/list"
  [_ context params]
  (log/trace :fn :receive-request :method "tools/list" :params params)
  ;; [ref: log_bad_input_params]
  (conform-or-log ::specs/list-tools-request params)
  (->> params
       (handle-list-tools context)
       (conform-or-log ::specs/list-tools-response)))

;; [ref: call_tool_request]
(defmethod jsonrpc.server/receive-request "tools/call"
  [_ context params]
  (log/trace :fn :receive-request :method "tools/call" :params params)
  ;; [ref: log_bad_input_params]
  (conform-or-log ::specs/call-tool-request params)
  (->> params
       (handle-call-tool context)
       (conform-or-log ::specs/call-tool-response)))

;; [ref: list_resources_request]
(defmethod jsonrpc.server/receive-request "resources/list"
  [_ context params]
  (log/trace :fn :receive-request :method "resources/list" :params params)
  ;; [ref: log_bad_input_params]
  (conform-or-log ::specs/list-resources-request params)
  (->> params
       (handle-list-resources context)
       (conform-or-log ::specs/list-resources-response)))

;; [ref: read_resource_request]
(defmethod jsonrpc.server/receive-request "resources/read"
  [_ context params]
  (log/trace :fn :receive-request :method "resources/read" :params params)
  ;; [ref: log_bad_input_params]
  (conform-or-log ::specs/read-resource-request params)
  (->> params
       (handle-read-resource context)
       (conform-or-log ::specs/read-resource-response)))

;; [ref: list_prompts_request]
(defmethod jsonrpc.server/receive-request "prompts/list"
  [_ context params]
  (log/trace :fn :receive-request :method "prompts/list" :params params)
  ;; [ref: log_bad_input_params]
  (conform-or-log ::specs/list-prompts-request params)
  (->> params
       (handle-list-prompts context)
       (conform-or-log ::specs/list-prompts-response)))

;; [ref: get_prompt_request]
(defmethod jsonrpc.server/receive-request "prompts/get"
  [_ context params]
  (log/trace :fn :receive-request :method "prompts/get" :params params)
  ;; [ref: log_bad_input_params]
  (conform-or-log ::specs/get-prompt-request params)
  (->> params
       (handle-get-prompt context)
       (conform-or-log ::specs/get-prompt-response)))

;;; @TODO: Requests to Implement

;; [ref: list_resource_templates_request]
(defmethod jsonrpc.server/receive-request "resources/templates/list"
  [_ _context params]
  (log/trace :fn :receive-request
             :method "resources/templates/list"
             :params params)
  ;; [ref: log_bad_input_params]
  (conform-or-log ::specs/list-resource-templates-request params)
  (identity ::specs/list-resource-templates-response)
  ::jsonrpc.server/method-not-found)

;; [ref: resource_subscribe_unsubscribe_request]
(defmethod jsonrpc.server/receive-request "resources/subscribe"
  [_ _context params]
  (log/trace :fn :receive-request :method "resources/subscribe" :params params)
  ;; [ref: log_bad_input_params]
  (conform-or-log ::specs/resource-subscribe-unsubscribe-request params)
  ::jsonrpc.server/method-not-found)

;; [ref: resource_subscribe_unsubscribe_request]
(defmethod jsonrpc.server/receive-request "resources/unsubscribe"
  [_ _context params]
  (log/trace :fn :receive-request
             :method "resources/unsubscribe"
             :params params)
  ;; [ref: log_bad_input_params]
  (conform-or-log ::specs/resource-subscribe-unsubscribe-request params)
  ::jsonrpc.server/method-not-found)

;; [ref: set_logging_level_request]
(defmethod jsonrpc.server/receive-request "logging/setLevel"
  [_ _context params]
  (log/trace :fn :receive-request :method "logging/setLevel" :params params)
  ;; [ref: log_bad_input_params]
  (conform-or-log ::specs/set-logging-level-request params)
  ::jsonrpc.server/method-not-found)

;; [ref: complete_request]
(defmethod jsonrpc.server/receive-request "completion/complete"
  [_ _context params]
  (log/trace :fn :receive-request :method "completion/complete" :params params)
  ;; [ref: log_bad_input_params]
  (conform-or-log ::specs/complete-request params)
  (identity ::specs/complete-response)
  ::jsonrpc.server/method-not-found)

;;; Notification Receivers
;; Notifications received from the client.

;; [ref: cancelled_notification]
(defmethod jsonrpc.server/receive-notification "notifications/cancelled"
  [_ context params]
  (log/trace :fn :receive-notification
             :method "notifications/cancelled"
             :params params)
  (conform-or-log ::specs/cancelled-notification params)
  ;; Record the cancellation. Handlers can check this atom to abort work.
  (when-let [request-id (:requestId params)]
    (when-let [cancelled (:cancelled-requests context)]
      (swap! cancelled conj request-id)
      (log/debug :msg "Request cancelled by client"
                 :request-id request-id
                 :reason (:reason params))))
  nil)

;; [ref: progress_notification]
(defmethod jsonrpc.server/receive-notification "notifications/progress"
  [_ _context params]
  (log/trace :fn :receive-notification
             :method "notifications/progress"
             :params params)
  (conform-or-log ::specs/progress-notification params)
  ;; Progress notifications from client are informational; log and continue.
  (log/debug :msg "Progress notification received"
             :progress-token (:progressToken params)
             :progress (:progress params)
             :total (:total params))
  nil)

;; [ref: roots_list_changed_notification]
;; Client informs server that the list of roots has changed.
(defmethod jsonrpc.server/receive-notification "notifications/roots/list_changed"
  [_ context params]
  (log/trace :fn :receive-notification
             :method "notifications/roots/list_changed"
             :params params)
  (conform-or-log ::specs/root-list-changed-notification params)
  ;; Invoke callback if registered, so server can re-request roots.
  (when-let [on-roots-changed (:on-roots-changed context)]
    (try (on-roots-changed context)
         (catch Exception e
           (log/error :msg "Error in on-roots-changed callback"
                      :error (.getMessage e)))))
  nil)

;;; Notification Senders
;; Functions for sending notifications from server to client.
;; All take context (which must contain :server) and conform params to spec.

(defn send-notification!
  "Send a JSON-RPC notification from server to client.
   Requires :server key in context (set during start!)."
  [context method params]
  (if-let [server (:server context)]
    (do (log/trace :fn :send-notification! :method method)
        (jsonrpc.server/send-notification server method params))
    (log/error :fn :send-notification!
               :msg "Cannot send notification: no :server in context"
               :method method)))

;; [ref: tool_list_changed_notification]
(defn send-tools-list-changed!
  "Notify client that the list of available tools has changed.
   method: notifications/tools/list_changed"
  [context]
  (let [params {}]
    (conform-or-log ::specs/tool-list-changed-notification params)
    (send-notification! context "notifications/tools/list_changed" params)))

;; [ref: resource_list_changed_notification]
(defn send-resources-list-changed!
  "Notify client that the list of available resources has changed.
   method: notifications/resources/list_changed"
  [context]
  (let [params {}]
    (conform-or-log ::specs/resource-list-changed-notification params)
    (send-notification! context "notifications/resources/list_changed" params)))

;; [ref: resource_updated_notification]
(defn send-resource-updated!
  "Notify client that a specific resource has been updated.
   Only sent if client previously subscribed via resources/subscribe.
   method: notifications/resources/updated"
  [context uri]
  (let [params {:uri uri}]
    (conform-or-log ::specs/resource-updated-notification params)
    (send-notification! context "notifications/resources/updated" params)))

;; [ref: prompt_list_changed_notification]
(defn send-prompts-list-changed!
  "Notify client that the list of available prompts has changed.
   method: notifications/prompts/list_changed"
  [context]
  (let [params {}]
    (conform-or-log ::specs/prompt-list-changed-notification params)
    (send-notification! context "notifications/prompts/list_changed" params)))

;; [ref: progress_notification] (server -> client)
(defn send-progress!
  "Send a progress notification for a long-running request.
   progress-token: opaque token from the original request's _meta.progressToken
   progress: current progress value (should increase monotonically)
   total: optional total value (nil if unknown)"
  [context progress-token progress & {:keys [total]}]
  (let [params (cond-> {:progressToken progress-token
                        :progress progress}
                 total (assoc :total total))]
    ;; Validate against spec (logs on non-conformance, Postel's law)
    (conform-or-log ::specs/progress-notification params)
    (send-notification! context "notifications/progress" params)))

;; [ref: cancelled_notification] (server -> client)
(defn send-cancelled!
  "Notify client that the server is cancelling a previously-issued request.
   request-id: the id of the request being cancelled
   reason: optional human-readable reason"
  [context request-id & {:keys [reason]}]
  (let [params (cond-> {:requestId request-id}
                 reason (assoc :reason reason))]
    (conform-or-log ::specs/cancelled-notification params)
    (send-notification! context "notifications/cancelled" params)))

;; [ref: logging_message_notification]
(defn send-log-message!
  "Send a logging message notification to the client.
   level: one of \"debug\" \"info\" \"notice\" \"warning\" \"error\"
          \"critical\" \"alert\" \"emergency\"
   data: the log data (any JSON-serializable value)
   logger: optional logger name string"
  [context level data & {:keys [logger]}]
  (let [params (cond-> {:level level :data data}
                 logger (assoc :logger logger))]
    (conform-or-log ::specs/logging-message-notification params)
    (send-notification! context "notifications/message" params)))

;;; Server Spec

(defn validate-spec!
  "Validates the server-spec and throws if invalid.
   Strips handler functions before logging to avoid JSON serialization errors."
  [server-spec]
  (when-not (specs/valid-server-spec? server-spec)
    (let [msg "Invalid server-spec definition"
          ;; Strip handlers before logging to avoid JSON serialization errors
          loggable-spec (-> server-spec
                            (update :tools (fn [tools] (mapv #(dissoc % :handler) tools)))
                            (update :prompts (fn [prompts] (mapv #(dissoc % :handler) prompts)))
                            (update :resources (fn [resources] (mapv #(dissoc % :handler) resources))))]
      (log/debug :msg msg :spec loggable-spec)
      (throw (ex-info msg (specs/explain-server-spec server-spec)))))
  server-spec)

(defn register-tool!
  "Register a tool and optionally notify connected clients of the change.
   Sends notifications/tools/list_changed if :server is present in context."
  [context tool handler]
  (swap! (:tools context) assoc (:name tool) {:tool tool, :handler handler})
  (when (:server context)
    (send-tools-list-changed! context)))

(defn unregister-tool!
  "Remove a tool by name and notify connected clients of the change."
  [context tool-name]
  (swap! (:tools context) dissoc tool-name)
  (when (:server context)
    (send-tools-list-changed! context)))

(defn register-resource!
  "Register a resource and optionally notify connected clients of the change.
   Sends notifications/resources/list_changed if :server is present in context."
  [context resource handler]
  (swap! (:resources context) assoc
    (:uri resource)
    {:resource resource, :handler handler})
  (when (:server context)
    (send-resources-list-changed! context)))

(defn unregister-resource!
  "Remove a resource by URI and notify connected clients of the change."
  [context uri]
  (swap! (:resources context) dissoc uri)
  (when (:server context)
    (send-resources-list-changed! context)))

(defn register-prompt!
  "Register a prompt and optionally notify connected clients of the change.
   Sends notifications/prompts/list_changed if :server is present in context."
  [context prompt handler]
  (swap! (:prompts context) assoc
    (:name prompt)
    {:prompt prompt, :handler handler})
  (when (:server context)
    (send-prompts-list-changed! context)))

(defn unregister-prompt!
  "Remove a prompt by name and notify connected clients of the change."
  [context prompt-name]
  (swap! (:prompts context) dissoc prompt-name)
  (when (:server context)
    (send-prompts-list-changed! context)))

(defn- create-empty-context
  [name version]
  (log/trace :fn :create-empty-context)
  ;; [tag: context_must_be_a_map]
  ;;
  ;; Since so much of the state is "global" in nature, it's tempting to
  ;; just make the entire context global instead of defining atoms at each
  ;; key. However, do not do this!
  ;;
  ;; This context is passed to lsp4j, which expects the data-structure to
  ;; be `associative?` in nature and uses it further for it's own temporary
  ;; state.
  {:server-info {:name name, :version version},
   :tools (atom {}),
   :resources (atom {}),
   :prompts (atom {}),
   :protocol (atom nil),
   :capabilities (atom {:tools {}, :resources {}, :prompts {}}),
   :connected-clients (atom {}),
   ;; Set of request IDs that have been cancelled by the client.
   ;; Handlers can check (contains? @(:cancelled-requests ctx) request-id)
   ;; to abort long-running work early.
   :cancelled-requests (atom #{})})

(defn create-context!
  "Create and configure an MCP server from a configuration map.
   Config map should have the shape:
   {:name \"server-name\"
    :version \"1.0.0\"
    :tools [{:name \"tool-name\"
             :description \"Tool description\"
             :inputSchema {...}
             :handler (fn [args] ...)}]
    :prompts [{:name \"prompt-name\"
               :description \"Prompt description\"
               :handler (fn [args] ...)}]
    :resources [{:uri \"resource-uri\"
                 :type \"text\"
                 :handler (fn [uri] ...)}]}"
  [{:keys [name version tools prompts resources], :as spec}]
  (validate-spec! spec)
  (log/with-context {:action :create-context!}
    (let [context (create-empty-context name version)]
      (when (> (count tools) 0)
        (log/debug :num-tools (count tools)
                   :msg "Registering tools"
                   :server-info {:name name, :version version}))
      (doseq [tool tools]
        (register-tool! context (dissoc tool :handler) (:handler tool)))
      (when (> (count resources) 0)
        (log/debug :num-resources (count resources)
                   :msg "Registering resources"
                   :server-info {:name name, :version version}))
      (doseq [resource resources]
        (register-resource! context
                            (dissoc resource :handler)
                            (:handler resource)))
      (when (> (count prompts) 0)
        (log/debug :num-prompts (count prompts)
                   :msg "Registering prompts"
                   :server-info {:name name, :version version}))
      (doseq [prompt prompts]
        (register-prompt! context (dissoc prompt :handler) (:handler prompt)))
      context)))

(defn request-cancelled?
  "Check if a request has been cancelled by the client.
   Useful for long-running tool handlers to check and abort early."
  [context request-id]
  (contains? @(:cancelled-requests context) request-id))

(defn start!
  "Start the server and associate it with the context for notification sending.
   The :server key in context is required for send-*! notification functions."
  [server context]
  (log/info :msg "[SERVER] Starting server...")
  ;; Associate server with context so notification senders can find it.
  ;; NOTE: This mutates the context map by assoc'ing :server. Since context
  ;; is a plain map (not an atom), this works because jsonrpc.server/start
  ;; receives the enriched context.
  (let [context (assoc context :server server)]
    (jsonrpc.server/start server context)))

(defn chan-server
  []
  (let [input-ch (async/chan 3)
        output-ch (async/chan 3)]
    (jsonrpc.server/chan-server {:output-ch output-ch, :input-ch input-ch})))
