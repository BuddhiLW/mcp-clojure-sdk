(ns io.modelcontext.clojure-sdk.notification-test
  (:require [clojure.core.async :as async]
            [clojure.test :refer [deftest is testing]]
            [io.modelcontext.clojure-sdk.server :as server]
            [io.modelcontext.clojure-sdk.test-helper :as h]
            [jsonrpc4clj.server :as jsonrpc.server]))

;;; Test fixtures

(def test-tool
  {:name "test-tool"
   :description "A test tool"
   :inputSchema {:type "object" :properties {"arg" {:type "string"}}}
   :handler (fn [_] {:type "text" :text "ok"})})

(def test-resource
  {:uri "file:///test.txt"
   :name "Test File"
   :description "A test file"
   :mimeType "text/plain"
   :handler (fn [uri] {:uri uri :mimeType "text/plain" :text "hello"})})

(def test-prompt
  {:name "test-prompt"
   :description "A test prompt"
   :handler (fn [_] {:messages [{:role "assistant"
                                  :content {:type "text" :text "hi"}}]})})

(defn- make-context
  "Create a test context with optional server."
  ([] (make-context {}))
  ([opts]
   (let [context (server/create-context!
                   {:name "test-server"
                    :version "1.0.0"
                    :tools []
                    :prompts []
                    :resources []})]
     (if-let [server (:server opts)]
       (assoc context :server server)
       context))))

;;; Tests for notification receivers

(deftest cancelled-notification-receiver
  (testing "notifications/cancelled records cancellation in context"
    (let [context (make-context)]
      ;; Simulate receiving a cancelled notification
      (jsonrpc.server/receive-notification
        "notifications/cancelled"
        context
        {:requestId 42 :reason "No longer needed"})
      (is (contains? @(:cancelled-requests context) 42)
          "Request ID should be recorded in cancelled-requests"))))

(deftest cancelled-notification-receiver-without-reason
  (testing "notifications/cancelled works without reason"
    (let [context (make-context)]
      (jsonrpc.server/receive-notification
        "notifications/cancelled"
        context
        {:requestId 99})
      (is (contains? @(:cancelled-requests context) 99)))))

(deftest progress-notification-receiver
  (testing "notifications/progress is handled without error"
    ;; Progress from client is informational only - just verify no exception
    (is (nil? (jsonrpc.server/receive-notification
                "notifications/progress"
                {}
                {:progressToken "tok-1" :progress 50 :total 100})))))

(deftest roots-list-changed-receiver
  (testing "notifications/roots/list_changed invokes callback"
    (let [called (atom false)
          context (assoc (make-context)
                    :on-roots-changed (fn [_ctx] (reset! called true)))]
      (jsonrpc.server/receive-notification
        "notifications/roots/list_changed"
        context
        {})
      (is @called "on-roots-changed callback should have been invoked")))

  (testing "notifications/roots/list_changed handles missing callback gracefully"
    (let [context (make-context)]
      ;; No :on-roots-changed in context - should not throw
      (is (nil? (jsonrpc.server/receive-notification
                  "notifications/roots/list_changed"
                  context
                  {}))))))

;;; Tests for request-cancelled? helper

(deftest request-cancelled-helper
  (testing "request-cancelled? returns false for non-cancelled requests"
    (let [context (make-context)]
      (is (not (server/request-cancelled? context 1)))))

  (testing "request-cancelled? returns true after cancellation"
    (let [context (make-context)]
      (swap! (:cancelled-requests context) conj 42)
      (is (server/request-cancelled? context 42)))))

;;; Tests for notification senders

(deftest send-tools-list-changed-test
  (testing "send-tools-list-changed! sends notification through server"
    (let [server (server/chan-server)
          context (assoc (make-context) :server server)
          _join (jsonrpc.server/start server context)]
      ;; The notification should appear on the output channel
      (server/send-tools-list-changed! context)
      (let [msg (h/take-or-timeout (:output-ch server) 500)]
        (is (not= :timeout msg) "Should receive notification")
        (is (= "notifications/tools/list_changed" (:method msg))
            "Method should be notifications/tools/list_changed"))
      (jsonrpc.server/shutdown server))))

(deftest send-resources-list-changed-test
  (testing "send-resources-list-changed! sends notification through server"
    (let [server (server/chan-server)
          context (assoc (make-context) :server server)
          _join (jsonrpc.server/start server context)]
      (server/send-resources-list-changed! context)
      (let [msg (h/take-or-timeout (:output-ch server) 500)]
        (is (not= :timeout msg) "Should receive notification")
        (is (= "notifications/resources/list_changed" (:method msg))))
      (jsonrpc.server/shutdown server))))

(deftest send-resource-updated-test
  (testing "send-resource-updated! sends notification with uri"
    (let [server (server/chan-server)
          context (assoc (make-context) :server server)
          _join (jsonrpc.server/start server context)]
      (server/send-resource-updated! context "file:///test.txt")
      (let [msg (h/take-or-timeout (:output-ch server) 500)]
        (is (not= :timeout msg) "Should receive notification")
        (is (= "notifications/resources/updated" (:method msg)))
        (is (= "file:///test.txt" (get-in msg [:params :uri]))))
      (jsonrpc.server/shutdown server))))

(deftest send-prompts-list-changed-test
  (testing "send-prompts-list-changed! sends notification through server"
    (let [server (server/chan-server)
          context (assoc (make-context) :server server)
          _join (jsonrpc.server/start server context)]
      (server/send-prompts-list-changed! context)
      (let [msg (h/take-or-timeout (:output-ch server) 500)]
        (is (not= :timeout msg) "Should receive notification")
        (is (= "notifications/prompts/list_changed" (:method msg))))
      (jsonrpc.server/shutdown server))))

(deftest send-progress-test
  (testing "send-progress! sends notification with progress data"
    (let [server (server/chan-server)
          context (assoc (make-context) :server server)
          _join (jsonrpc.server/start server context)]
      (server/send-progress! context "token-1" 50 :total 100)
      (let [msg (h/take-or-timeout (:output-ch server) 500)]
        (is (not= :timeout msg) "Should receive notification")
        (is (= "notifications/progress" (:method msg)))
        (is (= "token-1" (get-in msg [:params :progressToken])))
        (is (= 50 (get-in msg [:params :progress])))
        (is (= 100 (get-in msg [:params :total]))))
      (jsonrpc.server/shutdown server)))

  (testing "send-progress! works without total"
    (let [server (server/chan-server)
          context (assoc (make-context) :server server)
          _join (jsonrpc.server/start server context)]
      (server/send-progress! context "token-2" 75)
      (let [msg (h/take-or-timeout (:output-ch server) 500)]
        (is (not= :timeout msg) "Should receive notification")
        (is (nil? (get-in msg [:params :total]))))
      (jsonrpc.server/shutdown server))))

(deftest send-cancelled-test
  (testing "send-cancelled! sends notification with request-id and reason"
    (let [server (server/chan-server)
          context (assoc (make-context) :server server)
          _join (jsonrpc.server/start server context)]
      (server/send-cancelled! context 42 :reason "Timeout")
      (let [msg (h/take-or-timeout (:output-ch server) 500)]
        (is (not= :timeout msg) "Should receive notification")
        (is (= "notifications/cancelled" (:method msg)))
        (is (= 42 (get-in msg [:params :requestId])))
        (is (= "Timeout" (get-in msg [:params :reason]))))
      (jsonrpc.server/shutdown server))))

(deftest send-log-message-test
  (testing "send-log-message! sends notification with level and data"
    (let [server (server/chan-server)
          context (assoc (make-context) :server server)
          _join (jsonrpc.server/start server context)]
      (server/send-log-message! context "info" "Server started" :logger "mcp.core")
      (let [msg (h/take-or-timeout (:output-ch server) 500)]
        (is (not= :timeout msg) "Should receive notification")
        (is (= "notifications/message" (:method msg)))
        (is (= "info" (get-in msg [:params :level])))
        (is (= "Server started" (get-in msg [:params :data])))
        (is (= "mcp.core" (get-in msg [:params :logger]))))
      (jsonrpc.server/shutdown server))))

;;; Tests for register/unregister with notifications

(deftest register-tool-sends-notification
  (testing "register-tool! sends list_changed when server is present"
    (let [server (server/chan-server)
          context (assoc (make-context) :server server)
          _join (jsonrpc.server/start server context)]
      (server/register-tool!
        context
        {:name "new-tool"
         :description "Dynamically added"
         :inputSchema {:type "object"}}
        (fn [_] {:type "text" :text "dynamic"}))
      (let [msg (h/take-or-timeout (:output-ch server) 500)]
        (is (not= :timeout msg) "Should receive list_changed notification")
        (is (= "notifications/tools/list_changed" (:method msg))))
      ;; Verify tool was actually registered
      (is (get @(:tools context) "new-tool"))
      (jsonrpc.server/shutdown server)))

  (testing "register-tool! does NOT send notification without server"
    (let [context (make-context)]
      ;; No :server in context - should register without error
      (server/register-tool!
        context
        {:name "quiet-tool"
         :description "No notification"
         :inputSchema {:type "object"}}
        (fn [_] {:type "text" :text "quiet"}))
      (is (get @(:tools context) "quiet-tool")))))

(deftest unregister-tool-sends-notification
  (testing "unregister-tool! sends list_changed and removes tool"
    (let [server (server/chan-server)
          context (assoc (make-context) :server server)
          _join (jsonrpc.server/start server context)]
      ;; First register (consumes one notification)
      (server/register-tool!
        context
        {:name "temp-tool"
         :description "Will be removed"
         :inputSchema {:type "object"}}
        identity)
      (h/take-or-timeout (:output-ch server) 500) ;; consume register notification

      ;; Now unregister
      (server/unregister-tool! context "temp-tool")
      (let [msg (h/take-or-timeout (:output-ch server) 500)]
        (is (not= :timeout msg) "Should receive list_changed notification")
        (is (= "notifications/tools/list_changed" (:method msg))))
      (is (nil? (get @(:tools context) "temp-tool")))
      (jsonrpc.server/shutdown server))))

(deftest send-notification-without-server
  (testing "send-notification! logs error but does not throw without server"
    (let [context (make-context)]
      ;; No :server - should not throw, just log error
      (is (nil? (server/send-notification! context "test/method" {}))))))

;;; Tests for context structure

(deftest context-has-cancelled-requests
  (testing "create-context! includes cancelled-requests atom"
    (let [context (make-context)]
      (is (some? (:cancelled-requests context)))
      (is (set? @(:cancelled-requests context)))
      (is (empty? @(:cancelled-requests context))))))
