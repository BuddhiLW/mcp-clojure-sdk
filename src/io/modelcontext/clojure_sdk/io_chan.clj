(ns io.modelcontext.clojure-sdk.io-chan
  (:require [babashka.json :as json]
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [me.vedang.logger.interface :as log]))

(set! *warn-on-reflection* true)

;;;; IO <-> chan

;; Follow the MCP spec for reading and writing JSON-RPC messages. Convert the
;; messages to and from Clojure hashmaps and shuttle them to core.async
;; channels.

;; https://modelcontextprotocol.io/specification

(defn ^:private read-message
  "Read a single JSON-RPC message from the input.
   Returns:
     ::eof         - clean end-of-stream (.readLine returned nil)
     :parse-error  - malformed JSON (recoverable, skip and continue)
     nil           - json/read-str returned nil (JSON null literal)
     <map>         - valid parsed JSON-RPC message"
  [^java.io.BufferedReader input]
  (try (let [content (.readLine input)]
         (log/trace :fn :read-message :line content)
         (if (nil? content)
           ::eof
           (json/read-str content)))
       (catch Exception ex (log/error :fn :read-message :ex ex) :parse-error)))

(defn ^:private kw->camelCaseString
  "Convert keywords to camelCase strings, but preserve capitalization of things
  that are already strings."
  [k]
  (cond-> k (keyword? k) csk/->camelCaseString))

(def ^:private write-lock (Object.))

(defn ^:private write-message
  [^java.io.BufferedWriter output msg]
  (let [content (json/write-str (cske/transform-keys kw->camelCaseString msg))]
    (locking write-lock
      (doto output (.write ^String content) (.newLine) (.flush)))))

(defn input-stream->input-chan
  "Returns a channel which will yield parsed messages that have been read off
  the `input`. When the input is closed, closes the channel. By default when the
  channel closes, will close the input, but can be determined by `close?`.

  Reads in a thread to avoid blocking a go block thread.

  EOF handling: When stdin closes (e.g. Emacs parent exits), the channel is
  closed gracefully. Parse errors skip the bad message and continue reading.
  nil values from json/read-str (JSON null) are skipped to prevent
  AssertionError on the core.async channel."
  [input]
  (log/trace :fn :input-stream->input-chan :msg "Creating new input-chan")
  (let [messages (async/chan 1)]
    ;; close output when channel closes
    (async/thread
      (with-open [reader (io/reader (io/input-stream input))]
        (loop []
          (let [msg (read-message reader)]
            (cond
              ;; Clean EOF - stdin closed, shut down gracefully
              (= msg ::eof)
              (do (log/info :fn :input-stream->input-chan
                            :msg "Stdin EOF detected, closing channel gracefully")
                  (async/close! messages))

              ;; Parse error - log warning and skip (don't kill server)
              (= msg :parse-error)
              (do (log/warn :fn :input-stream->input-chan
                            :msg "JSON parse error, skipping malformed message")
                  (recur))

              ;; nil from json/read-str (JSON null) - skip to prevent
              ;; AssertionError: Can't put nil on channel
              (nil? msg)
              (do (log/debug :fn :input-stream->input-chan
                             :msg "Skipping nil message (JSON null)")
                  (recur))

              ;; Valid message - put on channel
              :else
              (do (log/trace :fn :input-stream->input-chan :msg msg)
                  (when (async/>!! messages msg)
                    ;; wait for next message
                    (recur))))))))
    messages))

(defn output-stream->output-chan
  "Returns a channel which expects to have messages put on it. nil values are
  not allowed. Serializes and writes the messages to the output. When the
  channel is closed, closes the output.

  Writes in a thread to avoid blocking a go block thread."
  [output]
  (let [messages (async/chan 1)]
    ;; close output when channel closes
    (async/thread (with-open [writer (io/writer (io/output-stream output))]
                    (loop []
                      (when-let [msg (async/<!! messages)]
                        (log/trace :fn :output-stream->output-chan :msg msg)
                        (try
                          (write-message writer msg)
                          (catch Throwable e (async/close! messages) (throw e)))
                        (recur)))))
    messages))
