(ns adobe.cmap.core
  "Adobe's CID-to-Unicode CMaps, parsed.

  ## What these answer

  A PDF composite font names a character collection — `Adobe-Japan1`,
  `Adobe-GB1`, `Adobe-CNS1`, `Adobe-KR`, `Adobe-Korea1` — and puts CIDs in
  the content stream. `/ToUnicode` is how a producer is supposed to say what
  those CIDs mean, and 25 of 576 `/Type0` fonts in a measured corpus do not
  ship one. For those the collection's own table is the answer, and Adobe
  publishes it.

  This is a **vendored resource**, not a clean-room reimplementation. The
  files under `resources/adobe/cmap/` are Adobe's, byte for byte, under the
  BSD-3-Clause licence reproduced beside them. Retyping a 23,000-entry
  mapping would introduce errors nobody could find; the whole value here is
  that it is Adobe's table and not somebody's transcription of it.

  ## The parser is not the PDF one, and that is deliberate

  These files are `beginbfchar`/`beginbfrange` — the same syntax a PDF
  `/ToUnicode` stream uses, so a reader already exists in `kotoba-lang/hanmen`.
  Depending on a PDF renderer to read a font resource would be backwards, and
  the parser is thirty lines. Two small parsers beat a dependency pointing
  the wrong way.

  ## Loading

  `cid->unicode` reads a resource, which is a host effect and JVM-only —
  `.cljc` here is for the parser, which is pure and portable. A caller on
  another platform reads the bytes itself and calls `parse`."
  (:require [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])))

(def orderings
  "The collections with a published table, and the file each lives in.

  `Adobe-Identity` is deliberately absent: it declares that the CIDs mean
  whatever the font says they mean, so there is no table to publish and a
  font using it without `/ToUnicode` is not decodable by anyone."
  {"Adobe-Japan1" "Adobe-Japan1-UCS2"
   "Adobe-GB1" "Adobe-GB1-UCS2"
   "Adobe-CNS1" "Adobe-CNS1-UCS2"
   "Adobe-KR" "Adobe-KR-UCS2"
   "Adobe-Korea1" "Adobe-Korea1-UCS2"})

(defn- hex->long [h]
  #?(:clj (Long/parseLong h 16) :cljs (js/parseInt h 16)))

(defn- hex->str
  "A UTF-16BE hex run as a string.

  Four hex digits per code unit, and a surrogate pair is two of them — which
  is why this builds from code UNITS rather than decoding each group as a
  code point. Doing the latter turns every emoji and every CJK extension B
  character into two replacement characters."
  [h]
  (apply str (map (fn [p] (char (hex->long (apply str p))))
                  (partition 4 4 nil h))))

(defn parse
  "A CMap's text into `{cid \"string\"}`.

  Pure, so a caller that read the bytes some other way can use it. Entries
  outside `beginbfchar`/`beginbfrange` blocks are ignored — the file is
  PostScript and most of it is prologue."
  [text]
  (let [chars (reduce
               (fn [acc block]
                 (reduce (fn [acc [_ src dst]]
                           (assoc acc (hex->long src) (hex->str dst)))
                         acc
                         (re-seq #"<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>" block)))
               {}
               (map second (re-seq #"(?s)beginbfchar(.*?)endbfchar" text)))]
    (reduce
     (fn [acc block]
       (reduce
        (fn [acc [_ lo hi arr single]]
          (let [lo (hex->long lo) hi (hex->long hi)]
            (cond
              (seq arr)
              (reduce (fn [a [i [_ v]]] (assoc a (+ lo i) (hex->str v)))
                      acc
                      (map-indexed vector (re-seq #"<([0-9A-Fa-f]+)>" arr)))

              (seq single)
              ;; A range's destination increments — it is not one string
              ;; repeated. Reading it the other way maps a whole range to a
              ;; single character, which looks like a font whose glyphs are
              ;; all the same.
              ;;
              ;; The LAST code unit increments, not the whole value: a
              ;; destination of `<D842DF9F>` is a surrogate pair, and adding
              ;; one to it as a number would walk the high surrogate.
              (let [units (mapv #(hex->long (apply str %))
                                (partition 4 4 nil single))
                    n (max 0 (- hi lo))]
                (reduce (fn [a i]
                          (assoc a (+ lo i)
                                 (apply str (map char (update units
                                                              (dec (count units))
                                                              + i)))))
                        acc
                        (range 0 (inc n))))

              :else acc)))
        acc
        (re-seq #"<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>\s*(?:\[([^\]]*)\]|<([0-9A-Fa-f]+)>)"
                block)))
     chars
     (map second (re-seq #"(?s)beginbfrange(.*?)endbfrange" text)))))

(defn known?
  "Whether a table exists for this ordering. `nil` and an unknown name are
  both false, so a caller can pass what the font said without checking."
  [ordering]
  (contains? orderings (str ordering)))

#?(:clj
   (def ^:private cache (atom {})))

#?(:clj
   (defn cid->unicode
     "The table for `ordering`, or nil.

     Memoised, because parsing one is a few hundred thousand characters of
     regex and a document that uses a collection uses it on every page. The
     tables are immutable resources on the classpath, so there is nothing to
     invalidate.

     nil rather than a throw for an unknown ordering: a font naming a
     collection nobody published is a normal thing to meet, and the caller's
     answer is the same as for a font with no table at all."
     [ordering]
     (when-let [file (get orderings (str ordering))]
       (or (get @cache ordering)
           (when-let [url (io/resource (str "adobe/cmap/" file))]
             (let [m (parse (slurp url))]
               (swap! cache assoc ordering m)
               m))))))
