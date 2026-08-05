(ns adobe.cmap.core-test
  "The tables, and the two ways a CMap range is misread.

  Spot checks against Adobe's own table rather than a transcription of it:
  every assertion below is a CID whose meaning can be confirmed from the
  published collection, so a parser that drifted would fail here rather than
  produce plausible Japanese."
  (:require [clojure.test :refer [deftest is testing]]
            [adobe.cmap.core :as cmap]))

;; ── the parser ───────────────────────────────────────────────────────────────

(deftest bfchar-maps-one-cid-each
  (is (= {1 "A" 2 "B"}
         (cmap/parse "begincmap\n2 beginbfchar\n<0001> <0041>\n<0002> <0042>\nendbfchar\nendcmap"))))

(deftest a-range-increments-its-destination
  ;; Reading it as one string repeated maps a whole range to a single
  ;; character, which looks like a font whose glyphs are all the same.
  (is (= {1 "A" 2 "B" 3 "C"}
         (cmap/parse "beginbfrange\n<0001> <0003> <0041>\nendbfrange"))))

(deftest a-range-with-an-array-maps-elementwise
  (is (= {1 "A" 2 "Z"}
         (cmap/parse "beginbfrange\n<0001> <0002> [<0041> <005A>]\nendbfrange"))))

(deftest a-surrogate-pair-survives-both-forms
  ;; Four hex digits is one UTF-16 code unit, so a pair is eight. Decoding
  ;; each group as a code point turns every character outside the BMP into
  ;; two replacement characters.
  (let [one (cmap/parse "beginbfchar\n<0001> <D842DF9F>\nendbfchar")]
    (is (= "𠮟" (get one 1))))

  (testing "and a range increments the LOW surrogate, not the whole number"
    (let [r (cmap/parse "beginbfrange\n<0001> <0002> <D842DF9F>\nendbfrange")]
      (is (= "𠮟" (get r 1)))
      (is (= (str (char 0xD842) (char 0xDFA0)) (get r 2))
          "the high surrogate is unchanged"))))

;; ── the published tables ─────────────────────────────────────────────────────

(deftest every-ordering-loads-and-is-large
  (doseq [[ordering _] cmap/orderings]
    (testing ordering
      (let [m (cmap/cid->unicode ordering)]
        (is (map? m))
        (is (> (count m) 8000) (str ordering " has " (count m) " entries"))))))

(deftest japan1-says-what-adobe-says
  ;; Spot checks from the published collection. CID 1 is the space in every
  ;; Adobe collection, and the Latin block that follows it is ASCII order.
  (let [m (cmap/cid->unicode "Adobe-Japan1")]
    (is (= " " (get m 1)))
    (is (= "A" (get m 34)) "CID 34 is capital A in Adobe-Japan1")
    (is (= "あ" (get m 843)) "the first hiragana")
    (is (= "亜" (get m 1125)) "the first kanji of the JIS level-1 set")))

(deftest an-unknown-ordering-is-nil-and-not-a-throw
  ;; A font naming a collection nobody published is a normal thing to meet,
  ;; and the caller's answer is the same as for a font with no table at all.
  (is (nil? (cmap/cid->unicode "Adobe-Identity")))
  (is (nil? (cmap/cid->unicode "Made-Up-1")))
  (is (nil? (cmap/cid->unicode nil)))
  (is (false? (cmap/known? "Adobe-Identity")))
  (is (true? (cmap/known? "Adobe-Japan1"))))

(deftest the-table-is-parsed-once
  ;; A document that uses a collection uses it on every page, and each parse
  ;; is a few hundred thousand characters of regex.
  (cmap/cid->unicode "Adobe-KR")
  (let [t0 (System/nanoTime)
        _ (dotimes [_ 50] (cmap/cid->unicode "Adobe-KR"))
        elapsed-ms (/ (- (System/nanoTime) t0) 1e6)]
    (is (< elapsed-ms 50)
        (str "50 lookups took " elapsed-ms "ms, which is a re-parse"))))
