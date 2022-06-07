(ns audience-test.core-test
  (:require [clojure.test :as t]
            [audience-test.core :as aud]))

(t/deftest create-ds-test
  (t/testing "create-ds returns a disjoint set structure in which each element is represented by itself"
    (t/is (= {1 nil 2 nil 3 nil}
             (aud/create-ds [1 2 3])))))

(def ds {1 nil
         2 1
         3 nil
         4 3
         5 4})

(t/deftest representant-of-test
  (t/testing "representant-of correctly returns the representant of each element in a ds data structure"
    (t/is (= 1 (aud/representant-of ds 1)))
    (t/is (= 1 (aud/representant-of ds 2)))
    (t/is (= 3 (aud/representant-of ds 3)))
    (t/is (= 3 (aud/representant-of ds 4)))
    (t/is (= 3 (aud/representant-of ds 5)))))

(t/deftest union-test
  (t/testing "union correctly returns a version of ds in which two disjoint sets are joined into one"
    (let [joined-ds (aud/union ds 2 5)]
      (t/is (= 3 (aud/representant-of joined-ds 1)))
      (t/is (= 3 (aud/representant-of joined-ds 2)))
      (t/is (= 3 (aud/representant-of joined-ds 3)))
      (t/is (= 3 (aud/representant-of joined-ds 4)))
      (t/is (= 3 (aud/representant-of joined-ds 5))))))

(t/deftest subsets-test
  (t/testing "subsets correctly returns all the subsets of a collection with a certain number of elements"
    (let [collection (range 1 (inc 5))]
      (t/is (= '(()) (aud/subsets collection 0)))
      (t/is (= '((1) (2) (3) (4) (5))
               (aud/subsets collection 1)))
      (t/is (= '((1 2) (1 3) (1 4) (1 5) (2 3) (2 4) (2 5) (3 4) (3 5) (4 5))
               (aud/subsets collection 2)))
      (t/is (= '((1 2 3)          
                 (1 2 4)
                 (1 2 5)
                 (1 3 4)
                 (1 3 5)
                 (1 4 5)
                 (2 3 4)
                 (2 3 5)
                 (2 4 5)
                 (3 4 5))
               (aud/subsets collection 3)))
      (t/is (= '((1 2 3 4) (1 2 3 5) (1 2 4 5) (1 3 4 5) (2 3 4 5))
               (aud/subsets collection 4)))
      (t/is (= '((1 2 3 4 5))
               (aud/subsets collection 5))))))

(t/deftest empty-graph-test
  (t/testing "empty-graph returns a map structure representing a graph with vertices from 1 to n and no edges"
    (t/is (= {1 [] 2 [] 3 [] 4 [] 5 []}
             (aud/empty-graph 5)))))

(t/deftest random-edges-for-minimal-weakly-connected-digraph-test
  (t/testing "random-edges-for-minimal-weakly-connected-digraph returns the correct amount of edges for a minimal weakly connect digraph"
    (t/is (= 10
             (count (aud/random-edges-for-minimal-weakly-connected-digraph 11))))))

(def weighted-digraph
  {5 '([10 70])
   3 '([5 7])
   7 '([10 14])
   4 '([10 90])
   2 '([9 23])
   9 '([10 0])
   1 '([6 19] [8 25] [2 57])})

(t/deftest has-edge-test
  (t/testing "has-edge correctly returns a non-nil value whenever digraph has an edge starting at u and ending at v"
    (t/is (aud/has-edge weighted-digraph 5 10))
    (t/is (not (aud/has-edge weighted-digraph 5 9)))
    (t/is (aud/has-edge weighted-digraph 3 5))
    (t/is (not (aud/has-edge weighted-digraph 3 6)))
    (t/is (aud/has-edge weighted-digraph 7 10))
    (t/is (not (aud/has-edge weighted-digraph 7 9)))
    (t/is (aud/has-edge weighted-digraph 4 10))
    (t/is (not (aud/has-edge weighted-digraph 4 9)))
    (t/is (aud/has-edge weighted-digraph 2 9))
    (t/is (not (aud/has-edge weighted-digraph 2 8)))
    (t/is (aud/has-edge weighted-digraph 9 10))
    (t/is (not (aud/has-edge weighted-digraph 9 8)))
    (t/is (aud/has-edge weighted-digraph 1 6))
    (t/is (aud/has-edge weighted-digraph 1 8))
    (t/is (aud/has-edge weighted-digraph 1 2))
    (t/is (not (aud/has-edge weighted-digraph 1 5)))))

(t/deftest add-edge-test
  (t/testing "add-edge returns either the same digraph in case it already contains the to-be-added edge or a new digraph based on the old one but containing the to-be-added edge"
    (t/is (= weighted-digraph
             (aud/add-edge weighted-digraph 5 10 2)))
    (t/is (= (update weighted-digraph 5 #(conj % [9 2]))
             (aud/add-edge weighted-digraph 5 9 2)))))

(t/deftest num-edges-test
  (t/testing "num-edges returns the correct number of edges contained in a certain digraph"
    (t/is (= 9
             (aud/num-edges weighted-digraph)))))

(t/deftest random-weakly-connected-digraph-test
  (t/testing "random-weakly-connected-digraph throws an exception for inapproriate sparseness values"
    (t/is (thrown? clojure.lang.ExceptionInfo (aud/random-weakly-connected-digraph 4 2)))
    (t/is (thrown? clojure.lang.ExceptionInfo (aud/random-weakly-connected-digraph 4 13)))
    (t/is (thrown? clojure.lang.ExceptionInfo (aud/random-weakly-connected-digraph 5 3)))
    (t/is (thrown? clojure.lang.ExceptionInfo (aud/random-weakly-connected-digraph 5 21))))
  (t/testing "random-weakly-connected-digraph returns an weighted digraph for approriate sparseness values"
    (t/is (map? (aud/random-weakly-connected-digraph 4 3)))
    (t/is (map? (aud/random-weakly-connected-digraph 4 12)))
    (t/is (map? (aud/random-weakly-connected-digraph 5 4)))
    (t/is (map? (aud/random-weakly-connected-digraph 5 20)))))

(t/deftest single-source-shortest-paths-initial-state-test
  (t/testing "single-source-shortest-paths-initial-state returns the expected map structure to represent the initial state of a single-source shortest paths algorithm"
    (t/is (= {1 {:distance 0 :predecessor nil}          
              2 {:distance Integer/MAX_VALUE :predecessor nil}
              3 {:distance Integer/MAX_VALUE :predecessor nil}}
             (aud/single-source-shortest-paths-initial-state [1 2 3] 1)))
    (t/is (= {1 {:distance Integer/MAX_VALUE :predecessor nil}          
              2 {:distance 0 :predecessor nil}
              3 {:distance Integer/MAX_VALUE :predecessor nil}}
             (aud/single-source-shortest-paths-initial-state [1 2 3] 2)))
    (t/is (= {1 {:distance Integer/MAX_VALUE :predecessor nil}          
              2 {:distance Integer/MAX_VALUE :predecessor nil}
              3 {:distance 0 :predecessor nil}}
             (aud/single-source-shortest-paths-initial-state [1 2 3] 3)))))

(t/deftest relax-test
  (let [state {1 {:distance 0 :predecessor nil}
               2 {:distance 2 :predecessor 1}
               3 {:distance 5 :predecessor 2}
               4 {:distance 3 :predecessor 2}}]
    (t/testing "relax correctly updates the algorithm state when a better path is found"
      (t/is (= {1 {:distance 0 :predecessor nil}
                2 {:distance 2 :predecessor 1}
                3 {:distance 4 :predecessor 1}
                4 {:distance 3 :predecessor 2}}
               (aud/relax state 1 3 4)))
      (t/is (= {1 {:distance 0 :predecessor nil}
                2 {:distance 2 :predecessor 1}
                3 {:distance 4 :predecessor 1}
                4 {:distance 3 :predecessor 2}}
               (aud/relax (assoc-in state [3 :distance] Integer/MAX_VALUE) 1 3 4))))
    (t/testing "relax does not update the algorithm state when a worse path is found"
      (t/is (= state (aud/relax state 1 3 6))))))

;; Introduction to Algorithms, 3rd edition, page 659
(def cormen-example-digraph
  {1 [[2 10] [4 5]]
   2 [[3 1] [4 2]]
   3 [[5 4]]
   4 [[2 3] [3 9] [5 2]]
   5 [[3 6] [1 7]]})

(t/deftest dijkstra-test
  (t/testing "dijkstra returns the correct single source paths final state"
    (t/is (= {1 {:distance 0 :predecessor nil}
              2 {:distance 8 :predecessor 4}
              3 {:distance 9 :predecessor 2}
              4 {:distance 5 :predecessor 1}
              5 {:distance 7 :predecessor 4}}
             (aud/dijkstra cormen-example-digraph 1)))))

(t/deftest shortest-path-test
  (t/testing "shortest-path returns the shortest path from s to t in a digraph whose weights are all nonnegative"
    (t/is (= '(1)
             (aud/shortest-path cormen-example-digraph 1 1)))
    (t/is (= '(1 4 2)
             (aud/shortest-path cormen-example-digraph 1 2)))
    (t/is (= '(1 4 2 3)
             (aud/shortest-path cormen-example-digraph 1 3)))
    (t/is (= '(1 4)
             (aud/shortest-path cormen-example-digraph 1 4)))
    (t/is (= '(1 4 5)
             (aud/shortest-path cormen-example-digraph 1 5)))))

(t/deftest eccentricity-test
  (t/testing "eccentricity returns the length of the longest path starting at a certain vertex"
    (t/is (= 9
             (aud/eccentricity cormen-example-digraph 1)))
    (t/is (= 11
             (aud/eccentricity cormen-example-digraph 2)))
    (t/is (= 19
             (aud/eccentricity cormen-example-digraph 3)))
    (t/is (= 9
             (aud/eccentricity cormen-example-digraph 4)))
    (t/is (= 15
             (aud/eccentricity cormen-example-digraph 5)))))

(t/deftest radius-test
  (t/testing "radius should return the minimum eccentricity of any vertex in a certain digraph"
    (t/is (= 9
             (aud/radius cormen-example-digraph)))))

(t/deftest diameter-test
  (t/testing "diameter should return the maximum eccentricity of any vertex in a certain digraph"
    (t/is (= 19
             (aud/diameter cormen-example-digraph)))))
