(ns audience-test.core)

;; Graph traversal

;; This link has a simple explanation of a simple Graph
;; definition "language" and implementations of depth/breadth graph
;; search algorithms.

;; http://hueypetersen.com/posts/2013/06/25/graph-traversal-with-clojure/

;; The code is reproduced here for readability:

(def G-unweighted {:1 [:2 :3],
                   :2 [:4],
                   :3 [:4],
                   :4 []})

(defn traverse-graph-dfs [g s]
  (loop [vertices [] explored #{s} frontier [s]]
    (if (empty? frontier)
      vertices
      (let [v (peek frontier)
            neighbors (g v)]
        (recur
          (conj vertices v)
          (into explored neighbors)
          (into (pop frontier) (remove explored neighbors)))))))

(comment
  (defn seq-graph-dfs [g s]
    ((fn rec-dfs [explored frontier]
       (lazy-seq
        (if (empty? frontier)
          nil
          (let [v (peek frontier)
                neighbors (g v)]
            (cons v (rec-dfs
                     (into explored neighbors)
                     (into (pop frontier) (remove explored neighbors))))))))
     #{s} [s])))

(comment
  (defn seq-graph-bfs [g s]
    ((fn rec-bfs [explored frontier]
       (lazy-seq
        (if (empty? frontier)
          nil
          (let [v (peek frontier)
                neighbors (g v)]
            (cons v (rec-bfs
                     (into explored neighbors)
                     (into (pop frontier) (remove explored neighbors))))))))
     #{s} (conj (clojure.lang.PersistentQueue/EMPTY) s))))

(comment
  (traverse-graph-dfs G-unweighted :1) [:1 :3 :4 :2]
  (seq-graph-dfs G-unweighted :1) '(:1 :3 :4 :2)
  (seq-graph-bfs G-unweighted :1) '(:1 :2 :3 :4))

;; The author then simplifies it by recognising that only the initial
;; data structure for holding the nodes traversed is different between
;; the depth and breadth first implementations.

;; He then abstacts that out and the result is:

(defn seq-graph [d g s]
  ((fn rec-seq [explored frontier]
     (lazy-seq
       (if (empty? frontier)
         nil
         (let [v (peek frontier)
               neighbors (g v)]
           (cons v (rec-seq
                     (into explored neighbors)
                     (into (pop frontier) (remove explored neighbors))))))))
   #{s} (conj d s)))

#_{:clj-kondo/ignore [:redefined-var]}
(def seq-graph-dfs (partial seq-graph []))
#_{:clj-kondo/ignore [:redefined-var]}
(def seq-graph-bfs (partial seq-graph (clojure.lang.PersistentQueue/EMPTY)))

;; Questions

;; 1. Extend the graph definition to include a weight between graph edges

;; For example:

(def G {:1 [(:2 1) (:3 2)],
        :2 [(:4 4)],
        :3 [(:4 2)],
        :4 [] })

;; Answer: I'm fine with the proposed format


;; 2. Write an algorithm to randomly generate a simple directed graph
;; using your answer from #1

;; Input:
;;     N - size of generated graph
;;     S - sparseness (number of directed edges actually; from N-1 (inclusive) to N(N-1) (inclusive))
;; Output:
;;     simple connected graph G(n,s) with N vertices and S edges

;; Please ensure that your graph is connected, otherwise you won't be
;; able to complete the following questions.

;; Answer:

;; well, the first thing which needs to be implemented is a way of
;; generating minimally connected graphs. As we are talking about
;; digraphs, there are two notions of connectivity: a digraph can be
;; weakly connected or strongly connected. A weakly connected digraph
;; has its underlying graph connected, whereas a strongly connected
;; digraph has a path from u to v, for any of its vertices u and v.

;; for the sake of simplicity, I am assuming the notion of weakly
;; connected digraphs

;; now we need to devise a way to generate random orientations of
;; spanning trees. For that purpose, it would be nice to have disjoint
;; set data strucutres and their operations, which I am going to
;; implement next.

;; I propose to represent a disjoint set (union-find) data structure
;; like this

(def DS {:1 nil
         :2 nil
         :3 nil})

;; In such a data structure, the map keys represent the elements of
;; interest, each valued by its parent. It is said that an
;; element with no parent is a representant. An element r
;; which happens to be a representant is representing the set of
;; elements which are descendants of r. This way, since parent
;; relations describe a forest, each representant represents a set of
;; elements which is disjoint from any other representant's. 

;; for simplicity matters, I am not implementing the ranked version of
;; disjoint sets data strucutre

(defn create-ds [elements]
  (zipmap elements (repeat nil)))

(defn representant-of [ds element]
  (letfn [(find [element]
            (let [parent (ds element)]
              (if parent
                (recur parent)
                element)))]
    (find element)))

;; a ranked version of this would make sure to avoid building the
;; longest chain of descendants
(defn union [ds element-a element-b]
  (let [representant-a (representant-of ds element-a)
        representant-b (representant-of ds element-b)]
    (assoc ds representant-a representant-b)))

;; now we are good to go to generate a minimal (weakly) connected digraph

(defn subsets [coll n]
  (cond
    (= 0 n) '(())
    (empty? coll) '()
    :else
    (let [item (first coll)
          hyp-coll (rest coll)
          hyp1-subsets (subsets hyp-coll (dec n))
          hyp2-subsets (subsets hyp-coll n)
          subsets-containing-item (map #(conj % item) hyp1-subsets)
          subsets-not-containing-item hyp2-subsets]
      (concat subsets-containing-item
              subsets-not-containing-item))))

(def global-maximum-edge-weight 100)

(defn random-edge-weight []
  (rand-int (inc global-maximum-edge-weight)))

(defn empty-graph [num-verts]
  (zipmap (range 1 (inc num-verts))
          (repeat [])))

(defn random-edges-for-minimal-weakly-connected-digraph [num-verts]
  (let [vertices (range 1 (inc num-verts))
        num-edges-to-collect (dec num-verts)
        all-edges (subsets vertices 2)
        shuffled-edges (shuffle all-edges)]
    (letfn [(selection-step [disjoint-sets candidate-edges collected-edges]
              (let [edge (first candidate-edges)
                    u (nth edge 0)
                    v (nth edge 1)]
                (cond
                  (= (count collected-edges) num-edges-to-collect)
                  collected-edges

                  (= (representant-of disjoint-sets u)
                     (representant-of disjoint-sets v))
                  (recur disjoint-sets
                         (rest candidate-edges)
                         collected-edges)

                  :else
                  (recur (union disjoint-sets u v)
                         (rest candidate-edges)
                         (conj collected-edges edge)))))]
      (selection-step (create-ds vertices) shuffled-edges '()))))

(defn has-edge [digraph u v]
  (let [u-neighbors (into #{} (map first (digraph u)))]
    (u-neighbors v)))

(defn add-edge [digraph u v w]
  (if (has-edge digraph u v)
    digraph
    (let [u-neighbors (digraph u)
          new-neighbors (conj u-neighbors [v w])]
      (assoc digraph u new-neighbors))))

(defn num-edges [digraph]
  (apply + (map count (vals digraph))))

(defn random-minimal-weakly-connected-digraph [num-verts]
  (let [empty-digraph (empty-graph num-verts)
        selected-edges (random-edges-for-minimal-weakly-connected-digraph num-verts)]
    (reduce (fn [digraph [u v]]
              (add-edge digraph u v (random-edge-weight)))
            empty-digraph selected-edges)))

;; now that we are able to generate minimal (weakly) connected
;; digraphs, we need to meet the sparseness parameter. For that, we
;; part from a minimal connected digraph and randomly add edges until
;; we reach the expected number of edges

(defn random-weakly-connected-digraph [num-verts sparseness]
  (let [min-sparseness (dec num-verts)
        max-sparseness (* num-verts (dec num-verts))]
    (when-not (<= min-sparseness sparseness max-sparseness)
      (throw (ex-info "invalid sparseness parameter" {:num-verts num-verts
                                                      :sparseness sparseness
                                                      :min min-sparseness
                                                      :max max-sparseness})))
    (let [vertices (range 1 (inc num-verts))
          all-edges (subsets vertices 2)
          shuffled-edges (shuffle all-edges)
          minimal-connected-digraph (random-minimal-weakly-connected-digraph num-verts)
          num-edges-to-add (- sparseness min-sparseness)]
      (letfn [(step [current-digraph candidate-edges num-edges-to-add]
                (if (zero? num-edges-to-add)
                  current-digraph
                  (let [edge (first candidate-edges)
                        u (nth edge 0)
                        v (nth edge 1)]
                    (if (has-edge current-digraph u v)
                      (recur current-digraph (rest candidate-edges) num-edges-to-add)
                      (recur (add-edge current-digraph u v (random-edge-weight))
                             (rest candidate-edges)
                             (dec num-edges-to-add))))))]
        (step minimal-connected-digraph shuffled-edges num-edges-to-add)))))

;; 3. Write an implementation of Dijkstra's algorithm that traverses
;; your graph and outputs the shortest path between any 2 randomly
;; selected vertices.

;; I should be able to write something like this for example.

(comment
  (def random-graph (random-weakly-connected-digraph 10 10))
  (shortest-path random-graph (first (keys random-graph)) (last (keys random-graph))) ; => list of nodes which is the shortest path by edge weight between the 2 nodes, or no path if one does not exist.
)

;; Answer

(defn single-source-shortest-paths-initial-state [vertices initial-vertex]
  (-> vertices
      (zipmap (repeat {:distance Integer/MAX_VALUE
                       :predecessor nil}))
      (assoc-in [initial-vertex :distance] 0)))

(defn relax [state u v w]
  (let [u-distance (get-in state [u :distance])
        v-distance (get-in state [v :distance])]
    (if (> v-distance (+ u-distance w))
      (-> state
          (assoc-in [v :distance] (+ u-distance w))
          (assoc-in [v :predecessor] u))
      state)))

;; the following dijkstra implementation is a bit more general than
;; the requested one, as dijkstra is usually applied to find all
;; shortest paths from a single source.

(defn dijkstra [digraph initial-vertex]
  (let [vertices (keys digraph)
        initial-state (single-source-shortest-paths-initial-state vertices initial-vertex)
        initial-unvisited-vertices (into #{} vertices)]
    (letfn [(next-vertex-to-visit [state unvisited-vertices]
              (apply min-key #(get-in state [% :distance]) unvisited-vertices))
            (step [state unvisited-vertices]
              (if (empty? unvisited-vertices)
                state
                (let [vertex (next-vertex-to-visit state unvisited-vertices)
                      next-unvisited-vertices (disj unvisited-vertices vertex)
                      out-neighbors (digraph vertex)
                      non-visited-out-neighbors (filter #(-> % first unvisited-vertices) out-neighbors)
                      next-state (reduce (fn [state [out-neighbor weight]]
                                           (relax state vertex out-neighbor weight))
                                         state non-visited-out-neighbors)]
                  (recur next-state next-unvisited-vertices))))]
      (step initial-state initial-unvisited-vertices))))

;; using the final state produced by Dijkstra's algorithm, it is
;; possible to recover the shortest path starting at some vertex s and
;; ending at some vertex t

(defn shortest-path [digraph s t]
  (let [single-source-shortest-paths-final-state (dijkstra digraph s)]
    (letfn [(step-backwards [current-vertex current-path]
              (if (= current-vertex s)
                (conj current-path s)
                (recur (get-in single-source-shortest-paths-final-state
                               [current-vertex :predecessor])
                       (conj current-path current-vertex))))]
      (step-backwards t '()))))

;; 4. Write a suite of functions to calculate distance properties for
;; your graph.

;; Now that you have implemented Dijkstra's algorithm you should be
;; able to calculate the eccentricity of any vertex in your graph, and
;; in turn the radius and diameter of your graph.

;; Please re-acquaint yourself with graph distance properties
;; https://en.wikipedia.org/wiki/Distance_(graph_theory),

;; The eccentricity of a vertex v is defined as the greatest distance
;; between v and any other vertex.  The radius of a graph is the
;; minimum eccentricity of any vertex in a graph.  The diameter of a
;; graph is the maximum eccentricity of any vertex in a graph.  I
;; should be able to write something like this:

(comment
  (def random-graph (random-weakly-connected-digraph 10 10))
  (eccentricity random-graph (first (keys random-graph))) ; => number expressing eccentricity for `first` vertex in random-graph
  (radius random-graph) ; => minimal eccentricity
  (diameter random-graph) ; => maximal eccentricity
)

(defn eccentricity [digraph vertex]
  (let [single-source-shortest-paths-final-state (dijkstra digraph vertex)]
    (->> single-source-shortest-paths-final-state
         vals
         (map :distance)
         (apply max))))

(defn radius [digraph]
  (->> digraph
       keys
       (map #(eccentricity digraph %))
       (apply min)))

(defn diameter [digraph]
  (->> digraph
       keys
       (map #(eccentricity digraph %))
       (apply max)))
