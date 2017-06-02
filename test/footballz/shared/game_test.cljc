(ns footballz.shared.game-test
  (:require [footballz.shared.game :as g])
  (:use clojure.test))

(deftest test-leave
  (is (= (g/leave {:entities {1 {:id 1} 2 {:id 2}}} 2) {:entities {1 {:id 1}}})))

(deftest test-distinct-by
  (is (=
        (g/distinct-by :id [{:id 1 :a 1} {:id 1 :a 2} {:id 2}])
        [{:id 1 :a 1} {:id 2}])))

(deftest test-collision-groups-empty-list
  (is (= (g/collision-groups []) [])))

(deftest test-collision-groups-one-element
  (is (= (g/collision-groups [{:id 1}]) [])))

(deftest test-collision-groups-two-noncoliding
  (is (= (g/collision-groups [{:id       1
                               :position [10 10]
                               :radius   5}
                              {:id       2
                               :position [20 20]
                               :radius   5}])
         [])))

(deftest test-collision-groups-two-coliding
  (is (= (g/collision-groups [{:id       1
                               :position [10 10]
                               :old-position [9 9]
                               :velocity [1 1]
                               :radius   10}
                              {:id       2
                               :position [13 14]
                               :old-position [14 15]
                               :velocity [-1 -1]
                               :radius   7}])
         [[{:id       1
            :position [9 9]
            :old-position [9 9]
            :velocity [-0.8 -0.8]
            :radius   10}
           {:id       2
            :position [14 15]
            :old-position [14 15]
            :velocity [0.8 0.8]
            :radius   7}]])))