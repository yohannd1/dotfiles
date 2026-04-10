# Test suite for the public interface in all libraries here.

(use dotf-acr)
(use dotf-path)
(use dotf-utils)

(defn- test-utils []
  # Test stable-sort with plain numbers
  (def arr @[9 8 3 2 1 4])
  (stable-sort arr)
  (assert (deep= arr @[1 2 3 4 8 9]))

  # Test stable-sort with pairs, ordering by the first entry. The ordering
  # should be stable (elements of the same value should keep their relative
  # order).
  (def arr @[[8 "foo"] [2 "abc"] [8 "bar"] [2 "baz"]])
  (stable-sort arr (fn [[a _] [b _]] (compare< a b)))
  (assert (deep= arr @[[2 "abc"] [2 "baz"] [8 "foo"] [8 "bar"]]))

  # Test remove-prefix and remove-suffix
  (assert (= (remove-suffix "@%foo" "oo") "@%f"))
  (assert (= (remove-prefix "foo@%" "fo") "o@%")))

(defn test-all []
  # TODO: add tests for all of 'em!
  (test-utils))
