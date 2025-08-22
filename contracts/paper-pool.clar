
;; Manages office supply sharing with usage tracking and automatic reordering

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-SUPPLY (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-USER-NOT-FOUND (err u103))

;; Constants
(define-constant MIN-STOCK-THRESHOLD u50)
(define-constant REORDER-QUANTITY u500)
(define-constant CONTRACT-OWNER tx-sender)

;; Data maps
(define-map pool-inventory
  principal
  {
    current-stock: uint,
    total-consumed: uint,
    last-reorder-block: uint,
    reorder-pending: bool
  }
)

(define-map user-usage
  {pool: principal, user: principal}
  {
    total-used: uint,
    last-usage-block: uint
  }
)

(define-map pool-members
  {pool: principal, user: principal}
  bool
)

;; Read-only functions
(define-read-only (get-pool-status (pool principal))
  (default-to
    {current-stock: u0, total-consumed: u0, last-reorder-block: u0, reorder-pending: false}
    (map-get? pool-inventory pool)
  )
)

(define-read-only (get-user-usage (pool principal) (user principal))
  (default-to
    {total-used: u0, last-usage-block: u0}
    (map-get? user-usage {pool: pool, user: user})
  )
)

(define-read-only (is-pool-member (pool principal) (user principal))
  (default-to false (map-get? pool-members {pool: pool, user: user}))
)

(define-read-only (needs-reorder (pool principal))
  (let ((inventory (get-pool-status pool)))
    (< (get current-stock inventory) MIN-STOCK-THRESHOLD)
  )
)

;; Public functions
(define-public (initialize-pool (initial-stock uint))
  (let ((pool tx-sender))
    (asserts! (> initial-stock u0) ERR-INVALID-AMOUNT)
    (map-set pool-inventory pool {
      current-stock: initial-stock,
      total-consumed: u0,
      last-reorder-block: stacks-block-height,
      reorder-pending: false
    })
    (map-set pool-members {pool: pool, user: pool} true)
    (ok true)
  )
)

(define-public (add-member (pool principal) (user principal))
  (begin
    (asserts! (or (is-eq tx-sender pool) (is-eq tx-sender CONTRACT-OWNER)) ERR-NOT-AUTHORIZED)
    (map-set pool-members {pool: pool, user: user} true)
    (ok true)
  )
)

(define-public (consume-supply (pool principal) (amount uint))
  (let (
    (inventory (get-pool-status pool))
    (current-usage (get-user-usage pool tx-sender))
  )
    (asserts! (is-pool-member pool tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= (get current-stock inventory) amount) ERR-INSUFFICIENT-SUPPLY)

    ;; Update inventory
    (map-set pool-inventory pool {
      current-stock: (- (get current-stock inventory) amount),
      total-consumed: (+ (get total-consumed inventory) amount),
      last-reorder-block: (get last-reorder-block inventory),
      reorder-pending: (get reorder-pending inventory)
    })

    ;; Update user usage
    (map-set user-usage {pool: pool, user: tx-sender} {
      total-used: (+ (get total-used current-usage) amount),
      last-usage-block: stacks-block-height
    })

    ;; Check if reorder needed
    (if (needs-reorder pool)
      (begin
        (unwrap-panic (trigger-reorder pool))
        (ok true))
      (ok true)
    )
  )
)

(define-public (restock-supply (pool principal) (amount uint))
  (let ((inventory (get-pool-status pool)))
    (asserts! (or (is-eq tx-sender pool) (is-eq tx-sender CONTRACT-OWNER)) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)

    (map-set pool-inventory pool {
      current-stock: (+ (get current-stock inventory) amount),
      total-consumed: (get total-consumed inventory),
      last-reorder-block: stacks-block-height,
      reorder-pending: false
    })
    (ok true)
  )
)

(define-private (trigger-reorder (pool principal))
  (let ((inventory (get-pool-status pool)))
    (map-set pool-inventory pool {
      current-stock: (get current-stock inventory),
      total-consumed: (get total-consumed inventory),
      last-reorder-block: (get last-reorder-block inventory),
      reorder-pending: true
    })
    (ok true)
  )
)
