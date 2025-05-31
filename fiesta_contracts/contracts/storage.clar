;; Enhanced Simple Storage Contract
;; A comprehensive contract with multiple storage features and access controls

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-invalid-value (err u102))
(define-constant err-value-unchanged (err u103))

;; Define data variables
(define-data-var stored-value int 0)
(define-data-var previous-value int 0)
(define-data-var update-count uint u0)
(define-data-var is-locked bool false)
(define-data-var min-value int -1000000)
(define-data-var max-value int 1000000)

;; Define maps
(define-map authorized-users principal bool)
(define-map value-history uint {value: int, updater: principal, block-height: uint})

;; Initialize contract owner as authorized user
(map-set authorized-users contract-owner true)

;; Public function to set the stored value with enhanced features
(define-public (set-value (new-value int))
  (let ((current-value (var-get stored-value))
        (current-count (var-get update-count)))
    (asserts! (not (var-get is-locked)) err-not-authorized)
    (asserts! (is-authorized-user tx-sender) err-not-authorized)
    (asserts! (and (>= new-value (var-get min-value)) (<= new-value (var-get max-value))) err-invalid-value)
    (asserts! (not (is-eq new-value current-value)) err-value-unchanged)
    
    ;; Store previous value and update history
    (var-set previous-value current-value)
    (map-set value-history current-count {
      value: new-value,
      updater: tx-sender,
      block-height: block-height
    })
    
    ;; Update the stored value and increment counter
    (var-set stored-value new-value)
    (var-set update-count (+ current-count u1))
    
    (ok true)))

;; Public function to increment the stored value by a specified amount
(define-public (increment-value (amount int))
  (let ((current-value (var-get stored-value)))
    (set-value (+ current-value amount))))

;; Public function to decrement the stored value by a specified amount
(define-public (decrement-value (amount int))
  (let ((current-value (var-get stored-value)))
    (set-value (- current-value amount))))

;; Public function to reset value to zero (owner only)
(define-public (reset-value)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set previous-value (var-get stored-value))
    (var-set stored-value 0)
    (ok true)))

;; Public function to add authorized user (owner only)
(define-public (add-authorized-user (user principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-users user true)
    (ok true)))

;; Public function to remove authorized user (owner only)
(define-public (remove-authorized-user (user principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (is-eq user contract-owner)) err-not-authorized)
    (map-delete authorized-users user)
    (ok true)))

;; Public function to lock/unlock the contract (owner only)
(define-public (set-lock-status (locked bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set is-locked locked)
    (ok true)))

;; Public function to set value limits (owner only)
(define-public (set-value-limits (min-val int) (max-val int))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (< min-val max-val) err-invalid-value)
    (var-set min-value min-val)
    (var-set max-value max-val)
    (ok true)))

;; Read-only function to get the current stored value
(define-read-only (get-value)
  (var-get stored-value))

;; Read-only function to get the previous value
(define-read-only (get-previous-value)
  (var-get previous-value))

;; Read-only function to get update statistics
(define-read-only (get-update-stats)
  {
    current-value: (var-get stored-value),
    previous-value: (var-get previous-value),
    update-count: (var-get update-count),
    is-locked: (var-get is-locked)
  })

;; Read-only function to get value limits
(define-read-only (get-value-limits)
  {
    min-value: (var-get min-value),
    max-value: (var-get max-value)
  })

;; Read-only function to check if user is authorized
(define-read-only (is-authorized-user (user principal))
  (default-to false (map-get? authorized-users user)))

;; Read-only function to get value history by index
(define-read-only (get-value-history (index uint))
  (map-get? value-history index))

;; Read-only function to get comprehensive contract info
(define-read-only (get-contract-info)
  {
    stored-value: (var-get stored-value),
    previous-value: (var-get previous-value),
    update-count: (var-get update-count),
    is-locked: (var-get is-locked),
    min-value: (var-get min-value),
    max-value: (var-get max-value),
    contract-owner: contract-owner,
    contract-name: "enhanced-simple-storage"
  })

;; Read-only function to get the absolute difference between current and previous values
(define-read-only (get-value-change)
  (let ((current (var-get stored-value))
        (previous (var-get previous-value)))
    (if (>= current previous)
        (- current previous)
        (- previous current))))

;; Read-only function to check if value is at minimum or maximum limit
(define-read-only (get-limit-status)
  (let ((current-value (var-get stored-value)))
    {
      at-min-limit: (is-eq current-value (var-get min-value)),
      at-max-limit: (is-eq current-value (var-get max-value)),
      can-increment: (< current-value (var-get max-value)),
      can-decrement: (> current-value (var-get min-value))
    }))