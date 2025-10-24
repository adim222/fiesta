;; Multi-Send with Off-Chain Signatures Contract
;; Enables gasless multi-send transactions through off-chain signature verification

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-SIGNATURE (err u101))
(define-constant ERR-NONCE-ALREADY-USED (err u102))
(define-constant ERR-EXPIRED-REQUEST (err u103))
(define-constant ERR-INVALID-RECIPIENT (err u104))
(define-constant ERR-INSUFFICIENT-BALANCE (err u105))
(define-constant ERR-TRANSFER-FAILED (err u106))
(define-constant ERR-INVALID-RELAYER (err u107))

;; Data Variables
(define-data-var relayer-fee uint u1000) ;; Fee in microSTX (0.001 STX)

;; Data Maps
(define-map user-nonces principal uint)
(define-map authorized-relayers principal bool)

;; Multi-send request structure
(define-map pending-requests 
  { user: principal, nonce: uint }
  {
    recipients: (list 50 principal),
    amounts: (list 50 uint),
    expiry: uint,
    relayer-fee: uint,
    signature: (buff 65)
  }
)

;; Events
(define-private (emit-multi-send-event (user principal) (nonce uint) (recipients (list 50 principal)) (amounts (list 50 uint)))
  (print {
    event: "multi-send-executed",
    user: user,
    nonce: nonce,
    recipients: recipients,
    amounts: amounts,
    block-height: block-height
  })
)

;; Private Functions

;; Generate message hash for signature verification
(define-private (generate-message-hash 
  (user principal) 
  (nonce uint) 
  (recipients (list 50 principal)) 
  (amounts (list 50 uint)) 
  (expiry uint)
  (fee-amount uint))
  (sha256 (concat
    (concat
      (concat
        (unwrap-panic (to-consensus-buff? user))
        (unwrap-panic (to-consensus-buff? nonce))
      )
      (concat
        (unwrap-panic (to-consensus-buff? recipients))
        (unwrap-panic (to-consensus-buff? amounts))
      )
    )
    (concat
      (unwrap-panic (to-consensus-buff? expiry))
      (unwrap-panic (to-consensus-buff? fee-amount))
    )
  ))
)

;; Verify off-chain signature
(define-private (verify-signature 
  (user principal) 
  (nonce uint) 
  (recipients (list 50 principal)) 
  (amounts (list 50 uint)) 
  (expiry uint)
  (fee-amount uint)
  (signature (buff 65)))
  (let (
    (message-hash (generate-message-hash user nonce recipients amounts expiry fee-amount))
  )
    ;; Recover the public key from the signature and verify it matches the user
    (match (secp256k1-recover? message-hash signature)
      recovered-pubkey 
        (match (principal-of? recovered-pubkey)
          recovered-principal (is-eq recovered-principal user)
          none-case false
        )
      error-code false
    )
  )
)

;; Execute individual transfer
(define-private (execute-transfer (recipient principal) (amount uint))
  (if (> amount u0)
    (stx-transfer? amount tx-sender recipient)
    (ok true)
  )
)

;; Process transfers with fold
(define-private (process-transfer (transfer-data { recipient: principal, amount: uint }) (acc { success: bool, index: uint }))
  (if (get success acc)
    (match (execute-transfer (get recipient transfer-data) (get amount transfer-data))
      success { success: true, index: (+ (get index acc) u1) }
      error { success: false, index: (get index acc) }
    )
    acc
  )
)

;; Combine recipients and amounts into transfer data
(define-private (zip-transfers (recipients (list 50 principal)) (amounts (list 50 uint)))
  (map combine-transfer-data recipients amounts)
)

(define-private (combine-transfer-data (recipient principal) (amount uint))
  { recipient: recipient, amount: amount }
)

;; Public Functions

;; Submit multi-send request with off-chain signature
(define-public (submit-multi-send-request
  (user principal)
  (nonce uint)
  (recipients (list 50 principal))
  (amounts (list 50 uint))
  (expiry uint)
  (signature (buff 65)))
  (let (
    (current-nonce (default-to u0 (map-get? user-nonces user)))
    (relayer-fee-amount (var-get relayer-fee))
  )
    ;; Verify relayer is authorized
    (asserts! (default-to false (map-get? authorized-relayers tx-sender)) ERR-INVALID-RELAYER)
    
    ;; Verify nonce
    (asserts! (is-eq nonce (+ current-nonce u1)) ERR-NONCE-ALREADY-USED)
    
    ;; Verify not expired
    (asserts! (> expiry block-height) ERR-EXPIRED-REQUEST)
    
    ;; Verify signature
    (asserts! (verify-signature user nonce recipients amounts expiry relayer-fee-amount signature) ERR-INVALID-SIGNATURE)
    
    ;; Verify recipients list is not empty and amounts match
    (asserts! (is-eq (len recipients) (len amounts)) ERR-INVALID-RECIPIENT)
    
    ;; Store the request
    (map-set pending-requests 
      { user: user, nonce: nonce }
      {
        recipients: recipients,
        amounts: amounts,
        expiry: expiry,
        relayer-fee: relayer-fee-amount,
        signature: signature
      }
    )
    
    (ok true)
  )
)

;; Execute multi-send transaction
(define-public (execute-multi-send (user principal) (nonce uint))
  (let (
    (request-data (unwrap! (map-get? pending-requests { user: user, nonce: nonce }) ERR-UNAUTHORIZED))
    (recipients (get recipients request-data))
    (amounts (get amounts request-data))
    (expiry (get expiry request-data))
    (relayer-fee-amount (get relayer-fee request-data))
    (current-nonce (default-to u0 (map-get? user-nonces user)))
  )
    ;; Verify relayer is authorized
    (asserts! (default-to false (map-get? authorized-relayers tx-sender)) ERR-INVALID-RELAYER)
    
    ;; Verify not expired
    (asserts! (> expiry block-height) ERR-EXPIRED-REQUEST)
    
    ;; Verify nonce sequence
    (asserts! (is-eq nonce (+ current-nonce u1)) ERR-NONCE-ALREADY-USED)
    
    ;; Calculate total amount needed
    (let (
      (total-amount (fold + amounts u0))
      (user-balance (stx-get-balance user))
      (transfer-data (zip-transfers recipients amounts))
    )
      ;; Verify user has sufficient balance
      (asserts! (>= user-balance (+ total-amount relayer-fee-amount)) ERR-INSUFFICIENT-BALANCE)
      
      ;; Execute transfers
      (let (
        (transfer-result (fold process-transfer transfer-data { success: true, index: u0 }))
      )
        (asserts! (get success transfer-result) ERR-TRANSFER-FAILED)
        
        ;; Pay relayer fee
        (try! (as-contract (stx-transfer? relayer-fee-amount user tx-sender)))
        
        ;; Update nonce
        (map-set user-nonces user nonce)
        
        ;; Remove processed request
        (map-delete pending-requests { user: user, nonce: nonce })
        
        ;; Emit event
        (emit-multi-send-event user nonce recipients amounts)
        
        (ok true)
      )
    )
  )
)

;; Admin Functions

;; Add authorized relayer
(define-public (add-relayer (relayer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (map-set authorized-relayers relayer true)
    (ok true)
  )
)

;; Remove authorized relayer
(define-public (remove-relayer (relayer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (map-delete authorized-relayers relayer)
    (ok true)
  )
)

;; Update relayer fee
(define-public (set-relayer-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set relayer-fee new-fee)
    (ok true)
  )
)

;; Read-only Functions

;; Get user nonce
(define-read-only (get-user-nonce (user principal))
  (default-to u0 (map-get? user-nonces user))
)

;; Check if relayer is authorized
(define-read-only (is-authorized-relayer (relayer principal))
  (default-to false (map-get? authorized-relayers relayer))
)

;; Get relayer fee
(define-read-only (get-relayer-fee)
  (var-get relayer-fee)
)

;; Get pending request
(define-read-only (get-pending-request (user principal) (nonce uint))
  (map-get? pending-requests { user: user, nonce: nonce })
)

;; Generate message hash for off-chain signing
(define-read-only (get-message-hash-for-signing
  (user principal) 
  (nonce uint) 
  (recipients (list 50 principal)) 
  (amounts (list 50 uint)) 
  (expiry uint)
  (fee-amount uint))
  (generate-message-hash user nonce recipients amounts expiry fee-amount)
)

;; Initialize contract with default relayer (contract owner)
(map-set authorized-relayers CONTRACT-OWNER true)