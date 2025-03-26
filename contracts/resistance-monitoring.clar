;; Resistance Monitoring Contract
;; Identifies developing immunity to treatments

;; Data Variables
(define-data-var resistance-counter uint u0)

;; Data Maps
(define-map resistance-reports
  { report-id: uint }
  {
    reporter: principal,
    pest-id: uint,
    protocol-id: uint,
    location: (string-utf8 100),
    resistance-level: uint,
    evidence: (string-utf8 500),
    observed-date: uint,
    confirmation-status: (string-utf8 20),
    confirmer: (optional principal),
    confirmation-date: (optional uint),
    recommended-alternatives: (list 10 uint),
    report-date: uint
  }
)

(define-map pest-resistance
  { pest-id: uint }
  { report-ids: (list 100 uint) }
)

(define-map protocol-resistance
  { protocol-id: uint }
  { report-ids: (list 100 uint) }
)

(define-map location-resistance
  { location: (string-utf8 100) }
  { report-ids: (list 100 uint) }
)

(define-map resistance-confirmers
  { confirmer-id: principal }
  { is-active: bool }
)

;; Register as a resistance confirmer
(define-public (register-confirmer)
  (begin
    (map-set resistance-confirmers
      { confirmer-id: tx-sender }
      { is-active: true }
    )
    (ok true)
  )
)

;; Report resistance observation
(define-public (report-resistance
    (pest-id uint)
    (protocol-id uint)
    (location (string-utf8 100))
    (resistance-level uint)
    (evidence (string-utf8 500))
    (observed-date uint)
    (recommended-alternatives (list 10 uint)))
  (let ((report-id (var-get resistance-counter)))
    (begin
      ;; Store the resistance report
      (map-set resistance-reports
        { report-id: report-id }
        {
          reporter: tx-sender,
          pest-id: pest-id,
          protocol-id: protocol-id,
          location: location,
          resistance-level: resistance-level,
          evidence: evidence,
          observed-date: observed-date,
          confirmation-status: u"pending",
          confirmer: none,
          confirmation-date: none,
          recommended-alternatives: recommended-alternatives,
          report-date: block-height
        }
      )

      ;; Update pest-to-resistance mapping
      (let ((pest-list (default-to { report-ids: (list) } (map-get? pest-resistance { pest-id: pest-id }))))
        (map-set pest-resistance
          { pest-id: pest-id }
          { report-ids: (unwrap-panic (as-max-len? (append (get report-ids pest-list) report-id) u100)) }
        )
      )

      ;; Update protocol-to-resistance mapping
      (let ((protocol-list (default-to { report-ids: (list) } (map-get? protocol-resistance { protocol-id: protocol-id }))))
        (map-set protocol-resistance
          { protocol-id: protocol-id }
          { report-ids: (unwrap-panic (as-max-len? (append (get report-ids protocol-list) report-id) u100)) }
        )
      )

      ;; Update location-to-resistance mapping
      (let ((location-list (default-to { report-ids: (list) } (map-get? location-resistance { location: location }))))
        (map-set location-resistance
          { location: location }
          { report-ids: (unwrap-panic (as-max-len? (append (get report-ids location-list) report-id) u100)) }
        )
      )

      ;; Increment counter and return report ID
      (var-set resistance-counter (+ report-id u1))
      (ok report-id)
    )
  )
)

;; Confirm or reject a resistance report
(define-public (confirm-resistance
    (report-id uint)
    (confirmation-status (string-utf8 20)))
  (let ((report-data (map-get? resistance-reports { report-id: report-id }))
        (confirmer-data (map-get? resistance-confirmers { confirmer-id: tx-sender })))
    (if (and (is-some report-data)
             (is-some confirmer-data)
             (get is-active (unwrap-panic confirmer-data))
             (is-eq (get confirmation-status (unwrap-panic report-data)) u"pending"))
      (begin
        (map-set resistance-reports
          { report-id: report-id }
          (merge (unwrap-panic report-data) {
            confirmation-status: confirmation-status,
            confirmer: (some tx-sender),
            confirmation-date: (some block-height)
          })
        )
        (ok true)
      )
      (err u1) ;; Not an active confirmer, report doesn't exist, or already confirmed/rejected
    )
  )
)

;; Update recommended alternatives
(define-public (update-recommended-alternatives
    (report-id uint)
    (recommended-alternatives (list 10 uint)))
  (let ((report-data (map-get? resistance-reports { report-id: report-id })))
    (if (and (is-some report-data)
             (or (is-eq tx-sender (get reporter (unwrap-panic report-data)))
                 (is-some (get confirmer (unwrap-panic report-data)))))
      (begin
        (map-set resistance-reports
          { report-id: report-id }
          (merge (unwrap-panic report-data) {
            recommended-alternatives: recommended-alternatives
          })
        )
        (ok true)
      )
      (err u1) ;; Not the reporter or confirmer, or report doesn't exist
    )
  )
)

;; Get resistance report details by ID
(define-read-only (get-resistance-report (report-id uint))
  (map-get? resistance-reports { report-id: report-id })
)

;; Get all resistance reports for a specific pest
(define-read-only (get-resistance-by-pest (pest-id uint))
  (default-to { report-ids: (list) } (map-get? pest-resistance { pest-id: pest-id }))
)

;; Get all resistance reports for a specific protocol
(define-read-only (get-resistance-by-protocol (protocol-id uint))
  (default-to { report-ids: (list) } (map-get? protocol-resistance { protocol-id: protocol-id }))
)

;; Get all resistance reports at a specific location
(define-read-only (get-resistance-by-location (location (string-utf8 100)))
  (default-to { report-ids: (list) } (map-get? location-resistance { location: location }))
)

;; Get confirmed resistance reports
(define-read-only (get-confirmed-resistance-reports)
  (ok {
    message: u"To find confirmed resistance reports, please query by specific pest, protocol, or location and check the confirmation status"
  })
)

;; Get high resistance reports (resistance level >= threshold)
(define-read-only (get-high-resistance-reports (threshold uint))
  (ok {
    threshold: threshold,
    message: u"To find high resistance reports, please query by specific pest, protocol, or location and then filter by resistance level"
  })
)

;; Check if a user is a confirmer
(define-read-only (is-confirmer (user principal))
  (let ((confirmer-data (map-get? resistance-confirmers { confirmer-id: user })))
    (if (is-some confirmer-data)
      (get is-active (unwrap-panic confirmer-data))
      false
    )
  )
)

;; Get the total number of resistance reports
(define-read-only (get-resistance-report-count)
  (var-get resistance-counter)
)

