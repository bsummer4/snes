; #LoROM
; #Code w main
; CLC
; XCE
; SEP #$30b
; {yay704}
; LDA #$0Fb
; STA $2100w
; LDA #$00b
; STA $2121w
; REP #$30b
; INC $0000
; LDA $0000
; SEP #$30b
; STA $2122w
; XBA
; STA $2122w
; BRA {yay704}
; RTS
; #Data $00:FFFC {$8000 $0000}
