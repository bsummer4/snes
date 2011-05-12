 ; #LoROM
 ; #File snes-registers.h
 ; #File pong.h ; # TODO not clean

 ; #Code w {RESET_Vector}
 ; SEI
 ; CLC
 ; XCE
 ; JML Reset

 ; #Code w {NMI_Vector}
 ; JML Nmi

 ; #Code w {IRQ_vector}
 ; JML Reset

 ; #Code l {Reset}
 ; REP #$38
 ; PHK
 ; PLB
 ; LDA #$0000
 ; TCD
 ; LDX #$1FFF
 ; TXS
 ; JSR Reset.Registers
 ; JSR Reset.VRAM
 ; JSR Reset.OAM
 ; JSR Reset.CGRAM
 ; JSR Reset.APU
 ; LDA #$0000 // Clear.WRAM
 ; LDX #$4000
 ; {-} ; DEX
 ; DEX
 ; STA $7E:0000,X
 ; STA $7E:4000,X
 ; STA $7E:8000,X
 ; STA $7E:C000,X
 ; STA $7F:0000,X
 ; STA $7F:4000,X
 ; STA $7F:8000,X
 ; STA $7F:C000,X
 ; BNE {-}
 ; TAX
 ; TAY
 ; JML Main

 ; #Code w {Reset.Registers}
 ; PHP
 ; SEP #$20
 ; LDA #$00
 ; STA MEMSEL
 ; LDA #$80
 ; STA INIDISP
 ; STZ OBJSEL
 ; STZ OAMADD.l
 ; STZ OAMADD.h
 ; STZ BGMODE
 ; STZ MOSAIC
 ; STZ BG1SC
 ; STZ BG2SC
 ; STZ BG3SC
 ; STZ BG4SC
 ; STZ BG12NBA
 ; STZ BG34NBA
 ; STZ BG1HOFS
 ; STZ BG1HOFS
 ; STZ BG1VOFS
 ; STZ BG1VOFS
 ; STZ BG2HOFS
 ; STZ BG2HOFS
 ; STZ BG2VOFS
 ; STZ BG2VOFS
 ; STZ BG3HOFS
 ; STZ BG3HOFS
 ; STZ BG3VOFS
 ; STZ BG3VOFS
 ; STZ BG4HOFS
 ; STZ BG4HOFS
 ; STZ BG4VOFS
 ; STZ BG4VOFS
 ; STZ VMAINC
 ; STZ VMADD.l
 ; STZ VMADD.h
 ; STZ M7SEL
 ; STZ M7A
 ; STZ M7A
 ; STZ M7B
 ; STZ M7B
 ; STZ M7C
 ; STZ M7C
 ; STZ M7D
 ; STZ M7D
 ; STZ M7X
 ; STZ M7X
 ; STZ M7Y
 ; STZ M7Y
 ; STZ CGADD
 ; STZ W12SEL
 ; STZ W34SEL
 ; STZ WOBJSEL
 ; STZ WH0
 ; STZ WH1
 ; STZ WH2
 ; STZ WH3
 ; STZ WBGLOG
 ; STZ WOBJLOG
 ; STZ TM
 ; STZ TS
 ; STZ TMW_
 ; STZ TSW_
 ; STZ CGSWSEL
 ; STZ CGADSUB
 ; STZ COLDATA
 ; STZ SETINI
 ; LDA STAT78
 ; STZ WMADD.l
 ; STZ WMADD.h
 ; STZ WMADD.b
 ; STZ NMITIMEN
 ; STZ WRIO
 ; STZ WRMPYA
 ; STZ WRMPYB
 ; STZ WRDIVA.l
 ; STZ WRDIVA.h
 ; STZ WRDIVB
 ; STZ HTIME.l
 ; STZ HTIME.h
 ; STZ VTIME.l
 ; STZ VTIME.h
 ; STZ MDMAEN
 ; STZ HDMAEN
 ; LDA RDNMI
 ; LDA TIMEUP
 ; PLP
 ; RTS

 ; #Code w {Reset.VRAM}
 ; PHP
 ; SEP #$20
 ; LDA #$80
 ; STA VMAINC
 ; REP #$20
 ; STZ VMADD
 ; JSL Immediate_DMA
 ; #Data {$09 $18 $80:FFF0 $0000}
 ; PLP
 ; RTS

 ; #Code w {Reset.OAM}
 ; PHP
 ; REP #$20
 ; STZ OAMADD
 ; JSL Immediate_DMA
 ; #Data {$0A $04 $80:FFF0 $0220}
 ; PLP
 ; RTS

 ; #Code w {Reset.CGRAM}
 ; PHP
 ; SEP #$20
 ; STZ CGADD
 ; JSL Immediate_DMA
 ; #Data {$0A $22 $80:FFF0 $0200}
 ; PLP
 ; RTS

 ; #Code w {Reset.APU}
 ; RTS


 ; # NMI ===============================

 ; #Code l {Nmi.Wait}
 ; PHP
 ; SEP #$20
 ; LDA #$FF
 ; STA Nmi.ready
 ; {Wait}
 ; LDA Nmi.ready
 ; BNE {Wait}
 ; PLP
 ; RTL

 ; #Code l {Nmi}
 ; SEI
 ; PHA
 ; PHX
 ; PHY
 ; PHP
 ; PHB
 ; PHK
 ; PLB
 ; SEP #$20
 ; LDA Nmi.ready
 ; BEQ {+}
 ; STZ CGADD
 ; JSL Immediate_DMA
 ; #Data {$02 $22 $7E:0320 $0200}
 ; REP #$20
 ; LDA #$8000
 ; STA OAMADD
 ; JSL Immediate_DMA
 ; #Data {$02 $04 $7E:0100 $0220}
 ; JSR Nmi.Registers
 ; JSR Nmi.HDMA
 ; JSR Nmi.VRAM_DMA
 ; JSR Nmi.VRAM_Write
 ; INC Nmi.count
 ; SEP #$20
 ; STZ Nmi.ready
 ; {+} ; PLB
 ; PLP
 ; PLY
 ; PLX
 ; PLA
 ; CLI
 ; RTI

 ; #Code w {Nmi.Registers}
 ; PHP
 ; SEP #$20
 ; LDA inidisp
 ; STA INIDISP
 ; LDA objsel
 ; STA OBJSEL
 ; LDA bgmode
 ; STA BGMODE
 ; LDA mosaic
 ; STA MOSAIC
 ; LDA bg1sc
 ; STA BG1SC
 ; LDA bg2sc
 ; STA BG2SC
 ; LDA bg3sc
 ; STA BG3SC
 ; LDA bg4sc
 ; STA BG4SC
 ; LDA bg12nba
 ; STA BG12NBA
 ; LDA bg34nba
 ; STA BG34NBA
 ; LDA bg1hofs.l
 ; STA BG1HOFS
 ; LDA bg1hofs.h
 ; STA BG1HOFS
 ; LDA bg1vofs.l
 ; STA BG1VOFS
 ; LDA bg1vofs.h
 ; STA BG1VOFS
 ; LDA bg2hofs.l
 ; STA BG2HOFS
 ; LDA bg2hofs.h
 ; STA BG2HOFS
 ; LDA bg2vofs.l
 ; STA BG2VOFS
 ; LDA bg2vofs.h
 ; STA BG2VOFS
 ; LDA bg3hofs.l
 ; STA BG3HOFS
 ; LDA bg3hofs.h
 ; STA BG3HOFS
 ; LDA bg3vofs.l
 ; STA BG3VOFS
 ; LDA bg3vofs.h
 ; STA BG3VOFS
 ; LDA bg4hofs.l
 ; STA BG4HOFS
 ; LDA bg4hofs.h
 ; STA BG4HOFS
 ; LDA bg4vofs.l
 ; STA BG4VOFS
 ; LDA bg4vofs.h
 ; STA BG4VOFS
 ; LDA vmainc
 ; STA VMAINC
 ; LDA m7sel
 ; STA M7SEL
 ; LDA m7a.l
 ; STA M7A
 ; LDA m7a.h
 ; STA M7A
 ; LDA m7b.l
 ; STA M7B
 ; LDA m7b.h
 ; STA M7B
 ; LDA m7c.l
 ; STA M7C
 ; LDA m7c.h
 ; STA M7C
 ; LDA m7d.l
 ; STA M7D
 ; LDA m7d.h
 ; STA M7D
 ; LDA m7x.l
 ; STA M7X
 ; LDA m7x.h
 ; STA M7X
 ; LDA m7y.l
 ; STA M7Y
 ; LDA m7y.h
 ; STA M7Y
 ; LDA w12sel
 ; STA W12SEL
 ; LDA w34sel
 ; STA W34SEL
 ; LDA wobjsel
 ; STA WOBJSEL
 ; LDA wh0
 ; STA WH0
 ; LDA wh1
 ; STA WH1
 ; LDA wh2
 ; STA WH2
 ; LDA wh3
 ; STA WH3
 ; LDA wbglog
 ; STA WBGLOG
 ; LDA wobjlog
 ; STA WOBJLOG
 ; LDA tm
 ; STA TM
 ; LDA ts
 ; STA TS
 ; LDA tmw
 ; STA TMW_
 ; LDA tsw
 ; STA TSW_
 ; LDA cgswsel
 ; STA CGSWSEL
 ; LDA cgadsub
 ; STA CGADSUB
 ; LDA coldata_blue
 ; STA COLDATA
 ; LDA coldata_green
 ; STA COLDATA
 ; LDA coldata_red
 ; STA COLDATA
 ; LDA setini
 ; STA SETINI
 ; LDA STAT77
 ; STA stat77
 ; LDA STAT78
 ; STA stat78
 ; LDA nmitimen
 ; STA NMITIMEN
 ; LDA wrio
 ; STA WRIO
 ; LDA htime.l
 ; STA HTIME.l
 ; LDA htime.h
 ; STA HTIME.h
 ; LDA vtime.l
 ; STA VTIME.l
 ; LDA vtime.h
 ; STA VTIME.h
 ; LDA memsel
 ; STA MEMSEL
 ; LDA RDNMI
 ; STA rdnmi
 ; LDA TIMEUP
 ; STA timeup
 ; LDA RDIO
 ; STA rdio
 ; {Wait} ; LDA HVBJOY
 ; STA hvbjoy
 ; AND #$01
 ; BNE {Wait}
 ; REP #$20
 ; LDA joy1
 ; STA joy1.last
 ; LDA joy2
 ; STA joy2.last
 ; LDA joy3
 ; STA joy3.last
 ; LDA joy4
 ; STA joy4.last
 ; LDA JOY1
 ; STA joy1
 ; LDA JOY2
 ; STA joy2
 ; LDA JOY3
 ; STA joy3
 ; LDA JOY4
 ; STA joy4
 ; PLP
 ; RTS

 ; #Code w {Nmi.HDMA}
 ; PHP
 ; SEP #$10
 ; LDX #$10
 ; LDY #$00
 ; {-} ; REP #$20
 ; LDA [Nmi.HDMA.data],Y
 ; CMP #$0000
 ; BEQ {+}
 ; STA DMAP,X
 ; INY
 ; INY
 ; LDA [Nmi.HDMA.data],Y
 ; INY
 ; INY
 ; STA DMAA.l,X
 ; LDA [Nmi.HDMA.data],Y
 ; INY
 ; INY
 ; STA DMAA.b,X
 ; LDA [Nmi.HDMA.data],Y
 ; INY
 ; INY
 ; STA DMAD,X
 ; {+} ; SEP #$20
 ; TXA
 ; CLC
 ; ADC #$10
 ; CMP #$80
 ; BEQ {+}
 ; TAX
 ; BRA {-}
 ; {+} ; LDA hdmaen
 ; STA HDMAEN
 ; PLP
 ; RTS

 ; #Code w {Nmi.VRAM_DMA}
 ; PHP
 ; {-} ; REP #$30
 ; LDA Nmi.VRAM_DMA.data_i
 ; BEQ {+}
 ; SEC
 ; SBC #$000A
 ; STA Nmi.VRAM_DMA.data_i
 ; TAY
 ; SEP #$20
 ; LDA [Nmi.VRAM_DMA.data],Y
 ; INY
 ; STA VMAINC
 ; REP #$20
 ; LDA [Nmi.VRAM_DMA.data],Y
 ; INY
 ; INY
 ; STA VMADD
 ; LDA [Nmi.VRAM_DMA.data],Y
 ; INY
 ; INY
 ; STA DMAP
 ; LDA [Nmi.VRAM_DMA.data],Y
 ; INY
 ; INY
 ; STA DMAA.l
 ; LDA [Nmi.VRAM_DMA.data],Y
 ; INY
 ; INY
 ; STA DMAA.b
 ; SEP #$20
 ; LDA [Nmi.VRAM_DMA.data],Y
 ; STA DMAD
 ; LDA #$01
 ; STA MDMAEN
 ; BRA {-}
 ; {+} ; PLP
 ; RTS

 ; #Code w {Nmi.VRAM_Write}
 ; PHP
 ; REP #$20
 ; LDA Nmi.VRAM_Write.table_i
 ; BEQ {+}
 ; SEP #$20
 ; LDA #$80
 ; STA VMAINC
 ; REP #$30
 ; LDX #$0000
 ; {-} ; LDA Nmi.VRAM_Write.addr,X
 ; STA VMADD
 ; LDA Nmi.VRAM_Write.data,X
 ; STA VMDATA
 ; INX
 ; INX
 ; INX
 ; INX
 ; CPX Nmi.VRAM_Write.table_i
 ; BNE {-}
 ; STZ Nmi.VRAM_Write.table_i
 ; {+} ; PLP
 ; RTS
 ; #Data $00:FFE0 vector_table
