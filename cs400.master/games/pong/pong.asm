; #File snes-registers.h
; #File pong.h

; # Main Program ======================

; #Code l {Main}
; REP #$30
; {Loop} ; LDA Main.program
; ASL A
; TAX
; JSR (Main.programs,X)
; INC Main.count
; BRA {Loop}
; #Data w Main.programs
{
Engine.Initiate
Engine.Fadein
Engine.Run
}


handlers[];
SIG_INT = 3;
handlers[SIG_INT]();

; #Code w {Engine.Initiate}
; PHP
; PHB
;
; PHK // Load Graphics
; PLB
; SEP #$20
; LDA #$80
; STA VMAINC
; REP #$30
; LDA #$0000
; STA VMADD
; JSL Immediate_DMA
; #Data {$01 $18 frame_map $0800}
; LDA #$0400
; STA VMADD
; JSL Immediate_DMA
; #Data {$01 $18 floor_map $0800}
; LDA #$1000
; STA VMADD
; JSL Immediate_DMA
; #Data {$01 $18 char_set $1000}
; LDA #$7E03
; STA WMADD.h
; LDA #$0320
; STA WMADD.l
; JSL Immediate_DMA
; #Data {$00 $80 color_set $0200}
;
; LDA #$000D // Seed RNG
; STA Rng.number

; # TODO Initiate Shipts

; SEP #$20 // Initiate registers
; LDA #$09
; STA bgmode
; LDA #$00
; STA bg1sc
; LDA #$04
; STA bg2sc
; LDA #$11
; STA bg12nba
; LDA #$02
; STA bg34nba
; LDA #$17
; STA tm
; LDA #$81
; STA nmitimen
; STA NMITIMEN
; LDA #$00
; STA inidisp
; STA INIDISP
; CLI

; REP #$20 // Engine.Fadein
; INC Main.program

; PLB
; PLP
; RTS

; #Code w {Engine.Fadein}
; PHP
; SEP #$20
LDA Main.count // Investigate DEBUG
AND #$10
BEQ {+}
; LDA inidisp
; INC A
; AND #$0F
; STA inidisp
; CMP #$0F
; BNE {+}
; INC Main.program // Engine.Run
; {+} ; JSR Test_Joy
; JSR Update_Cgram
; LDA #$FF
; STA Nmi.ready
; JSL Nmi.Wait
; REP #$20
; STZ pause
; PLP
; RTS

; #Code w {Engine.Run}
; PHP
; SEP #$20

; JSR Test_Joy
; LDA pause
; BEQ {+Go}

; LDA #$09
; STA inidisp
; JSR Pause
; BRA {Done}

; {+Go} ; LDA #$0F
; STA inidisp
; JSL Ready_Oam
; JSL Rng
; JSR Ship
; JSR Snake_Tail
; JSR Collision
; JSR Snake_Head
; JSR Update_Fragment
; JSR Update_Cgram
; JSR Update_Frame
; JSL Hide_Unused_Oam

; LDA #$FF
; STA Nmi.ready
; {Done} ; JSL Nmi.Wait

; PLP
; RTS

; #Code w {Pause}
; RTS

; #Code w {Test_Joy}
; PHP
; REP #$30

; LDX #$0000
; {-} ; LDA joy,X
; BEQ {Trigger}
; CMP #$3030 // Soft reset
; BNE {+}
; JMP RESET_Vector
; {+} ; CLC
; EOR joy.last,X
; BNE {++}
; LDA joy.hold,X
; BEQ {+}
; DEC joy.hold,X
; LDA #$0000
; BRA {Trigger}
; {+} ; SEC
; DEC joy.cool,X
; BEQ {+}
; LDA #$0000
; BRA {Trigger}
; {++} ; LDA #$000F
; STA joy.hold,X
; {+} ; LDA #$000F
; STA joy.cool,X
; LDA joy,X
; BCS {Trigger}
; EOR joy.last,X
; AND joy,X
; {Trigger} ; STA joy.edge,X
; AND #$1000 // Pause
; BEQ {+}
; LDA #$FFFF
; EOR pause
; STA pause

; {+} ; INX
; INX
; CPX #$0008
; BCC {-}

; PLP
; RTS

; #Code w {Ship}
; PHP
; REP #$30

; LDX #ship_1
; BRA {+ship_1}
; {-ship_2} ; LDX #ship_2
; {+ship_1} ; STX ship_index
; LDA ship.status,X
; ASL A
; TAX
; JSR (Ship.programs,X)
; LDX ship_index
; CPX #ship_2
; BNE {-ship_2}

; PLP
; RTS
; #Data w Ship.programs
{
Ship.Ghost
Ship.Live
Ship.Death
Ship.Dead
}

; #Code w {Ship.Ghost}
; LDX ship_index
; LDA #$FFFF
; EOR ship.render,X
; STA ship.render,X
; BEQ {+}
; DEC ship.timer,X
; BNE {+}
; INC ship.status,X
; LDA #SNAKE_LENGTH_MIN
; STA ship.length,X
; #Code w {Ship.Live}
; LDX ship_index // Joy
; {+} ; CPX #ship_1
; BNE {+ship_2}
; LDY #$0000
; BRA {+ship_1}
; {+ship_2} ; LDY #$0002
; {+ship_1} ;
; LDA joy.edge,Y // Polarity (A,B)
; BIT #$8080
; BEQ {+}
; LDA ship.polarity,X
; EOR #$FFFF
; STA ship.polarity,X
; {+} ;
; LDA ship.status,X // Weapons
; CMP #$0001
; BNE {+}
; LDA ship.timer,X
; BNE {+drain}

; LDA joy,Y // Lengthen (X)
; BIT #$0040
; BEQ {+wall}
; LDA ship.charge,X
; BEQ {+}
; LDA ship.length,X
; CLC
; ADC #SNAKE_LENGTH_DELTA
; STA ship.length,X
; LDA #$0000
; STA ship.weapon,X
; BRA {+set}
; {+wall} ; LDA joy,Y // Wall (Y)
; BIT #$4000
; BEQ {+}
; LDA ship.charge,X
; AND #$001C
; BEQ {+}
; STA ship.charge,X
; LDA #$0001
; STA ship.weapon,X
; BRA {+set}
; {+drain} ; DEC ship.timer,X
; BNE {+}
; LDA ship.weapon,X
; CMP #$0000
; BEQ {+}
; LDA ship.charge,X
; BEQ {+}
; {+set} ; DEC ship.charge,X
; LDA #GAUGE_DISCHARGE_RATE
; STA ship.timer,X
; {+} ;

; LDA joy,Y // Movement (D-Pad)
; BIT #$0100 // Right
; BEQ {+left}
; LDA ship.direction,X
; CMP #$0002
; BEQ {+down_up}
; LDA #$0000
; STA ship.direction,X
; BRA {+down_up}
; {+left} ; BIT #$0200 // Left
; BEQ {+down}
; LDA ship.direction,X
; CMP #$0000
; BEQ {+down_up}
; LDA #$0002
; STA ship.direction,X
; {+down_up} ; LDA joy,Y
; {+down} ; BIT #$0400 // Down
; BEQ {+up}
; LDA ship.direction,X
; CMP #$0003
; BEQ {+}
; LDA #$0001
; STA ship.direction,X
; BRA {+}
; {+up} ; BIT #$0800 // Up
; BEQ {+}
; LDA ship.direction,X
; CMP #$0001
; BEQ {+}
; LDA #$0003
; STA ship.direction,X
; {+} ;
; LDA ship.x,X // Update Position
; STA ship.x_last,X
; LDA ship.y,X
; STA ship.y_last,X
; LDA ship.direction,X
; CMP #$0000 // Right
; BNE {+switch}
; LDA ship.x,X
; CLC
; ADC ship.v,X
; CMP #$F800
; BCC {+in}
; LDA #$0800
; {+in} ; STA ship.x,X
; BRA {+break}
; {+switch} ; CMP #$0001 // Down
; BNE {+switch}
; LDA ship.y,X
; CLC
; ADC ship.v,X
; CMP #$D800
; BCC {+in}
; LDA #$1800
; {+in} ; STA ship.y,X
; BRA {+break}
; {+switch} ; CMP #$0002 // Left
; BNE {+switch}
; LDA ship.x,X
; SEC
; SBC ship.v,X
; CMP #$0800
; BCS {+in}
; LDA #$F700
; {+in} ; STA ship.x,X
; BRA {+break}
; {+switch} ; CMP #$0003 // Up
; BNE {+break}
; LDA ship.y,X
; SEC
; SBC ship.v,X
; CMP #$1800
; BCS {+in}
; LDA #$D700
; {+in} ; STA ship.y,X
; {+break} ;
; LDA ship.render,X // Render Ship
; BEQ {+hide}
; STZ Draw_Sprite.char_i
; STZ Draw_Sprite.data_i.h
; CPX #ship_1
; BNE {+ship_2}
; LDA #ship_1_sprite
; BRA {+ship_1}
; {+ship_2} ; LDA #ship_2_sprite
; {+ship_1} ; STA Draw_Sprite.data_i
; SEP #$20
; LDA ship.x.h,X
; STA Draw_Sprite.x
; LDA ship.y.h,X
; STA Draw_Sprite.y
; LDA ship.polarity,X
; BNE {+blue}
; LDA #$21
; BRA {+red}
; {+blue} ; LDA #$23
; {+red} ; STA Draw_Sprite.obj_p_override
; JSL Draw_Sprite
; REP #$20
; {+hide} ;
; RTS

; #Code w {Ship.Death}
; PHB
; PEA $7E00
; PLB
; PLB
; LDX ship_index // Death Blossom
; LDA #$0008
; CLC
; ADC ship.charge,X
; STA Ship.Death.blossom
; LDY #$FFF0
; {-} ; TYA
; CLC
; ADC #$0010
; TAY
; CMP #$0800
; BCS {+max}
; LDA fragment.active,Y
; BNE {-}

; LDA #$FFFF // Color, position
; STA fragment.active,Y
; LDA ship.polarity,X
; STA fragment.polarity,Y
; LDA ship.x,X
; STA fragment.x,Y
; LDA ship.y,X
; STA fragment.y,Y

; {-zero} ; JSL Rng // Random velocity
; AND #$001F
; CLC
; ADC #$0008
; ASL A
; PHX
; TAX
; LDA sinusoid,X
; STA Multiply_16.m1
; LDA #$0001
; STA Multiply_16.m2
; JSL Multiply_16
; LDA Multiply_16.p
; PLX
; STA fragment.vx,Y
; JSL Rng
; AND #$001F
; ASL A
; PHX
; TAX
; LDA sinusoid,X
; STA Multiply_16.m1
; LDA #$0001
; STA Multiply_16.m2
; JSL Multiply_16
; LDA Multiply_16.p
; CLC
; PLX
; STA fragment.vy,Y
; ORA fragment.vx,Y
; BEQ {-zero}

; DEC Ship.Death.blossom
; BNE {-}
; {+max} ;
; STZ ship.render,X // Update Ship
; STZ ship.charge,X
; STZ ship.timer,X
; STZ ship.length,X
; JSL Rng
; AND #$FF00
; CMP #$0800
; BCS {+}
; LDA #$0800
; {+} ; CMP #$F800
; BCC {+}
; LDA #$F700
; {+} ; STA ship.x,X
; JSL Rng
; AND #$FF00
; CMP #$1800
; BCS {+}
; LDA #$1800
; {+} ; CMP #$D800
; BCC {+}
; LDA #$D700
; {+} ; STA ship.y,X
; JSL Rng
; AND #$0003
; STA ship.direction,X
; INC ship.status,X
; LDA #$0020
; STA ship.timer,X
; LDA #$FFFF

; PLB
; RTS

; #Code w {Ship.Dead}
; LDX ship_index
; LDA ship.snake_r,X
; CMP ship.snake_w,X
; BNE {+}
; STZ ship.status,X
; LDA #$002D
; STA ship.timer,X
; {+} ; RTS

; #Code w {Snake_Tail}
; PHP
; REP #$30
; PHB

; PEA $7E00
; PLB
; PLB
; LDX #ship_1
; BRA {+ship_1}
; {-ship_2} ; LDX #ship_2
; {+ship_1} ; STX ship_index
; LDA #SNAKE_RATE_DEAD
; STA Snake_Tail.i
; {-dead} ; LDA ship.snake_w,X // r = w
; SEC
; SBC ship.snake_r,X
; BEQ {+wait}
; BCC {+} // r < w
; CMP ship.length,X
; BCC {+wait}
; BRA {+eat}
; {+} ; CLC // r > w
; ADC #SNAKE_LENGTH_MAX
; CMP ship.length,X
; BCC {+wait}

; {+eat} ; INC ship.snake_r,X // Delete pixel
; INC ship.snake_r,X
; LDA ship.snake_r,X
; CMP #SNAKE_LENGTH_MAX
; BCC {+}
; STZ ship.snake_r,X
; LDA #$0000
; {+} ; CPX #ship_1
; BNE {+ship_2}
; CLC
; ADC #snake_1
; BRA {+ship_1}
; {+ship_2} ; CLC
; ADC #snake_2
; {+ship_1} ; TAX
; LDA $0000,X
; AND #$00FF
; XBA
; STA Plot.x
; LDA $0000,X
; AND #$FF00
; STA Plot.y
; STZ Plot.color
; JSR Plot
; {+wait} ;
; LDX ship_index
; LDA ship.status,X
; CMP #$0001
; BEQ {+}
; DEC Snake_Tail.i
; BNE {-dead}
; {+} ; CPX #ship_2
; BNE {-ship_2}

; PLB
; PLP
; RTS

; #Code w {Collision}
; PHP
; REP #$30
; PHB

; LDA ship_1.status // Ghost check
; CMP #$0001
; BNE {+miss}
; LDA ship_2.status
; CMP #$0001
; BNE {+miss}

; SEP #$20 // Ship on Ship
; LDA ship_1.x.h
; CMP ship_2.x.h
; BNE {+}
; LDA ship_1.y.h
; CMP ship_2.y.h
; BNE {+}
; BRA {+hit}

; {+} ; LDA ship_1.x.h // Ship through Ship
; CMP ship_2.x_last.h
; BNE {+miss}
; LDA ship_1.y.h
; CMP ship_2.y_last.h
; BNE {+miss}
; LDA ship_2.x.h
; CMP ship_1.x_last.h
; BNE {+miss}
; LDA ship_2.y.h
; CMP ship_1.y_last.h
; BNE {+miss}

; {+hit} ; INC ship_1.status // Hit
; INC ship_2.status
; BRA {+done}
; {+miss} ;
; REP #$20 // Ship on trail
; LDX #ship_1
; BRA {+ship_1}
; {-ship_2} ; LDX #ship_2
; {+ship_1} ; STX ship_index
; LDA ship.status,X
; CMP #$0001
; BNE {+}
; LDA ship.x,X
; STA Trailmap_Index.x
; LDA ship.y,X
; STA Trailmap_Index.y
; JSR Trailmap_Index
; PEA $7F00
; PLB
; PLB
; TAX
; LDA trailmap,X
; LDX ship_index
; AND #$0003
; CMP #$0000
; BEQ {+}
; CMP #$0003
; BEQ {collide}
; CMP #$0002
; BEQ {+blue}
; LDA ship.polarity,X
; BEQ {+charge}
; {collide} ; INC ship.status,X
; INC ship.deaths,X
; BRA {+}
; {+blue} ; LDA ship.polarity,X
; BEQ {collide}
; {+charge} ; LDA ship.charge,X
; CMP #$0010
; BCS {+}
; INC ship.charge,X
; {+} ; LDX ship_index
; CPX #ship_2
; BNE {-ship_2}

; {+done} ; PLB
; PLP
; RTS

; #Code w {Snake_Head}
; PHP
; PHB

; PEA $7E00 // Plot bitmap
; PLB
; PLB
; REP #$30
; LDX #ship_1
; BRA {+ship_1}
; {-ship_2} ; LDX #ship_2
; {+ship_1} ; STX ship_index

; LDA ship.status,X
; CMP #$0000
; BEQ {+wait}
; CMP #$0003
; BEQ {+wait}
; LDA ship.y,X
; AND #$FF00
; STA Plot.y
; LDA ship.x,X
; AND #$FF00
; STA Plot.x
; XBA
; ORA Plot.y
; PHA
; LDA ship.timer,X
; BEQ {+color}
; LDA ship.weapon,X
; CMP #$0001
; BNE {+color}
; LDA #$0003
; BRA {+}
; {+color} ; LDA ship.polarity,X
; BEQ {+red}
; LDA #$0002
; BRA {+}
; {+red} ; LDA #$0001
; {+} ; STA Plot.color
; JSR Plot

; LDX ship_index // Snake
; INC ship.snake_w,X
; INC ship.snake_w,X
; LDA ship.snake_w,X
; CMP #$4000
; BCC {+}
; STZ ship.snake_w,X
; LDA #$0000
; {+} ; CPX #ship_1
; BNE {+ship_2}
; CLC
; ADC #snake_1
; BRA {+ship_1}
; {+ship_2} ; CLC
; ADC #snake_2
; {+ship_1} ; TAX
; PLA
; STA $0000,X
; {+wait} ;
; LDX ship_index
; CPX #ship_2
; BNE {-ship_2}

; PLB
; PLP
; RTS

; #Code w {Plot} // Plot(color,x,y)
; PHB

; PEA $7E00 // VRAM Address
; PLB
; PLB
; JSR Trailmap_Index
; STA Plot.trailmap_i
; JSR Bitmap_Index
; CLC
; ADC #$2000
; LDX Nmi.VRAM_Write.table_i
; STA Nmi.VRAM_Write.addr,X

; LDA Plot.x // VRAM Data
; XBA
; AND #$0007
; TAX
; LDA Plot.color
; CMP #$0003
; BNE {+}
; LDA #$8080
; BRA {roll}
; {+} ; CMP #$0002
; BNE {+}
; LDA #$8000
; BRA {roll}
; {+} ; CMP #$0001
; BNE {+}
; LDA #$0080
; BRA {roll}
; {+} ; LDA #$8080
; {roll} ; DEX
; BMI {+}
; LSR A
; BRA {roll}
; {+} ; TAY
; LDA Plot.bitmap_i
; ASL A
; TAX
; LDA Plot.color
; BEQ {+zero}
; TYA
; ORA bitmap,X
; BRA {+}
; {+zero} ; TYA
; EOR #$FFFF
; AND bitmap,X
; {+} ; STA bitmap,X
; LDX Nmi.VRAM_Write.table_i
; STA Nmi.VRAM_Write.data,X
; INX
; INX
; INX
; INX
; STX Nmi.VRAM_Write.table_i

; PEA $7F00 // Trailmap
; PLB
; PLB
; LDX Plot.trailmap_i
; SEP #$20
; LDA Plot.color
; BEQ {+}
; ORA trailmap,X
; {+} ; STA trailmap,X
; REP #$20

; PLB
; RTS

; #Code w {Bitmap_Index}
; LDA Bitmap_Index.x
; AND #$F800
; XBA
; STA Bitmap_Index.i
; LDA Bitmap_Index.y
; AND #$F800
; LSR A
; LSR A
; LSR A
; CLC
; ADC Bitmap_Index.i
; STA Bitmap_Index.i
; LDA Bitmap_Index.y
; XBA
; AND #$0007
; CLC
; ADC Bitmap_Index.i
; STA Bitmap_Index.i
; RTS

; #Code w {Trailmap_Index}
; LDA Trailmap_Index.x
; AND #$FF00
; XBA
; STA Trailmap_Index.i
; LDA Trailmap_Index.y
; AND #$FF00
; CLC
; ADC Trailmap_Index.i
; STA Trailmap_Index.i
; RTS

; #Code w {Update_Fragment}
; PHP
; PHB
; PEA $7E00
; PLB
; PLB
; REP #$30

; LDX #$0000 // Update position
; {-} ; LDA fragment.active,X
; BEQ {+Next}
; LDA fragment.vx,X
; BMI {+Minus}
; CLC
; ADC fragment.x,X
; BCC {+}
; STZ fragment.active,X
; BRA {+Next}
; {+Minus} ; DEC A
; SEC
; ADC fragment.x,X
; BCS {+}
; STZ fragment.active,X
; BRA {+Next}
; {+} ; STA fragment.x,X

; LDA fragment.vy,X
; BMI {+Minus}
; CLC
; ADC fragment.y,X
; BCC {+}
; STZ fragment.active,X
; BRA {+Next}
; {+Minus} ; DEC A
; SEC
; ADC fragment.y,X
; BCS {+}
; STZ fragment.active,X
; BRA {+Next}
; {+} ; STA fragment.y,X

; {+Next} ; TXA
; CLC
; ADC #$0010
; CMP #$0800
; BEQ {+Done}
; TAX
; BRA {-}
; {+Done} ;


; STZ Draw_Sprite.data_i.h // Draw
; LDA #fragment_sprite
; STA Draw_Sprite.data_i
; STZ Draw_Sprite.char_i
; LDX #$0000
; {-} ; SEP #$20
; LDA fragment.active,X
; BEQ {+Next}

; LDA fragment.x.h,X
; STA Draw_Sprite.x
; LDA fragment.y.h,X
; STA Draw_Sprite.y
; LDA fragment.polarity,X
; BNE {Blue}
; LDA #$21
; BRA {+}
; {Blue} ; LDA #$23
; {+} ; STA Draw_Sprite.obj_p_override
; PHX
; JSL Draw_Sprite
; PLX
; {+Next} ; REP #$20
; TXA
; CLC
; ADC #$0010
; TAX
; CMP #$0800
; BCC {-}

; PLB
; PLP
; RTS

; #Code w {Update_Cgram}
; PHP
; REP #$30

; LDA Main.count // Charge Gauge
; AND #$0002
; BNE {+Dim}

; LDA #$001C
; STA $035C
; LDA #$41BC
; STA $035E
; LDA #$7000
; STA $037C
; LDA #$71B0
; STA $037E
; BRA {+Bright}

; {+Dim} ; LDA #$000C
; STA $035C
; LDA #$0018
; STA $035E
; LDA #$3000
; STA $037C
; LDA #$6000
; STA $037E
; {+Bright} ;

; PLP
; RTS

; #Code w {Update_Frame}
; PHP
; REP #$30

; LDX Nmi.VRAM_Write.table_i // Ship 1 Kills
; LDY #$002A // Name Address
; LDA ship_2.deaths // Draw high number
; AND #$00F0
; LSR A
; LSR A
; LSR A
; LSR A
; ORA #$2400 // Priority=1,Pallet=1 (mode1=>pallet0=bg3's)
; STA Nmi.VRAM_Write.data,X
; TYA
; STA Nmi.VRAM_Write.addr,X
; INY
; INX
; INX
; INX
; INX
; LDA ship_2.deaths // Draw low number
; AND #$000F
; ORA #$2400
; STA Nmi.VRAM_Write.data,X
; TYA
; STA Nmi.VRAM_Write.addr,X
; INX
; INX
; INX
; INX

; LDY #$0034 // Ship 2 Kills
; LDA ship_1.deaths
; AND #$00FF
; LSR A
; LSR A
; LSR A
; LSR A
; ORA #$2400
; STA Nmi.VRAM_Write.data,X
; TYA
; STA Nmi.VRAM_Write.addr,X
; INY
; INX
; INX
; INX
; INX
; LDA ship_1.deaths
; AND #$000F
; ORA #$2400
; STA Nmi.VRAM_Write.data,X
; TYA
; STA Nmi.VRAM_Write.addr,X
; INX
; INX
; INX
; INX

; LDA ship_1.charge // Ship 1 Charge Gauge
; LDY #$0025
; {-} ; CPY #$0029
; BEQ {++}
; SEC
; SBC #$0004
; BCC {+}
; PHA
; LDA ship_1.polarity
; BNE {Blue}
; LDA #$2437
; BRA {Red}
; {Blue} ; LDA #$2837
; {Red} ; STA Nmi.VRAM_Write.data,X
; TYA
; STA Nmi.VRAM_Write.addr,X
; INY
; INX
; INX
; INX
; INX
; PLA
; BRA {-}

; {+} ; ADC #$2037
; PHA
; LDA ship_1.polarity
; BNE {Blue}
; PLA
; ORA #$0400
; BRA {Red}
; {Blue} ; PLA
; ORA #$0800
; {Red} ; STA Nmi.VRAM_Write.data,X
; TYA
; STA Nmi.VRAM_Write.addr,X
; INY
; INX
; INX
; INX
; INX

; {-} ; CPY #$0029
; BEQ {++}
; LDA #$2433
; STA Nmi.VRAM_Write.data,X
; TYA
; STA Nmi.VRAM_Write.addr,X
; INY
; INX
; INX
; INX
; INX
; BRA {-}
; {++} ;
; LDA ship_2.charge // Ship 2 Charge Gauge
; LDY #$003A
; {-} ; CPY #$0036
; BEQ {++}
; SEC
; SBC #$0004
; BCC {+}
; PHA
; LDA ship_2.polarity
; BNE {Blue}
; LDA #$6437
; BRA {Red}
; {Blue} ; LDA #$6837
; {Red} ; STA Nmi.VRAM_Write.data,X
; TYA
; STA Nmi.VRAM_Write.addr,X
; DEY
; INX
; INX
; INX
; INX
; PLA
; BRA {-}

; {+} ; ADC #$6037
; PHA
; LDA ship_2.polarity
; BNE {Blue}
; PLA
; ORA #$0400
; BRA {Red}
; {Blue} ; PLA
; ORA #$0800
; {Red} ; STA Nmi.VRAM_Write.data,X
; TYA
; STA Nmi.VRAM_Write.addr,X
; DEY
; INX
; INX
; INX
; INX

; {-} ; CPY #$0036
; BEQ {++}
; LDA #$6433
; STA Nmi.VRAM_Write.data,X
; TYA
; STA Nmi.VRAM_Write.addr,X
; DEY
; INX
; INX
; INX
; INX
; BRA {-}
; {++} ; STX Nmi.VRAM_Write.table_i

; PLP
; RTS










; # General Functions =================


; #Code l {Multiply_16} // 16*16=16
; PHP
; REP #$30
; LDA #$0000
; {-} ; LDX Multiply_16.m1
; BEQ {+Return}
; LSR Multiply_16.m1
; BCC {+}
; CLC
; ADC Multiply_16.m2
; {+} ; ASL Multiply_16.m2
; BRA {-}
; {+Return} ; STA Multiply_16.p
; PLP
; RTL

; #Code l {Divide_16} // 16/16=16
; PHP
; REP #$30
; STZ Divide_16.q
; LDA Divide_16.d
; LDX Divide_16.n
; LDY #$0001
; {-} ; ASL A
; BCS {+}
; INY
; CPY #$0011
; BNE {-}
; {+} ; ROR A
; {-} ; PHA
; TXA
; SEC
; SBC $01,S
; BCC {+}
; TAX
; {+} ; ROL Divide_16.q
; PLA
; LSR A
; DEY
; BNE {-}
; LDA Divide_16.q
; STX Divide_16.r
; PLP
; RTL


; #Code l {Immediate_DMA}
; PHP
; PHB
; SEP #$20
; LDA $05,S
; PHA
; PLB
; REP #$20
; LDA $03,S
; INC A
; TAX
; LDA $0000,X
; STA DMAP
; LDA $0002,X
; STA DMAA.l
; LDA $0004,X
; STA DMAA.b
; SEP #$20
; LDA $0006,X
; STA DMAD.h
; LDA #$01
; STA MDMAEN
; REP #$20
; TXA
; CLC
; ADC #$0006
; STA $03,S
; PLB
; PLP
; RTL


; #Code l {Ready_Oam}
; PHP
; PHB
; PEA $7E00
; PLB
; PLB
; REP #$30
; STZ oam_i
; STZ oam2_byte_i // and bit OS
; LDX #$001E
; {-} ; STZ oam2,X
; DEX
; DEX
; BPL {-}
; PLB
; PLP
; RTL

; #Code l {Hide_Unused_Oam}
; PHP
; REP #$20
; LDA oam_i
; CMP #$0200
; BMI {+}
; PLP
; RTL
; {+} ; LSR A
; STA $00
; LSR A
; ADC $00
; STA $00
; CLC
; PER $000C
; PLA
; ADC $00
; STA $00
; SEP #$20
; LDA #$F0
; JMP ($0000)
; STA $0101
; STA $0105
; STA $0109
; STA $010D
; STA $0111
; STA $0115
; STA $0119
; STA $011D
; STA $0121
; STA $0125
; STA $0129
; STA $012D
; STA $0131
; STA $0135
; STA $0139
; STA $013D
; STA $0141
; STA $0145
; STA $0149
; STA $014D
; STA $0151
; STA $0155
; STA $0159
; STA $015D
; STA $0161
; STA $0165
; STA $0169
; STA $016D
; STA $0171
; STA $0175
; STA $0179
; STA $017D
; STA $0181
; STA $0185
; STA $0189
; STA $018D
; STA $0191
; STA $0195
; STA $0199
; STA $019D
; STA $01A1
; STA $01A5
; STA $01A9
; STA $01AD
; STA $01B1
; STA $01B5
; STA $01B9
; STA $01BD
; STA $01C1
; STA $01C5
; STA $01C9
; STA $01CD
; STA $01D1
; STA $01D5
; STA $01D9
; STA $01DD
; STA $01E1
; STA $01E5
; STA $01E9
; STA $01ED
; STA $01F1
; STA $01F5
; STA $01F9
; STA $01FD
; STA $0201
; STA $0205
; STA $0209
; STA $020D
; STA $0211
; STA $0215
; STA $0219
; STA $021D
; STA $0221
; STA $0225
; STA $0229
; STA $022D
; STA $0231
; STA $0235
; STA $0239
; STA $023D
; STA $0241
; STA $0245
; STA $0249
; STA $024D
; STA $0251
; STA $0255
; STA $0259
; STA $025D
; STA $0261
; STA $0265
; STA $0269
; STA $026D
; STA $0271
; STA $0275
; STA $0279
; STA $027D
; STA $0281
; STA $0285
; STA $0289
; STA $028D
; STA $0291
; STA $0295
; STA $0299
; STA $029D
; STA $02A1
; STA $02A5
; STA $02A9
; STA $02AD
; STA $02B1
; STA $02B5
; STA $02B9
; STA $02BD
; STA $02C1
; STA $02C5
; STA $02C9
; STA $02CD
; STA $02D1
; STA $02D5
; STA $02D9
; STA $02DD
; STA $02E1
; STA $02E5
; STA $02E9
; STA $02ED
; STA $02F1
; STA $02F5
; STA $02F9
; STA $02FD
; PLP
; RTL

; #Code l {Draw_Sprite} (sprite,x,y,char_i,override)
; PHP
; REP #$10
; LDY oam_i
; CPY #$0200 // OAM Overflow
; BCC {+}
; PLP
; RTL
; {+} ; PHB
; SEP #$20
; LDA Draw_Sprite.data_bank
; PHA
; PLB
; LDX Draw_Sprite.data_i
; LDA $0000,X // OBJ Count
; BRA {+}
; {--} ; CPY #$0200 // OAM Overflow
; BCC {++}
; BRL {+Return}
; {++} ; INX
; INX
; INX
; INX
; {+} ; INX
; STA Draw_Sprite.obj_count
; LDA $0000,X // OBJ X Position
; PHX
; CLC
; ADC Draw_Sprite.x
; STA oam.x,Y
; LDA $0004,X // OBJ OAM II
; AND #$03
; BCC {+}
; EOR #$01
; {+} ; STY oam_i
; SEP #$10
; LDY oam2_bit_i
; {-} ; BEQ {+}
; ASL A
; ASL A
; DEY
; DEY
; BRA {-}
; {+} ; LDY oam2_byte_i
; ORA oam2,Y
; STA oam2,Y
; LDY oam2_bit_i
; INY
; INY
; CPY #$08
; BCC {+}
; LDY #$00
; INC oam2_byte_i
; {+} ; STY oam2_bit_i
; REP #$10
; LDY oam_i
; PLX
; LDA $0001,X // OBJ Y Position
; CLC
; ADC Draw_Sprite.y
; STA oam.y,Y
; REP #$20
; LDA $0002,X // OBJ Character/Settings
; AND #$01FF
; CLC
; ADC Draw_Sprite.char_i
; AND #$01FF
; SEP #$20
; STA oam.c,Y
; XBA
; EOR $0003,X
; PHA
; LDA Draw_Sprite.obj_p_override
; BEQ {+}
; PLA
; AND #$C1
; EOR Draw_Sprite.obj_p_override
; BRA {++}
; {+} ; PLA
; {++} ; STA oam.p,Y // Draw Sprite OBJ P
; INY
; INY
; INY
; INY
; LDA Draw_Sprite.obj_count
; DEC A
; BMI {+Return}
; BRL {--}
; {+Return} ; STY oam_i
; PLB
; PLP
; RTL


; #Code l {Rng}
; PHP
; PHB
; PEA $0000
; PLB
; PLB
; REP #$20
; LDA Rng.number
; SEP #$20
; STA M7A
; XBA
; STA M7A
; LDA #$D3
; STA M7B
; REP #$20
; LDA RDMPY24
; INC A
; XBA
; STA Rng.number
; PLB
; PLP
; RTL

; #Data w ship_1_sprite
{ $00
$FE $FD $253C $01
}
; #Data w ship_2_sprite
{ $00
$FE $FD $253D $01
}
; #Data w fragment_sprite
{ $00
$FE $FD $2540 $01
}

; #Data l sinusoid
{ $0000
$0032
$0062
$008E
$00B5
$00D5
$00ED
$00FB
$0100
$00FB
$00ED
$00D5
$00B5
$008E
$0062
$0032
$0000
$FFCE
$FF9E
$FF72
$FF4B
$FF2B
$FF13
$FF05
$FF00
$FF05
$FF13
$FF2B
$FF4B
$FF72
$FF9E
$FFCE
$0000
$0032
$0062
$008E
$00B5
$00D5
$00ED
$00FB
}

; #Data l char_set {#JormungandrChar.dat}
; #Data l frame_map {#JormungandrFrameMap.dat}
; #Data l floor_map {#JormungandrFloorMap.dat}
; #Data l trail_map {#JormungandrTrailMap.dat}
; #Data l color_set {#JormungandrColor.dat}

; #Data $00:FFB0 rom_header
{$52 $3A $4A $4F $52 $4D $00 $00 $00 $00 $00 $00 $00 $00 $00 $00
$52 $3A $4A $6F $72 $6D $75 $6E $67 $61 $6E $64 $72 $20 $20 $20
$20 $20 $20 $20 $20 $20 $02 $09 $01 $01 $33 $00 $00 $00 $00 $00 }
