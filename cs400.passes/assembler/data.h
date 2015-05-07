//==============================================================
//                                                |            |
//   65816 Instruction Set                        |   | |  *   |
//                                                |   |_/      |
//   K. P. Trofatter                              |   |_|  *   |
//                                                |            |
//-------------------------------------------------------------|
//   data.h                                             Beta   |
//==============================================================

//==============================================================
// Instruction Structure                                       |
//==============================================================
typedef struct{
    char *mnemonic;
    short addr_m[22];
}Instruction;


//==============================================================
// Addressing Modes                                            |
//==============================================================
#define ADDR_M_COUNT 22

typedef enum Addr_m_{
      imp_ , //  0 |         | Implied
      a_   , //  1 | A       | Accumulator
      i_   , //  2 | #       | Immediate
      b_   , //  3 | b       | Direct
      bx_  , //  4 | b,X     | Direct X Indexed
      by_  , //  5 | b,Y     | Direct Y Indexed
      bs_  , //  6 | b,S     | Stack Indexed
      bi_  , //  7 | (b)     | Direct Indirect
      bl_  , //  8 | [b]     | Direct Indirect Long
      bxi_ , //  9 | (b,X)   | Direct X Indexed Indirect
      biy_ , // 10 | (b),Y   | Direct Indirect Y Indexed
      bly_ , // 11 | [b],Y   | Direct Indirect Long Y Indexed
      bsiy_, // 12 | (b,S),Y | Stack Relative Indirect Y Indexed
      bm_  , // 13 | b,b     | Block Move
      w_   , // 14 | w       | Absolute
      wx_  , // 15 | w,X     | Absolute X Indexed
      wy_  , // 16 | w,Y     | Absolute Y Indexed
      wi_  , // 17 | (w)     | Absolute Indirect
      wl_  , // 18 | [w]     | Absolute Indirect Long
      wxi_ , // 19 | (w,X)   | Absolute X Indexed Indirect
      l_   , // 20 | l       | Long
      lx_  , // 21 | l,X     | Long X Indexed
}Addr_m_;


//==============================================================
// Instruction Set                                             |
//==============================================================
#define MNEMONIC_COUNT 99

Instruction Set[MNEMONIC_COUNT]={
// ======================================================================================================================
// |      |  0 |  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 |
// | ASM  |----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----|
// |      | imp|  A |  # |  b |  X |  Y |  S | () | [] | (X)| ()Y| []Y|(S)Y| bm |  w |  X |  Y | () | [] | (X)|  l |  X |
// ======================================================================================================================
   {"ADC",{ -1 , -1 ,0x69,0x65,0x75, -1 ,0x63,0x72,0x67,0x61,0x71,0x77,0x73, -1 ,0x6D,0x7D,0x79, -1 , -1 , -1 ,0x6F,0x7F}},
   {"AND",{ -1 , -1 ,0x29,0x25,0x35, -1 ,0x23,0x32,0x27,0x21,0x31,0x37,0x33, -1 ,0x2D,0x3D,0x39, -1 , -1 , -1 ,0x2F,0x3F}},
   {"ASL",{ -1 ,0x0A, -1 ,0x06,0x16, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0x0E,0x1E, -1 , -1 , -1 , -1 , -1 , -1 }},
   {"BCC",{ -1 , -1 , -1 ,0x90, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"BCS",{ -1 , -1 , -1 ,0xB0, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"BEQ",{ -1 , -1 , -1 ,0xF0, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"BGE",{ -1 , -1 , -1 ,0xB0, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"BIT",{ -1 , -1 ,0x89,0x24,0x34, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0x2C,0x3C, -1 , -1 , -1 , -1 , -1 , -1 }},
   {"BLT",{ -1 , -1 , -1 ,0x90, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"BMI",{ -1 , -1 , -1 ,0x30, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"BNE",{ -1 , -1 , -1 ,0xD0, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"BPL",{ -1 , -1 , -1 ,0x10, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"BRA",{ -1 , -1 , -1 ,0x80, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"BRK",{ -1 , -1 , -1 ,0x00, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"BRL",{ -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0x82, -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"BVC",{ -1 , -1 , -1 ,0x50, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"BVS",{ -1 , -1 , -1 ,0x70, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"CLC",{0x18, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"CLD",{0xD8, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"CLI",{0x58, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"CLV",{0xB8, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"CMP",{ -1 , -1 ,0xC9,0xC5,0xD5, -1 ,0xC3,0xD2,0xC7,0xC1,0xD1,0xD7,0xD3, -1 ,0xCD,0xDD,0xD9, -1 , -1 , -1 ,0xCF,0xDF}},
   {"COP",{ -1 , -1 , -1 ,0x02, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"CPX",{ -1 , -1 ,0xE0,0xE4, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0xEC, -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"CPY",{ -1 , -1 ,0xC0,0xC4, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0xCC, -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"DEC",{ -1 ,0x3A, -1 ,0xC6,0xD6, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0xCE,0xDE, -1 , -1 , -1 , -1 , -1 , -1 }},
   {"DEX",{0xCA, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"DEY",{0x88, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"EOR",{ -1 , -1 ,0x49,0x45,0x55, -1 ,0x43,0x52,0x47,0x41,0x51,0x57,0x53, -1 ,0x4D,0x5D,0x59, -1 , -1 , -1 ,0x4F,0x5F}},
   {"INC",{ -1 ,0x1A, -1 ,0xE6,0xF6, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0xEE,0xFE, -1 , -1 , -1 , -1 , -1 , -1 }},
   {"INX",{0xE8, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"INY",{0xC8, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"JML",{ -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0xDC, -1 ,0x5C, -1 }},
   {"JMP",{ -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0x4C, -1 , -1 ,0x6C,0xDC,0x7C,0x5C, -1 }},
   {"JSL",{ -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0x22, -1 }},
   {"JSR",{ -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0x20, -1 , -1 , -1 , -1 ,0xFC,0x22, -1 }},
   {"LDA",{ -1 , -1 ,0xA9,0xA5,0xB5, -1 ,0xA3,0xB2,0xA7,0xA1,0xB1,0xB7,0xB3, -1 ,0xAD,0xBD,0xB9, -1 , -1 , -1 ,0xAF,0xBF}},
   {"LDX",{ -1 , -1 ,0xA2,0xA6, -1 ,0xB6, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0xAE, -1 ,0xBE, -1 , -1 , -1 , -1 , -1 }},
   {"LDY",{ -1 , -1 ,0xA0,0xA4,0xB4, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0xAC,0xBC, -1 , -1 , -1 , -1 , -1 , -1 }},
   {"LSR",{ -1 ,0x4A, -1 ,0x46,0x56, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0x4E,0x5E, -1 , -1 , -1 , -1 , -1 , -1 }},
   {"MVN",{ -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0x54, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"MVP",{ -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0x44, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"NOP",{0xEA, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"ORA",{ -1 , -1 ,0x09,0x05,0x15, -1 ,0x03,0x12,0x07,0x01,0x11,0x17,0x13, -1 ,0x0D,0x1D,0x19, -1 , -1 , -1 ,0x0F,0x1F}},
   {"PEA",{ -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0xF4, -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"PEI",{ -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0xD4, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"PER",{ -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0x62, -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"PHA",{0x48, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"PHB",{0x8B, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"PHD",{0x0B, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"PHK",{0x4B, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"PHP",{0x08, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"PHX",{0xDA, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"PHY",{0x5A, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"PLA",{0x68, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"PLB",{0xAB, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"PLD",{0x2B, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"PLP",{0x28, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"PLX",{0xFA, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"PLY",{0x7A, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"REP",{ -1 , -1 ,0xC2, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"ROL",{ -1 ,0x2A, -1 ,0x26,0x36, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0x2E,0x3E, -1 , -1 , -1 , -1 , -1 , -1 }},
   {"ROR",{ -1 ,0x6A, -1 ,0x66,0x76, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0x6E,0x7E, -1 , -1 , -1 , -1 , -1 , -1 }},
   {"RTI",{0x40, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"RTL",{0x6B, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"RTS",{0x60, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"SBC",{ -1 , -1 ,0xE9,0xE5,0xF5, -1 ,0xE3,0xF2,0xE7,0xE1,0xF1,0xF7,0xF3, -1 ,0xED,0xFD,0xF9, -1 , -1 , -1 ,0xEF,0xFF}},
   {"SEC",{0x38, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"SED",{0xF8, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"SEI",{0x78, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"SEP",{ -1 , -1 ,0xE2, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"STA",{ -1 , -1 , -1 ,0x85,0x95, -1 ,0x83,0x92,0x87,0x81,0x91,0x97,0x93, -1 ,0x8D,0x9D,0x99, -1 , -1 , -1 ,0x8F,0x9F}},
   {"STP",{0xDB, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"STX",{ -1 , -1 , -1 ,0x86, -1 ,0x96, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0x8E, -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"STY",{ -1 , -1 , -1 ,0x84,0x94, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0x8C, -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"STZ",{ -1 , -1 , -1 ,0x64,0x74, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0x9C,0x9E, -1 , -1 , -1 , -1 , -1 , -1 }},
   {"SWA",{0xFB, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TAD",{0x5B, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TAS",{0x1B, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TAX",{0xAA, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TAY",{0xA8, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TCD",{0x5B, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TCS",{0x1B, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TDA",{0x7B, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TDC",{0x7B, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TRB",{ -1 , -1 , -1 ,0x14, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0x1C, -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TSA",{0x3B, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TSB",{ -1 , -1 , -1 ,0x04, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 ,0x0C, -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TSC",{0x3B, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TSX",{0xBA, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TXA",{0x8A, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TXS",{0x9A, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TXY",{0x9B, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TYA",{0x98, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"TYX",{0xBB, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"WAI",{0xCB, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"WDM",{ -1 , -1 , -1 ,0x42, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"XBA",{0xEB, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }},
   {"XCE",{0xFB, -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 , -1 }}
// ======================================================================================================================
// |      | imp|  A |  # |  b |  X |  Y |  S | () | [] | (X)| ()Y| []Y|(S)Y| bm |  w |  X |  Y | () | [] | (X)|  l |  X |
// | ASM  |----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----|
// |      |  0 |  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 | 21 |
// ======================================================================================================================
};




//==============================================================
//                                                             |
//                                                             |
//                                                             |
//==============================================================