//==============================================================
//                                                |            |
//   65816 Assembler                              |   | |  *   |
//                                                |   |_/      |
//   K. P. Trofatter                              |   |_|  *   |
//                                                |            |
//-------------------------------------------------------------|
//   assembler.h                                        Beta   |
//==============================================================

//==============================================================
// #Include                                                    |
//==============================================================
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "data.h"


//==============================================================
// #Define                                                     |
//==============================================================
#define NAME_MAX 256




//==============================================================
//                                                             |
// Structures                                                  |
//                                                             |
//==============================================================

//==============================================================
// Structure Prototypes                                        |
//==============================================================

// File structure
struct File_s;
struct Stat_s;

// Dual link list
struct Dll_n;
struct Dll_file_s;
struct Dll_link_s;
struct Dll_name_s;


//==============================================================
// File Structure                                              |
//==============================================================

// Token data type
typedef enum Token_t_{
    Token_null_t_,
    Token_mnemonic_t_,
    Token_name_t_,
    Token_number_t_,
    Token_string_t_,
    Token_symbol_t_,
}Token_t_;

// File statistics
typedef struct Stat_s{
    long line_c; // File line count
    long stmt_c; // Processed statment count
    long assm_c; // Assembly statment count
    long comp_c; // Compiler statment count
    long dire_c; // Directive statment count
    long link_c; // Link statment count
    long name_c; // Name directive count
    long code_c; // Code directive count
    long data_c; // Data directive count
    long code_s; // Code size
    long data_s; // Data size
    long file_s; // File size
}Stat_s;

// File structure
typedef struct File_s{
    char     *name;             // File name
    FILE     *file;             // File pointer
    char     *mode;             // I/O  mode
    char      line[NAME_MAX+1]; // I/O  line buffer
    Token_t_  type;             // Data type
    long      data;             // Data value
    int       size;             // Data size (byte)
    int       m;                // Memory select
    int       x;                // Index select 
    Stat_s   *stat;             // File statistics
}File_s;


//==============================================================
// Dual Link List                                              |
//==============================================================

// Dll data type
typedef enum Dll_t_{
    Dll_null_t_,  // Null (unknown type)
    Dll_head_t_,  // List header
    Dll_file_t_,  // File
    Dll_name_t_,  // Name
    Dll_link_t_   // Link
}Dll_t_;

// Dll Node Structure
typedef struct Dll_n{
    struct Dll_n  *next; // Next list node
    struct Dll_n  *prev; // Previous list node
    Dll_t_  type; // Datatype
    void   *data; // Data pointer
}Dll_n;

// Dll file structure
typedef struct Dll_file_s{
    char   *name; // Name
    Stat_s *stat; // File statistics
}Dll_file_s;

// Dll name structure
typedef struct Dll_name_s{
    char *name;   // Name
    long  data;   // Data
    int   size;   // Data size (byte)
    int   mode;   // RESERVED
}Dll_name_s;

// Dll link structure
typedef struct Dll_link_s{
    char *name;   // Name
    char *file;   // Source file
    long  addr;   // Address
    int   mode;   // Invoking instruction
}Dll_link_s;




//==============================================================
//                                                             |
// Functions                                                   |
//                                                             |
//==============================================================

//==============================================================
// Main Program                                                |
//==============================================================

void First_Pass(char *name);
    
    // Assembler
    void Do_Assembly(File_s*);
    
    // Compiler
    void Do_Compiler(File_s*);
        void Do_Link(File_s*);
        
        // Reserved for future expansion
    
    // Directive
    void Do_Directive   (File_s*);
        void Dir_Block  (File_s*);
        void Dir_Code   (File_s*);
        void Dir_Cpu    (File_s*);
        void Dir_Data   (File_s*);
        void Dir_File   (File_s*);
        void Dir_Halt   (File_s*);
        void Dir_Name   (File_s*);
        void Dir_PC     (File_s*);
        void Dir_Print  (File_s*);
        void Dir_Rom    (File_s*);

void Second_Pass(void);
void Report(void);


//==============================================================
// General                                                     |
//==============================================================

// Error
void Error(int program, int code, File_s *In, void *data);

// File management
File_s* New_File(char *name, char *mode);
void    Free_File(File_s*);

// File format
int  Is_Whitespace (char);
int  Is_Numberspace(char);
int  Is_Namespace  (char);
int  Is_Name       (char*);

// File scanning
void Get_Token(File_s*);
void Read_Operand(File_s*);

// Program counter
long PC_to_File(long);
long File_to_PC(long);
int  Advance_PC(long);


//==============================================================
// Dual Link List                                              |
//==============================================================

// Memory management
Dll_n* Dll_New   (Dll_t_ type);
void   Dll_Free  (Dll_n *list);

// Sorted list
void   Dll_Insert(Dll_n *list, Dll_n *node);
Dll_n* Dll_Find  (Dll_n *list, Dll_n *node);

// Stack
void   Dll_Push  (Dll_n *list, Dll_n *node);
Dll_n* Dll_Pull  (Dll_n *list);

// Miscellaneous
void   Dll_Print (Dll_n *list);
int    Dll_Size  (Dll_n *list);




//==============================================================
//                                                             |
//                                                             |
//                                                             |
//==============================================================
