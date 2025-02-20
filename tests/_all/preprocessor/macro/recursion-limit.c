// tests for recursion depth of macros

// putchar
#include <stdio.h>

// Note(2025-02-01): The tail call optimization was removed to support self-referencing macros.
//
// // Generally, the recursion limit for macros is 100 / 2 as defined by MACRO_RECURSION_MAX.
// // However, when the macro is in "tail position", there's no limit on nesting depth.
// // Testing with a chain of 200 macros to test this "tail call optimization".
// #define A1 1
// #define A2 A1
// #define A3 A2
// #define A4 A3
// #define A5 A4
// #define A6 A5
// #define A7 A6
// #define A8 A7
// #define A9 A8
// #define A10 A9
// #define A11 A10
// #define A12 A11
// #define A13 A12
// #define A14 A13
// #define A15 A14
// #define A16 A15
// #define A17 A16
// #define A18 A17
// #define A19 A18
// #define A20 A19
// #define A21 A20
// #define A22 A21
// #define A23 A22
// #define A24 A23
// #define A25 A24
// #define A26 A25
// #define A27 A26
// #define A28 A27
// #define A29 A28
// #define A30 A29
// #define A31 A30
// #define A32 A31
// #define A33 A32
// #define A34 A33
// #define A35 A34
// #define A36 A35
// #define A37 A36
// #define A38 A37
// #define A39 A38
// #define A40 A39
// #define A41 A40
// #define A42 A41
// #define A43 A42
// #define A44 A43
// #define A45 A44
// #define A46 A45
// #define A47 A46
// #define A48 A47
// #define A49 A48
// #define A50 A49
// #define A51 A50
// #define A52 A51
// #define A53 A52
// #define A54 A53
// #define A55 A54
// #define A56 A55
// #define A57 A56
// #define A58 A57
// #define A59 A58
// #define A60 A59
// #define A61 A60
// #define A62 A61
// #define A63 A62
// #define A64 A63
// #define A65 A64
// #define A66 A65
// #define A67 A66
// #define A68 A67
// #define A69 A68
// #define A70 A69
// #define A71 A70
// #define A72 A71
// #define A73 A72
// #define A74 A73
// #define A75 A74
// #define A76 A75
// #define A77 A76
// #define A78 A77
// #define A79 A78
// #define A80 A79
// #define A81 A80
// #define A82 A81
// #define A83 A82
// #define A84 A83
// #define A85 A84
// #define A86 A85
// #define A87 A86
// #define A88 A87
// #define A89 A88
// #define A90 A89
// #define A91 A90
// #define A92 A91
// #define A93 A92
// #define A94 A93
// #define A95 A94
// #define A96 A95
// #define A97 A96
// #define A98 A97
// #define A99 A98
// #define A100 A99
// #define A101 A100
// #define A102 A101
// #define A103 A102
// #define A104 A103
// #define A105 A104
// #define A106 A105
// #define A107 A106
// #define A108 A107
// #define A109 A108
// #define A110 A109
// #define A111 A110
// #define A112 A111
// #define A113 A112
// #define A114 A113
// #define A115 A114
// #define A116 A115
// #define A117 A116
// #define A118 A117
// #define A119 A118
// #define A120 A119
// #define A121 A120
// #define A122 A121
// #define A123 A122
// #define A124 A123
// #define A125 A124
// #define A126 A125
// #define A127 A126
// #define A128 A127
// #define A129 A128
// #define A130 A129
// #define A131 A130
// #define A132 A131
// #define A133 A132
// #define A134 A133
// #define A135 A134
// #define A136 A135
// #define A137 A136
// #define A138 A137
// #define A139 A138
// #define A140 A139
// #define A141 A140
// #define A142 A141
// #define A143 A142
// #define A144 A143
// #define A145 A144
// #define A146 A145
// #define A147 A146
// #define A148 A147
// #define A149 A148
// #define A150 A149
// #define A151 A150
// #define A152 A151
// #define A153 A152
// #define A154 A153
// #define A155 A154
// #define A156 A155
// #define A157 A156
// #define A158 A157
// #define A159 A158
// #define A160 A159
// #define A161 A160
// #define A162 A161
// #define A163 A162
// #define A164 A163
// #define A165 A164
// #define A166 A165
// #define A167 A166
// #define A168 A167
// #define A169 A168
// #define A170 A169
// #define A171 A170
// #define A172 A171
// #define A173 A172
// #define A174 A173
// #define A175 A174
// #define A176 A175
// #define A177 A176
// #define A178 A177
// #define A179 A178
// #define A180 A179
// #define A181 A180
// #define A182 A181
// #define A183 A182
// #define A184 A183
// #define A185 A184
// #define A186 A185
// #define A187 A186
// #define A188 A187
// #define A189 A188
// #define A190 A189
// #define A191 A190
// #define A192 A191
// #define A193 A192
// #define A194 A193
// #define A195 A194
// #define A196 A195
// #define A197 A196
// #define A198 A197
// #define A199 A198
// #define A200 A199

// Testing with a chain of 50 non-tail macro expansions to test the recursion limit.
#define B1 0 + 0
#define B2 B1 + 0
#define B3 B2 + 0
#define B4 B3 + 0
#define B5 B4 + 0
#define B6 B5 + 0
#define B7 B6 + 0
#define B8 B7 + 0
#define B9 B8 + 0
#define B10 B9 + 0
#define B11 B10 + 0
#define B12 B11 + 0
#define B13 B12 + 0
#define B14 B13 + 0
#define B15 B14 + 0
#define B16 B15 + 0
#define B17 B16 + 0
#define B18 B17 + 0
#define B19 B18 + 0
#define B20 B19 + 0
#define B21 B20 + 0
#define B22 B21 + 0
#define B23 B22 + 0
#define B24 B23 + 0
#define B25 B24 + 0
#define B26 B25 + 0
#define B27 B26 + 0
#define B28 B27 + 0
#define B29 B28 + 0
#define B30 B29 + 0
#define B31 B30 + 0
#define B32 B31 + 0
#define B33 B32 + 0
#define B34 B33 + 0
#define B35 B34 + 0
#define B36 B35 + 0
#define B37 B36 + 0
#define B38 B37 + 0
#define B39 B38 + 0
#define B40 B39 + 0
#define B41 B40 + 0
#define B42 B41 + 0
#define B43 B42 + 0
#define B44 B43 + 0
#define B45 B44 + 0
#define B46 B45 + 0
#define B47 B46 + 0
#define B48 B47 + 0
#define B49 B48 + 0
#define B50 B49 + 0
// Going over the 50 limit is fine as long as the macro is not called
#define B51 B50 + 0
#define B52 B51 + 0
#define B53 B52 + 0
#define B54 B53 + 0
#define B55 B54 + 0
#define B56 B55 + 0
#define B57 B56 + 0
#define B58 B57 + 0
#define B59 B58 + 0
#define B60 B59 + 0
#define B61 B60 + 0
#define B62 B61 + 0
#define B63 B62 + 0
#define B64 B63 + 0
#define B65 B64 + 0
#define B66 B65 + 0
#define B67 B66 + 0
#define B68 B67 + 0
#define B69 B68 + 0
#define B70 B69 + 0
#define B71 B70 + 0
#define B72 B71 + 0
#define B73 B72 + 0
#define B74 B73 + 0
#define B75 B74 + 0
#define B76 B75 + 0
#define B77 B76 + 0
#define B78 B77 + 0
#define B79 B78 + 0
#define B80 B79 + 0
#define B81 B80 + 0
#define B82 B81 + 0
#define B83 B82 + 0
#define B84 B83 + 0
#define B85 B84 + 0
#define B86 B85 + 0
#define B87 B86 + 0
#define B88 B87 + 0
#define B89 B88 + 0
#define B90 B89 + 0
#define B91 B90 + 0
#define B92 B91 + 0
#define B93 B92 + 0
#define B94 B93 + 0
#define B95 B94 + 0
#define B96 B95 + 0
#define B97 B96 + 0
#define B98 B97 + 0
#define B99 B98 + 0
#define B100 B99 + 0
#define B101 B100 + 0
#define B102 B101 + 0

void putdigit(int n) {
  putchar('0' + n);
  putchar('\n');
}

void main() {
  // putdigit(A200); // Disabled, see note above
  putdigit(B50);
}
