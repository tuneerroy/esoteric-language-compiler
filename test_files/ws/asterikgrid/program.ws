[S S S T	T	S S T	S S N
_Push_100][N
S S N
_Create_Label_LOOP][S S S T N
_Push_1][T  S S T	_Subtract][S N
S _Duplicate][N
T	T	S N
_If_negative_Jump_to_Label_EXIT][S S S T	 S T S T S N
_Push_42_*][T	N
S S _Print_as_character][S N
S _Duplicate][S S S T	S T S N
_Push_10][T S T T	_Modulo][N
T	S T N
_If_0_Jump_to_Label_NEWLINE][N
S N
N
_Jump_to_Label_LOOP][N
S S T	N
_Create_Label_NEWLINE][S S S T  S T S N
_Push_10][T N
S S _Print_as_character][N
S N
N
_Jump_to_Label_LOOP]