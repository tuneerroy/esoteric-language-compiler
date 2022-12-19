[S S S N
_Push_0][S N
S _Duplicate_0][S N
S _Duplicate_0][T	N
T	T	_Read_STDIN_as_integer][T	T	T	_Retrieve_input][N
S S N
_Create_Label_LOOP][S N
S _Duplicate][N
T	S S N
_If_0_Jump_to_Label_DONE][S N
S _Duplicate][S S S T	S T S N
_Push_10][T S T T	_Modulo][S T	 S S T	S N
_Copy_0-based_2nd][T	 S S S _Add][S N
T	_Swap_top_two][S S S T  S T S N
_Push_10][T S T S _Integer_divide][N
S N
N
_Jump_to_Label_LOOP][N
S S S N
_Create_Label_DONE][T	T	T	_Retrieve_input][S N
T	_Swap_top_two][S S S T  S N
_Push_2][T  S S N
_Multiply]{T	 S T T	_Modulo][T  N
S T _Print_as_integer]
