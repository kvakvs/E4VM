%%% @doc Given a list of integers (the program) figure out Huffman encoding
%%% for this program and the decoding tree.
%%% Opcodes in the code then can be encoded using opcode frequency tree
%%% (either dynamically calculated by uasm_stats or hardcoded).
%%% @end

-module(uasm_huffman).

-export([
  decode/2,
  encode/1,
  encode_funs/1,
  encode_labels/1,
  encode_tree/2,
  merge_bits_into_binary/1
]).


%% @doc Given Huffman tree (a dict), encode it as a chain of bit 0/1 prefixed
%% node values of fixed length (LeafBits)
encode_tree(Leaf, LeafBits) when is_integer(Leaf) ->
  [<<1:1>>, <<Leaf:LeafBits>>];

encode_tree({L, R}, LeafBits) ->
  [<<0:1>>, encode_tree(L, LeafBits), encode_tree(R, LeafBits)].


%% @doc Given the module (for labels) and list of compiled funs with operators
%% {Opcode, Arg} produce frequency tree and encode each fun separately.
encode_funs(#{'$' := module, funs := Funs}) ->
  %% ETS table created by uasm_stats during ASM compilation stage
  Tree = tree(ets:tab2list(instr_stat)),
  Dict = dict:from_list(codewords(Tree)),

  Funs1 = lists:map(
    fun({FunArity, #{'$' := e4fun, output := Code}}) ->
      FunObject1 = encode_one_fun(Code, Dict),
      FunObject1#{
        tree => Tree,
        name => FunArity
      }
    end,
    Funs),
  #{'$' => huffman,
    tree => Tree,
    output => Funs1}.


%% @doc A helper which unwraps one opcode and its args, OR marks the bit
%% position for a label into the 'labels' key of the state
encode_one_op_fold({label, F},
                   State = #{labels := L0,
                             output_position := OutPos}) ->
  L1 = orddict:store(F, OutPos, L0),
  State#{labels => L1};

encode_one_op_fold({Op, Args},
                   State = #{output := O1,
                             dict := Dict,
                             output_position := OutPos}) ->
  OpBits = dict:fetch(Op, Dict),
  Output = [[OpBits | Args] | O1],
  OutputBitSize = bit_size(OpBits) + bit_size_list(Args, 0),
  State#{output => Output,
         output_position => OutPos + OutputBitSize}.


%% @doc Calculates bit size of [<<Bits1:X1>>, <<Bits2:X2>>, ...]
bit_size_list([], A) -> A;
bit_size_list([Bits | T], A) -> bit_size_list(T, A + bit_size(Bits)).


%% @doc Given function code (list of {Op, [Args]}) encode it as compressed
%% opcodes + compacted args.
%% Returns: an one_fun map with labels [{F, BitPos}, ...],
%%    intermediate output (for debug) and bytes output (binary)
encode_one_fun(FunCode, Dict) ->
  #{output := Out1,
    labels := Labels} = lists:foldl(fun encode_one_op_fold/2,
                                    #{dict => Dict,
                                      output => [],
                                      labels => orddict:new(),
                                      output_position => 0},
                                    FunCode),

  io:format("~p~n", [Labels]),
  OutBinary = merge_bits_into_binary(Out1),

  #{'$' => one_fun,
    output_intermediate => Out1,
    labels => Labels,
    output => OutBinary}.


%% @doc Labels are pairs of {Label, Bit Offset} followed by an encoded NIL []
encode_labels(Labels) ->
  LabelsBin = [[uasm_encode_int:varlength_unsigned(F),
                uasm_encode_int:varlength_unsigned(Offset)]
               || {F, Offset} <- Labels],
  [LabelsBin, uasm_encode_int:encode([], auto_bits)].


%% @doc Given a possibly nested list of bit strings create a single binary
merge_bits_into_binary(Out1) ->
  %% Given
  %% [<<Val1:Bits1>>, <<Val2:Bits2>>, {label, 1}, <<Val3:Bits3>>...]
  %% Ignore special tuple parts, take only bit codes
  << <<Piece/bitstring>> || Piece <- lists:flatten(Out1), is_bitstring(Piece)>>.


encode(Text) when is_list(Text) ->
  Tree = tree(freq_table(Text)),
  Dict = dict:from_list(codewords(Tree)),
  Code = << <<(dict:fetch(Char, Dict))/bitstring>> || Char <- Text>>,

  EncodedTree0 = encode_tree(Tree, 8),
  EncodedTree = merge_bits_into_binary(EncodedTree0),

  #{'$' => huffman,
    code => Code,
    tree => Tree,
    tree_encoded => EncodedTree,
    dict => Dict}.


decode(Code, Tree) ->
  decode(Code, Tree, Tree, []).


%%main(Input) ->
%%  {Code, Tree, Dict} = encode(Input),
%%  [begin
%%     io:format("~s: ", [[Key]]),
%%     print_bits(Value)
%%   end || {Key, Value} <- lists:sort(dict:to_list(Dict))],
%%  io:format("encoded: "),
%%  print_bits(Code),
%%  io:format("decoded: "),
%%  io:format("~s\n", [decode(Code, Tree)]).


decode(<<>>, _, _, Result) ->
  lists:reverse(Result);

decode(<<0:1, Rest/bits>>, Tree, {L = {_, _}, _R}, Result) ->
  decode(<<Rest/bits>>, Tree, L, Result);

decode(<<0:1, Rest/bits>>, Tree, {L, _R}, Result) ->
  decode(<<Rest/bits>>, Tree, Tree, [L | Result]);

decode(<<1:1, Rest/bits>>, Tree, {_L, R = {_, _}}, Result) ->
  decode(<<Rest/bits>>, Tree, R, Result);

decode(<<1:1, Rest/bits>>, Tree, {_L, R}, Result) ->
  decode(<<Rest/bits>>, Tree, Tree, [R | Result]).


codewords({L, R}) ->
  codewords(L, <<0:1>>) ++ codewords(R, <<1:1>>).


codewords({L, R}, <<Bits/bits>>) ->
  codewords(L, <<Bits/bits, 0:1>>) ++ codewords(R, <<Bits/bits, 1:1>>);

codewords(Symbol, <<Bits/bitstring>>) ->
  [{Symbol, Bits}].


tree([{N, _} | []]) ->
  N;

tree(Ns) ->
  [{N1, C1}, {N2, C2} | Rest] = lists:keysort(2, Ns),
  tree([{{N1, N2}, C1 + C2} | Rest]).


freq_table(Text) ->
  freq_table(lists:sort(Text), []).


freq_table([], Acc) ->
  Acc;

freq_table([S | Rest], Acc) ->
  {Block, MoreBlocks} = lists:splitwith(fun(X) -> X == S end, Rest),
  freq_table(MoreBlocks, [{S, 1 + length(Block)} | Acc]).


%%print_bits(<<>>) ->
%%  io:format("\n");
%%
%%print_bits(<<Bit:1, Rest/bitstring>>) ->
%%  io:format("~w", [Bit]),
%%  print_bits(Rest).
