{module, z}.  %% version = 0

{exports, [{f,0},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 11}.


{function, bin, 0, 2}.
  {label,1}.
    {line,[{location,"simple.erl",3}]}.
    {func_info,{atom,z},{atom,bin},0}.
  {label,2}.
    {move,{literal,<<"Some binary value">>},{x,0}}.
    return.


{function, map, 0, 4}.
  {label,3}.
    {line,[{location,"simple.erl",5}]}.
    {func_info,{atom,z},{atom,map},0}.
  {label,4}.
    {move,{literal,#{a => 3,b => "Hello"}},{x,0}}.
    return.


{function, f, 0, 6}.
  {label,5}.
    {line,[{location,"simple.erl",8}]}.
    {func_info,{atom,z},{atom,f},0}.
  {label,6}.
    {allocate_zero,1,0}.
    {line,[{location,"simple.erl",9}]}.
    {call,0,{f,2}}.
    {move,{x,0},{y,0}}.
    {line,[{location,"simple.erl",9}]}.
    {call,0,{f,4}}.
    {test_heap,3,1}.
    {put_tuple,2,{x,1}}.
    {put,{y,0}}.
    {put,{x,0}}.
    {move,{x,1},{x,0}}.
    {deallocate,1}.
    return.


{function, module_info, 0, 8}.
  {label,7}.
    {line,[]}.
    {func_info,{atom,z},{atom,module_info},0}.
  {label,8}.
    {move,{atom,z},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 10}.
  {label,9}.
    {line,[]}.
    {func_info,{atom,z},{atom,module_info},1}.
  {label,10}.
    {move,{x,0},{x,1}}.
    {move,{atom,z},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
