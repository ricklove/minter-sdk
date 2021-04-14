
export const Fa2SwapCode: { __type: 'Fa2SwapCode', protocol: string, code: object[] } = {
    __type: 'Fa2SwapCode',
    protocol: 'PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA',
    code: JSON.parse(`[{"prim":"parameter","args":[{"prim":"or","args":[{"prim":"or","args":[{"prim":"nat","annots":["%accept"]},{"prim":"nat","annots":["%cancel"]}]},{"prim":"pair","annots":["%start"],"args":[{"prim":"list","annots":["%assets_offered"],"args":[{"prim":"pair","args":[{"prim":"address","annots":["%fa2_address"]},{"prim":"list","annots":["%tokens"],"args":[{"prim":"pair","args":[{"prim":"nat","annots":["%token_id"]},{"prim":"nat","annots":["%amount"]}]}]}]}]},{"prim":"list","annots":["%assets_requested"],"args":[{"prim":"pair","args":[{"prim":"address","annots":["%fa2_address"]},{"prim":"list","annots":["%tokens"],"args":[{"prim":"pair","args":[{"prim":"nat","annots":["%token_id"]},{"prim":"nat","annots":["%amount"]}]}]}]}]}]}]}]},{"prim":"storage","args":[{"prim":"pair","args":[{"prim":"nat","annots":["%next_swap_id"]},{"prim":"big_map","annots":["%swaps"],"args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"pair","annots":["%swap_offer"],"args":[{"prim":"list","annots":["%assets_offered"],"args":[{"prim":"pair","args":[{"prim":"address","annots":["%fa2_address"]},{"prim":"list","annots":["%tokens"],"args":[{"prim":"pair","args":[{"prim":"nat","annots":["%token_id"]},{"prim":"nat","annots":["%amount"]}]}]}]}]},{"prim":"list","annots":["%assets_requested"],"args":[{"prim":"pair","args":[{"prim":"address","annots":["%fa2_address"]},{"prim":"list","annots":["%tokens"],"args":[{"prim":"pair","args":[{"prim":"nat","annots":["%token_id"]},{"prim":"nat","annots":["%amount"]}]}]}]}]}]},{"prim":"address","annots":["%seller"]}]}]}]}]},{"prim":"code","args":[[{"prim":"PUSH","args":[{"prim":"mutez"},{"int":"0"}]},{"prim":"AMOUNT"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[],[{"prim":"PUSH","args":[{"prim":"string"},{"string":"XTZ_TRANSFER"}]},{"prim":"FAILWITH"}]]},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"address"}]},{"prim":"string"}]},{"prim":"lambda","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]},{"prim":"operation"}]},[{"prim":"UNPAIR"},{"prim":"UNPAIR"},{"prim":"PAIR","args":[{"int":"3"}]},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"address"},{"prim":"string"}]}]},{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]},{"prim":"operation"},[{"prim":"UNPAIR"},{"prim":"UNPAIR","args":[{"int":"3"}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CONTRACT","annots":["%transfer"],"args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address","annots":["%from_"]},{"prim":"list","annots":["%txs"],"args":[{"prim":"pair","args":[{"prim":"address","annots":["%to_"]},{"prim":"pair","args":[{"prim":"nat","annots":["%token_id"]},{"prim":"nat","annots":["%amount"]}]}]}]}]}]}]},{"prim":"IF_NONE","args":[[{"prim":"FAILWITH"}],[{"prim":"SWAP"},{"prim":"DROP"}]]},{"prim":"SWAP"},{"prim":"CDR"},{"prim":"MAP","args":[[{"prim":"DUP","args":[{"int":"4"}]},{"prim":"PAIR"}]]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PUSH","args":[{"prim":"mutez"},{"int":"0"}]},{"prim":"NIL","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CONS"},{"prim":"TRANSFER_TOKENS"}]]},{"prim":"SWAP"},{"prim":"APPLY"}]]},{"prim":"SWAP"},{"prim":"UNPAIR"},{"prim":"IF_LEFT","args":[[{"prim":"IF_LEFT","args":[[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"SWAP"},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"GET"},{"prim":"IF_NONE","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"SWAP_NOT_EXIST"}]},{"prim":"FAILWITH"}],[]]},{"prim":"DUP"},{"prim":"CAR"},{"prim":"CAR"},{"prim":"MAP","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"SWAP_OFFERED_FA2_INVALID"}]},{"prim":"SENDER"},{"prim":"SELF_ADDRESS"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"6"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"SWAP"},{"prim":"EXEC"}]]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CDR"},{"prim":"MAP","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"SWAP_REQUESTED_FA2_INVALID"}]},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"SENDER"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"7"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"SWAP"},{"prim":"EXEC"}]]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"ITER","args":[[{"prim":"CONS"}]]},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"NONE","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]}]},{"prim":"address"}]}]},{"prim":"SWAP"},{"prim":"UPDATE"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"}],[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"SWAP"},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"GET"},{"prim":"IF_NONE","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"SWAP_NOT_EXIST"}]},{"prim":"FAILWITH"}],[]]},{"prim":"SENDER"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[],[{"prim":"PUSH","args":[{"prim":"string"},{"string":"NOT_SWAP_SELLER"}]},{"prim":"FAILWITH"}]]},{"prim":"DUP"},{"prim":"CAR"},{"prim":"CAR"},{"prim":"MAP","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"SWAP_OFFERED_FA2_INVALID"}]},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"SELF_ADDRESS"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"6"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"SWAP"},{"prim":"EXEC"}]]},{"prim":"SWAP"},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"NONE","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"nat"},{"prim":"nat"}]}]}]}]}]},{"prim":"address"}]}]},{"prim":"SWAP"},{"prim":"UPDATE"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"}]]}],[{"prim":"SENDER"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"1"}]},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CAR"},{"prim":"ADD"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"5"}]},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"SOME"},{"prim":"SWAP"},{"prim":"UPDATE"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"MAP","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"SWAP_OFFERED_FA2_INVALID"}]},{"prim":"SELF_ADDRESS"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"SWAP"},{"prim":"EXEC"}]]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"PAIR"}]]}]]}]`)
};
