export type Fa2MultiFtAssetCodeType = { __type: 'Fa2MultiFtAssetCodeType'; code: object[]; };
export default { __type: 'Fa2MultiFtAssetCodeType', code: JSON.parse(`[{"prim":"parameter","args":[{"prim":"or","args":[{"prim":"or","args":[{"prim":"or","args":[{"prim":"or","args":[{"prim":"unit","annots":["%confirm_admin"]},{"prim":"bool","annots":["%pause"]}]},{"prim":"address","annots":["%set_admin"]}],"annots":["%admin"]},{"prim":"or","args":[{"prim":"or","args":[{"prim":"pair","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address","annots":["%owner"]},{"prim":"nat","annots":["%token_id"]}]}],"annots":["%requests"]},{"prim":"contract","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address","annots":["%owner"]},{"prim":"nat","annots":["%token_id"]}],"annots":["%request"]},{"prim":"nat","annots":["%balance"]}]}]}],"annots":["%callback"]}],"annots":["%balance_of"]},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address","annots":["%from_"]},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address","annots":["%to_"]},{"prim":"pair","args":[{"prim":"nat","annots":["%token_id"]},{"prim":"nat","annots":["%amount"]}]}]}],"annots":["%txs"]}]}],"annots":["%transfer"]}]},{"prim":"list","args":[{"prim":"or","args":[{"prim":"pair","args":[{"prim":"address","annots":["%owner"]},{"prim":"pair","args":[{"prim":"address","annots":["%operator"]},{"prim":"nat","annots":["%token_id"]}]}],"annots":["%add_operator"]},{"prim":"pair","args":[{"prim":"address","annots":["%owner"]},{"prim":"pair","args":[{"prim":"address","annots":["%operator"]},{"prim":"nat","annots":["%token_id"]}]}],"annots":["%remove_operator"]}]}],"annots":["%update_operators"]}],"annots":["%assets"]}]},{"prim":"or","args":[{"prim":"or","args":[{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address","annots":["%owner"]},{"prim":"pair","args":[{"prim":"nat","annots":["%token_id"]},{"prim":"nat","annots":["%amount"]}]}]}],"annots":["%burn_tokens"]},{"prim":"pair","args":[{"prim":"nat","annots":["%token_id"]},{"prim":"map","args":[{"prim":"string"},{"prim":"bytes"}],"annots":["%token_info"]}],"annots":["%create_token"]}]},{"prim":"list","args":[{"prim":"pair","args":[{"prim":"address","annots":["%owner"]},{"prim":"pair","args":[{"prim":"nat","annots":["%token_id"]},{"prim":"nat","annots":["%amount"]}]}]}],"annots":["%mint_tokens"]}],"annots":["%tokens"]}]}]},{"prim":"storage","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"option","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address","annots":["%admin"]},{"prim":"bool","annots":["%paused"]}]},{"prim":"option","args":[{"prim":"address"}],"annots":["%pending_admin"]}]}],"annots":["%admin"]},{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"big_map","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]},{"prim":"nat"}],"annots":["%ledger"]},{"prim":"big_map","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]}]},{"prim":"unit"}],"annots":["%operators"]}]},{"prim":"pair","args":[{"prim":"big_map","args":[{"prim":"nat"},{"prim":"pair","args":[{"prim":"nat","annots":["%token_id"]},{"prim":"map","args":[{"prim":"string"},{"prim":"bytes"}],"annots":["%token_info"]}]}],"annots":["%token_metadata"]},{"prim":"big_map","args":[{"prim":"nat"},{"prim":"nat"}],"annots":["%token_total_supply"]}]}],"annots":["%assets"]}]},{"prim":"big_map","args":[{"prim":"string"},{"prim":"bytes"}],"annots":["%metadata"]}]}]},{"prim":"code","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"FA2_TOKEN_UNDEFINED"}]},{"prim":"PUSH","args":[{"prim":"string"},{"string":"FA2_INSUFFICIENT_BALANCE"}]},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]},{"prim":"big_map","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]},{"prim":"nat"}]}]},{"prim":"nat"},[{"prim":"UNPAIR"},{"prim":"GET"},{"prim":"IF_NONE","args":[[{"prim":"PUSH","args":[{"prim":"nat"},{"int":"0"}]}],[]]}]]},{"prim":"DUP"},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"lambda","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]},{"prim":"big_map","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]},{"prim":"nat"}]}]},{"prim":"nat"}]},{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"big_map","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]},{"prim":"nat"}]}]}]}]},{"prim":"big_map","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]},{"prim":"nat"}]},[{"prim":"UNPAIR"},{"prim":"SWAP"},{"prim":"UNPAIR"},{"prim":"UNPAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"UNPAIR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"ADD"},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"0"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[{"prim":"DROP"},{"prim":"NONE","args":[{"prim":"nat"}]},{"prim":"SWAP"},{"prim":"UPDATE"}],[{"prim":"DIG","args":[{"int":"2"}]},{"prim":"SWAP"},{"prim":"SOME"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"UPDATE"}]]}]]},{"prim":"SWAP"},{"prim":"APPLY"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"PAIR"},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"lambda","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]},{"prim":"big_map","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]},{"prim":"nat"}]}]},{"prim":"nat"}]},{"prim":"string"}]},{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"big_map","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]},{"prim":"nat"}]}]}]}]},{"prim":"big_map","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]},{"prim":"nat"}]},[{"prim":"UNPAIR"},{"prim":"UNPAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"UNPAIR"},{"prim":"UNPAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"UNPAIR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"SWAP"},{"prim":"SUB"},{"prim":"ISNAT"},{"prim":"IF_NONE","args":[[{"prim":"DROP","args":[{"int":"2"}]},{"prim":"FAILWITH"}],[{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"0"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[{"prim":"DROP"},{"prim":"NONE","args":[{"prim":"nat"}]},{"prim":"SWAP"},{"prim":"UPDATE"}],[{"prim":"DIG","args":[{"int":"2"}]},{"prim":"SWAP"},{"prim":"SOME"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"UPDATE"}]]}]]}]]},{"prim":"SWAP"},{"prim":"APPLY"},{"prim":"LAMBDA","args":[{"prim":"option","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"bool"}]},{"prim":"option","args":[{"prim":"address"}]}]}]},{"prim":"unit"},[{"prim":"IF_NONE","args":[[{"prim":"UNIT"}],[{"prim":"CAR"},{"prim":"CAR"},{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"NEQ"},{"prim":"IF","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"NOT_AN_ADMIN"}]},{"prim":"FAILWITH"}],[{"prim":"UNIT"}]]}]]}]]},{"prim":"DIG","args":[{"int":"6"}]},{"prim":"UNPAIR"},{"prim":"IF_LEFT","args":[[{"prim":"DIG","args":[{"int":"6"}]},{"prim":"DROP"},{"prim":"IF_LEFT","args":[[{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"IF_LEFT","args":[[{"prim":"IF_LEFT","args":[[{"prim":"DROP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"IF_NONE","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"NO_ADMIN_CAPABILITIES_CONFIGURED"}]},{"prim":"FAILWITH"}],[{"prim":"DUP"},{"prim":"CDR"},{"prim":"IF_NONE","args":[[{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"NO_PENDING_ADMIN"}]},{"prim":"FAILWITH"}],[{"prim":"SENDER"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[{"prim":"NONE","args":[{"prim":"address"}]},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"CDR"},{"prim":"SENDER"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"SOME"}],[{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"NOT_A_PENDING_ADMIN"}]},{"prim":"FAILWITH"}]]}]]}]]},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}],[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"IF_NONE","args":[[{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"NO_ADMIN_CAPABILITIES_CONFIGURED"}]},{"prim":"FAILWITH"}],[{"prim":"DUP"},{"prim":"CDR"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"SOME"}]]},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]}],[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"IF_NONE","args":[[{"prim":"DROP"},{"prim":"PUSH","args":[{"prim":"string"},{"string":"NO_ADMIN_CAPABILITIES_CONFIGURED"}]},{"prim":"FAILWITH"}],[{"prim":"SWAP"},{"prim":"SOME"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SOME"}]]},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]},{"prim":"UNPAIR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"}],[{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"IF_NONE","args":[[{"prim":"UNIT"}],[{"prim":"CAR"},{"prim":"CDR"},{"prim":"IF","args":[[{"prim":"PUSH","args":[{"prim":"string"},{"string":"PAUSED"}]},{"prim":"FAILWITH"}],[{"prim":"UNIT"}]]}]]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"IF_LEFT","args":[[{"prim":"IF_LEFT","args":[[{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DUP"},{"prim":"CAR"},{"prim":"MAP","args":[[{"prim":"DUP","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"MEM"},{"prim":"NOT"},{"prim":"IF","args":[[{"prim":"DROP"},{"prim":"DUP","args":[{"int":"7"}]},{"prim":"FAILWITH"}],[{"prim":"DUP","args":[{"int":"3"}]},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"8"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"SWAP"},{"prim":"PAIR"}]]}]]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"CDR"},{"prim":"PUSH","args":[{"prim":"mutez"},{"int":"0"}]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"TRANSFER_TOKENS"},{"prim":"SWAP"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CONS"},{"prim":"PAIR"}],[{"prim":"DIG","args":[{"int":"5"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"LAMBDA","args":[{"prim":"pair","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"address"}]},{"prim":"pair","args":[{"prim":"nat"},{"prim":"big_map","args":[{"prim":"pair","args":[{"prim":"address"},{"prim":"pair","args":[{"prim":"address"},{"prim":"nat"}]}]},{"prim":"unit"}]}]}]},{"prim":"unit"},[{"prim":"UNPAIR"},{"prim":"UNPAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"UNPAIR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[{"prim":"DROP","args":[{"int":"4"}]},{"prim":"UNIT"}],[{"prim":"DIG","args":[{"int":"3"}]},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"MEM"},{"prim":"IF","args":[[{"prim":"UNIT"}],[{"prim":"PUSH","args":[{"prim":"string"},{"string":"FA2_NOT_OPERATOR"}]},{"prim":"FAILWITH"}]]}]]}]]},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"ITER","args":[[{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"ITER","args":[[{"prim":"SWAP"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"MEM"},{"prim":"NOT"},{"prim":"IF","args":[[{"prim":"DROP","args":[{"int":"2"}]},{"prim":"DUP","args":[{"int":"8"}]},{"prim":"FAILWITH"}],[{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CAR"},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SENDER"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"8"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"8"}]},{"prim":"SWAP"},{"prim":"EXEC"}]]}]]},{"prim":"SWAP"},{"prim":"DROP"}]]},{"prim":"SWAP"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]}],[{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"SENDER"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"ITER","args":[[{"prim":"SWAP"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"IF_LEFT","args":[[],[]]},{"prim":"CAR"},{"prim":"COMPARE"},{"prim":"EQ"},{"prim":"IF","args":[[],[{"prim":"PUSH","args":[{"prim":"string"},{"string":"FA2_NOT_OWNER"}]},{"prim":"FAILWITH"}]]},{"prim":"SWAP"},{"prim":"IF_LEFT","args":[[{"prim":"SWAP"},{"prim":"UNIT"},{"prim":"SOME"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"UPDATE"}],[{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"NONE","args":[{"prim":"unit"}]},{"prim":"SWAP"},{"prim":"UPDATE"}]]}]]},{"prim":"SWAP"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]},{"prim":"UNPAIR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"}]]}],[{"prim":"DIG","args":[{"int":"5"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"SWAP"},{"prim":"EXEC"},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"IF_LEFT","args":[[{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"IF_LEFT","args":[[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"ITER","args":[[{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"}]]},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"ITER","args":[[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"GET"},{"prim":"IF_NONE","args":[[{"prim":"DROP","args":[{"int":"2"}]},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"FAILWITH"}],[{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"SWAP"},{"prim":"SUB"},{"prim":"ISNAT"},{"prim":"IF_NONE","args":[[{"prim":"DUP","args":[{"int":"6"}]},{"prim":"FAILWITH"}],[]]},{"prim":"SOME"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"UPDATE"}]]}]]},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}],[{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DUP"},{"prim":"CAR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"GET"},{"prim":"IF_NONE","args":[[{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"SWAP"},{"prim":"SOME"},{"prim":"SWAP"},{"prim":"UPDATE"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"PUSH","args":[{"prim":"nat"},{"int":"0"}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"SWAP"},{"prim":"SOME"},{"prim":"SWAP"},{"prim":"UPDATE"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"PAIR"}],[{"prim":"DROP","args":[{"int":"4"}]},{"prim":"PUSH","args":[{"prim":"string"},{"string":"FA2_DUP_TOKEN_ID"}]},{"prim":"FAILWITH"}]]},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]}],[{"prim":"DIG","args":[{"int":"3"}]},{"prim":"DROP"},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"ITER","args":[[{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUP","args":[{"int":"5"}]},{"prim":"SWAP"},{"prim":"EXEC"}]]},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"ITER","args":[[{"prim":"SWAP"},{"prim":"DUP"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"GET"},{"prim":"IF_NONE","args":[[{"prim":"DROP","args":[{"int":"2"}]},{"prim":"DUP","args":[{"int":"4"}]},{"prim":"FAILWITH"}],[{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"CDR"},{"prim":"ADD"},{"prim":"SOME"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"UPDATE"}]]}]]},{"prim":"DIG","args":[{"int":"4"}]},{"prim":"DROP"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"DUP"},{"prim":"DUG","args":[{"int":"2"}]},{"prim":"CDR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"NIL","args":[{"prim":"operation"}]},{"prim":"PAIR"}]]},{"prim":"UNPAIR"},{"prim":"DUP","args":[{"int":"3"}]},{"prim":"CDR"},{"prim":"DIG","args":[{"int":"2"}]},{"prim":"DIG","args":[{"int":"3"}]},{"prim":"CAR"},{"prim":"CAR"},{"prim":"PAIR"},{"prim":"PAIR"},{"prim":"SWAP"},{"prim":"PAIR"}]]}]]}]`) } as Fa2MultiFtAssetCodeType;
