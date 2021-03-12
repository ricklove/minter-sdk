(* Swaps contract.
Specification: https://hackmd.io/iArxW3FyR_e85JNPYvuQhQ.

In case you are implementing a client for this contract, please see
"Notes to middleware implementors" section in the spec mentioned above.
*)

#import "../fa2/fa2_interface.mligo" "FA2"

(* ==== Types ==== *)

type swap_id = nat

type fa2_asset =
  [@layout:comb]
  { fa2_address : address
  ; token_id : nat
  ; amount : nat
  }

type swap_status =
  [@layout:comb]
  | Open
  | Finished of address  // keeps who has accepted the swap
  | Cancelled

type swap_offer =
  [@layout:comb]
  { assets_offered : fa2_asset list
  ; assets_requested : fa2_asset list
  }

type swap_info =
  [@layout:comb]
  { swap_offer : swap_offer
  ; seller : address
  ; status : swap_status
  }

type swap_entrypoints =
  | Start of swap_offer
  | Cancel of swap_id
  | Accept of swap_id

type swap_storage =
  { next_swap_id : swap_id
  ; swaps : (swap_id, swap_info) big_map
  }

let init_storage : swap_storage =
  { next_swap_id = 0n
  ; swaps = (Big_map.empty : (swap_id, swap_info) big_map)
  }

type return = operation list * swap_storage

(* ==== Helpers ==== *)

(* This is just a marker of that the given error should never happen
 * because of invariants of our contract.
 *)
[@inline]
let unexpected_err(err : string) : string = err

let forbid_xtz_transfer : unit =
  if Tezos.amount = 0
  then ()
  else failwith "XTZ_TRANSFER"

let fa2_transfer_entrypoint(fa2, on_invalid_fa2 : address * string)
    : ((FA2.transfer list) contract) =
  match (Tezos.get_entrypoint_opt "%transfer" fa2 : (FA2.transfer list) contract option) with
  | None -> (failwith on_invalid_fa2 : (FA2.transfer list) contract)
  | Some c ->  c

let transfer_asset(from_, to_, on_invalid_fa2 : address * address * string)(asset : fa2_asset)
    : operation =
  let transfer_ep = fa2_transfer_entrypoint(asset.fa2_address, on_invalid_fa2) in
  let tx_dest : FA2.transfer_destination =
    { to_ = to_
    ; token_id = asset.token_id
    ; amount = asset.amount
    } in
  let param = ([{ from_ = from_; txs = [tx_dest] }] : FA2.transfer list) in
  Tezos.transaction param 0mutez transfer_ep

[@inline]
let get_swap(id, storage : swap_id * swap_storage) : swap_info =
  match Big_map.find_opt id storage.swaps with
  | None -> (failwith "SWAP_NOT_EXIST" : swap_info)
  | Some s -> s

let authorize_seller(swap : swap_info) : unit =
  if swap.seller = Tezos.sender
  then ()
  else failwith "NOT_SWAP_SELLER"

let check_is_open(swap : swap_info) : unit =
  match swap.status with
  | Open -> ()
  | Finished -> failwith "SWAP_FINISHED"
  | Cancelled -> failwith "SWAP_CANCELLED"

(* ==== Entrypoints ==== *)

let start_swap(swap_offer, storage : swap_offer * swap_storage) : return =
  let swap_id = storage.next_swap_id in
  let seller = Tezos.sender in
  let swap : swap_info =
    { swap_offer = swap_offer
    ; seller = seller
    ; status = Open
    } in
  let storage = { storage with
      next_swap_id = storage.next_swap_id + 1n
    ; swaps = Big_map.add swap_id swap storage.swaps
    } in

  let ops =
        List.map
        (transfer_asset(seller, Tezos.self_address, "SWAP_OFFERED_FA2_INVALID"))
        swap_offer.assets_offered in

  ops, storage

let cancel_swap(swap_id, storage : swap_id * swap_storage) : return =
  let swap = get_swap(swap_id, storage) in
  let u = authorize_seller(swap) in
  let u = check_is_open(swap) in

  let new_swap = { swap with status = Cancelled } in
  let storage = { storage with swaps = Big_map.add swap_id new_swap storage.swaps } in

  let ops =
        List.map
        (transfer_asset(Tezos.self_address, swap.seller, unexpected_err "SWAP_OFFERED_FA2_INVALID"))
        swap.swap_offer.assets_offered in

  ops, storage

let accept_swap(swap_id, storage : swap_id * swap_storage) : return =
  let swap = get_swap(swap_id, storage) in
  let u = check_is_open(swap) in
  let buyer = Tezos.sender in

  let new_swap = { swap with status = Finished buyer } in
  let storage = { storage with swaps = Big_map.add swap_id new_swap storage.swaps } in

  let ops1 =
        List.map
        (transfer_asset(Tezos.self_address, buyer, unexpected_err "SWAP_OFFERED_FA2_INVALID"))
        swap.swap_offer.assets_offered in
  let ops2 =
        List.map
        (transfer_asset(buyer, swap.seller, "SWAP_REQUESTED_FA2_INVALID"))
        swap.swap_offer.assets_requested in
  let snoc_ops (l, a : operation list * operation) = a :: l in
  let allOps = List.fold snoc_ops ops1 ops2 in

  allOps, storage

let swaps_main (param, storage : swap_entrypoints * swap_storage) : return =
  let u = forbid_xtz_transfer
  // TODO: Check contract size
  match param with
    | Start swap_offer -> start_swap(swap_offer, storage)
    | Cancel swap_id -> cancel_swap(swap_id, storage)
    | Accept swap_id -> accept_swap(swap_id, storage)