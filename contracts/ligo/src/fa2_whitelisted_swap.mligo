(* Swaps contract with limited set of allowed FA2 contracts.

Specification: https://hackmd.io/Z3YE5l0hSL-moDZdAsgg7w.

*)

#import "../fa2/fa2_interface.mligo" "FA2"
#include "../fa2_modules/simple_admin_option.mligo"
#include "fa2_swap.mligo"

(* ======== Swaps whitelist component ======== *)

(* ==== Types ==== *)

type whitelist = (address, unit) big_map

let addr_list_to_big_map(new_allowed: address list) : whitelist =
  List.fold
    (fun (m, a : (address, unit) big_map * address) -> Big_map.add a () m)
    new_allowed
    (Big_map.empty : (address, unit) big_map)

let swap_check_whitelist(whitelist, swap_param : whitelist * swap_entrypoints) : unit =
  match swap_param with
    Start swap_offer ->
      [@inline]
      let check_asset(asset : fa2_asset) : unit =
        match Big_map.find_opt asset.fa2_address whitelist with
        | Some u -> ()
        | None -> failwith("SWAP_FA2_NOT_WHITELISTED")
        in
      let u = List.map check_asset swap_offer.assets_offered in
      let u = List.map check_asset swap_offer.assets_requested in
      ()
      // TODO: distinguish offered vs requested in error
  | Accept x -> unit
  | Cancel x -> unit

(* ======== Swaps whitelisted contract ======== *)

type storage =
  { swap : swap_storage
  ; admin : simple_admin_storage
  ; whitelist : whitelist
  }

type entrypoints =
  | Swap of swap_entrypoints
  | Admin of simple_admin
  | Update_allowed of address list

let whitelisted_swaps_main(param, storage : entrypoints * storage)
    : ((operation list) * storage) =
  let swap_storage = storage.swap in
  let whitelist = storage.whitelist in

  match param with
    Swap swap_param ->
      let u = swap_check_whitelist(whitelist, swap_param) in
      let (ops, swap_storage) = swaps_main(swap_param, swap_storage) in
      ops, { storage with swap = swap_storage }
  | Admin admin_param ->
      let (ops, admin_storage) = simple_admin(admin_param, storage.admin) in
      ops, { storage with admin = admin_storage }
  | Update_allowed new_allowed ->
      let u = fail_if_not_admin_simple(storage.admin) in
      let whitelist = addr_list_to_big_map(new_allowed) in
      ([] : operation list), { storage with whitelist = whitelist }