import { BigNumber } from 'bignumber.js';

type address = string & { __type: 'address' };
type timestamp = string & { __type: 'timestamp' };

type nat = BigNumber & { __type: 'nat' };
type mutez = BigNumber & { __type: 'mutez' };
type tez = BigNumber & { __type: 'tez' };
type int = BigNumber & { __type: 'int' };

type Storage = {
    pauseable_admin?: {
        admin: address;
        paused: boolean;
        pending_admin?: address;
    };
    current_id: nat;
    max_auction_time: nat;
    max_config_to_start_time: nat;
    auctions: Map<nat, {
        seller: address;
        current_bid: mutez;
        start_time: timestamp;
        last_bid_time: timestamp;
        round_time: int;
        extend_time: int;
        asset: {
            fa2_address: address;
            fa2_batch: {
                token_id: nat;
                amount: nat;
            }[];
        }[];
        min_raise_percent: nat;
        min_raise: mutez;
        end_time: timestamp;
        highest_bidder: address;
    }>;
};

type Methods = {
    confirm_admin: () => Promise<void>;
    pause: (param: boolean) => Promise<void>;
    set_admin: (param: address) => Promise<void>;
    bid: (param: nat) => Promise<void>;
    cancel: (param: nat) => Promise<void>;
    configure: (params: {
        opening_price: mutez;
        min_raise_percent: nat;
        min_raise: mutez;
        round_time: nat;
        extend_time: nat;
        asset: {
            fa2_address: address;
            fa2_batch: {
                token_id: nat;
                amount: nat;
            }[];
        }[];
        start_time: timestamp;
        end_time: timestamp;
    }) => Promise<void>;
    resolve: (param: nat) => Promise<void>;
};

export type TestContractType = { methods: Methods, storage: Storage };
