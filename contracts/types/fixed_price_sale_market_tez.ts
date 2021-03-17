
import { MichelsonMap } from '@taquito/taquito';
import { BigNumber } from 'bignumber.js';

type address = string & { __type: 'address' };
type BigMap<K, V> = Omit<MichelsonMap<K, V>, 'get'> & { get: (key: K) => Promise<V> };
type mutez = BigNumber & { __type: 'mutez' };
type nat = BigNumber & { __type: 'nat' };

type Storage = {
    admin?: {
        admin: address;
        paused: boolean;
        pending_admin?: address;
    };
    sales: BigMap<{
        sale_seller: address;
        sale_token: {
            token_for_sale_address: address;
            token_for_sale_token_id: nat;
        };
    }, mutez>;
};

type Methods = {
    confirm_admin: () => Promise<void>;
    pause: (param: boolean) => Promise<void>;
    set_admin: (param: address) => Promise<void>;
    buy: (params: {
        sale_seller: address;
        sale_token: {
            token_for_sale_address: address;
            token_for_sale_token_id: nat;
        };
    }) => Promise<void>;
    cancel: (params: {
        sale_seller: address;
        sale_token: {
            token_for_sale_address: address;
            token_for_sale_token_id: nat;
        };
    }) => Promise<void>;
    sell: (params: {
        sale_price: mutez;
        sale_token_param_tez: {
            token_for_sale_address: address;
            token_for_sale_token_id: nat;
        };
    }) => Promise<void>;
};

export type Contract = { methods: Methods, storage: Storage };